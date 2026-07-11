package abap_frontend_ir

/*
ir is the executable intermediate representation between parsed and checked
ABAP source and later VM, native-code, LLVM, analysis, or debug consumers.

The package owns a single low-level SSA representation for both execution and
compiler optimization. Canonical `Opcode`, `Instruction_Attrs`, typed intrinsic
declarations, effect sets, memory metadata, and source/provenance records are
the authority for lowering, verification, printing, query/mutation helpers, and
VM preparation. The package no longer owns a bytecode carrier or a separate
register/offset executable prototype.

The package owns the lowering-ready shape of executable ABAP:

- control flow, block parameters, terminators, and dominance-sensitive value
  flow;
- slots for locals, parameters, globals, method instances, system fields, table
  handles, and temporaries;
- report entry functions and callable roles that make runtime dispatch explicit;
- effect ordering through explicit world-token reads and writes;
- module-local type ids and value/op/block/slot ids;
- core, ABAP-domain, table, SQL, call, message, and system-field operation
  records;
- explicit unsupported-operation records for source-bearing deferred behavior;
- deterministic printing, read-only walking/querying, verification, and
  mutation helpers.

AST still owns parsed syntax and token ranges. semantic still owns declaration
lookup, expression and SQL facts, OOP rules, dependency edges, and semantic
entity/type identity. Workspace, ADT, dependency-store, runtime, persistence, and
external I/O policy stay outside this package. IR consumers should reuse those
owning APIs and pass checked semantic snapshots into IR instead of creating a
parallel parser or loader.

Primary API flow:

1. Start from an existing semantic snapshot.

   Normal consumers should come through workspace or semantic analysis first.
   For a workspace command, read `Workspace_Analysis.project_results` and use
   each `project`/`checker` pair while that snapshot is alive. The CLI debug
   command `abap_frontend ir` follows this pattern: it uses existing workspace
   analysis, then lowers, verifies, and prints IR for each semantic project.

2. Lower checked ABAP into a `Module`.

	   Use `lower_project(project, checker, allocator)` for a full semantic
	   project/checker snapshot. It returns `Lower_Result`, whose `module` contains
	   one IR function for each lowered file body and nested executable callable.
	   Executable reports also record generated report-entry functions in
	   `Module.entries`; those entries dispatch load/global-initialization and the
	   lowered report event functions instead of relying on print order or a file
	   body no-op.
	   Use `lower_file(module, project, checker, file)` only when the caller already
	   owns a module and intentionally wants to append one checked file from the
	   same semantic snapshot.

   Lowering reuses semantic query APIs for files, declarations, references,
   expression facts, type facts, SQL facts, and call facts. It records stable IR
   metadata for canonical semantics and may also keep borrowed semantic and
   AST/source provenance on types, slots, functions, operations, and diagnostics
   where that provenance is available.

3. Verify before relying on IR.

   Use `verify_module(&result.module, allocator)` before handing IR to the VM,
   analysis, or other consumers that assume well-formed control, value, and
   effect flow. `Verify_Result` reports invalid ids, missing
   terminators, branch/return argument mismatches, dominance errors, world-chain
   breaks, invalid type references, malformed source-bearing unsupported ops,
   and operation signature mismatches.

4. Inspect or print through the package API.

   Use `print_module` for deterministic debug text and narrow regression
   snapshots. Use `walk_module`, `walk_function`, and `walk_block` for read-only
   structural scans over functions, blocks, operations, and terminators. Use the
   query helpers in `query.odin` for point lookups such as function/type/block
   records, operation locations, value definitions, slot records, and source
   locations. Consumers should prefer these helpers over open-coded array walks
   when the helper captures an IR-owned relationship.

5. Cross into execution through the VM IR boundary.

   Use `vm.prepare_module` or `vm.execute_module` for runtime execution of an
   IR module. Those VM APIs prepare a VM-owned execution cache directly from
   verified canonical `ir.Module` functions, blocks, instructions, terminators,
   and typed intrinsic payloads. VM execution, persistence, external I/O, and
   native host-library policy should not leak back into general IR construction
   or lowering. "Bytecode" is reserved for a future serialized encoding, not the
   canonical in-memory compiler IR.

Manual construction API:

Tests and future IR-producing passes can build modules directly:

- `module_make` and `module_destroy` own module storage.
- `module_add_type` interns IR-local type records.
- `builder_begin_function` starts a function with an entry block and world
  parameter.
- block helpers create blocks and block parameters.
- slot helpers create function-local storage records.
- `builder_emit_*` helpers append operations and keep the current world token
  coherent for effectful operations.
- terminator helpers close blocks with branches, conditional branches,
  multi-way switches, or returns.

Builder helpers assert impossible construction states, such as emitting after a
block has a terminator. Use `verify_module` for module data that may be produced
by lowering or future transformation passes and therefore needs diagnostics
instead of construction-time assertions.

Ownership and lifetime:

- `Module` owns its dynamic arrays and strings allocated through its allocator.
  Destroy it with `module_destroy`, or through `lower_result_destroy` for a
  `Lower_Result`.
- `Verify_Result` owns its diagnostics array and must be destroyed with
  `verify_result_destroy`.
- IR ids are module/function-local indexes. They are stable only while the
  owning module remains alive and its arrays are not externally mutated.
- `Source_Loc.file`, `Source_Loc.node`, `Function.entity`, `Slot.entity`,
  `Projection_Segment.entity`, and `Type.semantic_type` are borrowed pointers
  into the AST/semantic snapshot used to build the module. They are provenance,
  not persistent ids. Consumers that read them must keep the parsed/semantic
  snapshot alive.
- Verification, printing, and execution must not use those borrowed pointers as
  semantic authority. Use IR-owned names, type ids, projection names/indices,
  intrinsic payload strings, and metadata records when behavior needs stable
  identity outside lowering-time provenance.
- Source-bearing operations should preserve the closest AST node/range that
  explains the IR effect. Semantic-backed slots and types should point at the
  owning semantic entity/type when one exists.

Unsupported operations:

Unsupported ABAP syntax or semantic cases must be explicit `Core_Unsupported`
operations with `.Unsupported`, a concise message, and source provenance.
Effectful unsupported operations must still participate in world threading, may
  trap, and verify like other effectful IR. Do not silently drop unsupported
  behavior or guess runtime semantics. When an ABAP operation family is modeled as
  IR, prefer a named operation kind and payload over hiding it as unsupported even
if runtime support has not caught up yet.

Important invariants:

- Every function has a valid entry block and returns the function return types.
- Blocks end in exactly one terminator before consumers use them.
- Values dominate their uses; branch arguments match target block parameters.
- Switches have one default edge followed by typed case-value edges; every edge
  carries the arguments required by its target block.
- Entry block arguments mirror `Function_Signature.params`; return terminators
  mirror `Function_Signature.results`/`Function.return_types`.
- Blocks own block argument IDs and instruction ID order. Instruction records
  live in the function instruction arena; block terminators are ordinary
  canonical instruction records referenced by `Block.terminator`.
- Operation results are owned by their defining operation, and block parameters
  point back to their defining block.
- Effectful operations read the current world token first and write a new world
  token first when they mutate observable state.
- Semantic facts should be consumed from semantic-owned APIs or records. IR may
  match source ranges locally, but should not duplicate declaration, lookup,
  type, OOP, SQL, workspace, ADT, dependency-store, runtime, or persistence
  policy.
- VM preparation must preserve typed SSA values, block arguments, explicit
  effects, metadata, and typed intrinsics. ABAP/domain behavior that needs host
  state, I/O, SQL, internal-table semantics, system fields, or dynamic dispatch
  must be modeled as typed IR intrinsics before VM preparation admits it for
  execution.
- Recoverable exception control flow lowers to `Invoke` with explicit normal
  and exception successors. `verify_module` rejects non-`Invoke` `.May_Throw`
  operations by default. The only compatibility policy is the explicitly named
  legacy top-level propagation option, for transitional callers that still need
  to surface an unhandled exception as a VM-level trap.
*/
