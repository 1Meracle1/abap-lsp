# Low-Level SSA IR Migration Context and Plan

This document describes the target design for `src/ir`, the migration context
from the current transitional code, and the expected package/API layout for the
next implementation pass.

The package name remains `abap_frontend_ir`. The target representation is an
executable, optimizable, low-level SSA intermediate representation comparable in
spirit to LLVM IR, with MLIR-style block arguments instead of explicit phi
instructions. It is not bytecode. It should be suitable for direct VM execution
now and later lowering to LLVM or native code.

This is a design document. Code blocks are proposed Odin API layouts, not a
description of the current implementation.

## Design Inputs

The design is based on:

- LLVM IR module/function/basic-block/value/instruction structure.
- LLVM's `Value`/`User`/`Use` def-use and mutation model.
- MLIR block arguments as phi-equivalent SSA joins.
- GCC GIMPLE's preference for lowered, statement-like operations.
- The current repository state, especially:
  - `tmp/ir-low-level-ssa-plan.md`
  - `src/ir/*.odin`
  - `src/ir/bytecode/*.odin`
  - `src/vm/*.odin`
  - `src/vm/runtime/*.odin`
  - `src/ir/lower.odin`

## Non-Goals

- Do not preserve the current `src/ir/bytecode` representation as the target
  architecture.
- Do not preserve `Executable_Module` merely because it already exists.
- Do not make `ir` import `vm`.
- Do not keep runtime callback payloads as the execution interface.
- Do not add broad compatibility layers that live forever.
- Do not turn ABAP semantic operations into a large permanent set of core IR
  opcodes.

## Current-State Audit

### Useful Existing Pieces

`src/ir/ir.odin` already has several concepts worth keeping:

- `Module`, `Function`, `Basic_Block`, and SSA-like `Value` records.
- Block arguments.
- Source records on operations and blocks.
- Stable numeric handles for functions, blocks, operations, values, types, and
  slots.
- Explicit branch argument lists.

`src/ir/builder.odin` has a useful discipline:

- effectful operations thread a `world` value;
- the builder owns the current block and current effect token;
- branch and return helpers already preserve block argument flow.

`src/ir/verify.odin` has important verifier habits:

- structural validation before deeper checks;
- dominance checking;
- same-block use ordering;
- terminator validation;
- branch argument arity/type checking;
- effect/world-chain validation;
- operation-specific signature checks.

`src/ir/print.odin`, `src/ir/query.odin`, and `src/ir/walk.odin` provide the
right kind of package surface:

- deterministic printable IR;
- safe query helpers;
- visitor-style traversal;
- source/debug display independent of internal IDs.

`src/ir/lower.odin` is the best local catalog of ABAP execution requirements:

- reports, events, forms, methods, function calls, and local routines;
- statements and expressions;
- assignments, selectors, field paths, field symbols, and data refs;
- control flow;
- table operations;
- SQL operations;
- system fields;
- strings;
- exceptions;
- unsupported source-preserving fallbacks.

`src/vm/runtime` is the correct direction for runtime behavior:

- runtime `Value` representation;
- mutable cells and references;
- table storage and operations;
- system field state;
- IO capture;
- traps and exception state.

### Transitional Pieces That Should Not Survive

`src/ir/bytecode` is migration scaffolding. It is a flat executable encoding
with registers, block offsets, runtime callbacks, and callback payloads derived
from high-level IR operations. It is useful for current execution but should not
define the long-term compiler interface.

`src/ir/executable*.odin` is a useful prototype but not the final shape:

- it is still register-oriented;
- blocks are offsets into a flat instruction stream;
- operands/results/edges are side tables around flat instructions;
- `dst`, `src0`, and `src1` mirror a bytecode style;
- mutation replaces deleted instructions with `Nop`;
- def-use queries scan instead of maintaining use lists;
- `legacy_source` exists to bridge old structures.

`src/vm/executable_adapter.odin` converts the prototype IR back to bytecode.
That is a compatibility adapter, not a target architecture.

The old high-level `Op_Kind` taxonomy should not survive as the core operation
vocabulary. Many of those operations are ABAP semantic intrinsics, not
low-level compiler instructions.

### Confused Boundaries

Current package boundaries are mixed:

- `ir` contains high-level ABAP semantic operation payloads and execution-ish
  data.
- `ir/bytecode` owns executable layout even though long-term execution should
  read canonical IR or a VM-private prepared form.
- `vm` imports both `ir` and `ir/bytecode`.
- VM runtime callbacks depend on old `ir.Op_Kind` and `ir.Op_Payload`.
- The old `src/interpreter_runtime` direction should disappear; runtime
  behavior belongs under `src/vm` or a VM-owned runtime layer.

Long-term:

- `ir` owns the compiler IR, verification, printing, analysis, mutation, and
  lowering surface.
- `vm` owns execution, frame layout, runtime values, runtime cells, intrinsic
  implementations, traps, IO, and host integration.
- `vm` may import `ir`.
- `ir` must not import `vm`.

## Target Package Layout

### `src/ir`

Expected files after the migration:

- `doc.odin`: package documentation and public architecture notes.
- `ids.odin`: opaque handle types and invalid constants.
- `module.odin`: `Module`, global tables, entry records, construction and
  destruction.
- `type.odin`: IR type arena, type interning, ABAP layout type records.
- `constant.odin`: constant arena and initializer records.
- `metadata.odin`: source, debug, semantic, and trace metadata.
- `effect.odin`: effect sets, memory access, alias scopes, effect verification.
- `intrinsic.odin`: intrinsic declarations, signatures, payload schemas,
  builtin intrinsic catalog.
- `value.odin`: SSA value records and use-list storage.
- `instruction.odin`: instruction records, opcode definitions, instruction
  attributes.
- `block.odin`: block records and successor edge records.
- `function.odin`: function records, signatures, linkage, role, local arenas.
- `builder.odin`: structured construction APIs.
- `query.odin`: safe accessors, definition lookup, operand/result views.
- `mutate.odin`: use-list-preserving mutation APIs.
- `analysis_cfg.odin`: CFG, predecessor/successor, RPO.
- `analysis_dominance.odin`: dominance and dominance frontier.
- `verify.odin`: verifier entry points and verifier diagnostics.
- `print.odin`: deterministic textual IR printer.
- `walk.odin`: traversal helpers.
- `lower.odin`: semantic ABAP to canonical IR lowering.
- `pass_dce.odin`: initial dead-code deletion pass.
- `pass_fold.odin`: initial constant/canonical folding pass.
- `*_test.odin`: focused tests next to the relevant implementation.

Expected deleted or fully replaced files:

- `src/ir/bytecode/*.odin`
- `src/ir/executable.odin`
- `src/ir/executable_query.odin`
- `src/ir/executable_lower.odin`
- `src/ir/executable_pass.odin`
- associated executable/bytecode tests after equivalent canonical IR tests
  exist.

### `src/vm`

Expected files after the migration:

- `api.odin`: run options/results and convenience execution entry points.
- `prepare_types.odin`: owned prepared module, function, and instruction data.
- `prepare.odin` and `prepare_lowering.odin`: verification and preparation of
  `ir.Module` into the VM-owned executable representation.
- `machine.odin`: VM lifecycle, scratch state, and instruction stepping.
- `frame.odin`: stack frames, registers, calls, branches, and returns.
- `scalar_ops.odin` and `memory_ops.odin`: scalar instruction adapters and
  address/load/store operations.
- `intrinsic_dispatch.odin`: intrinsic dispatch protocol and built-in routing.
- `intrinsic_abap.odin`: ABAP scalar/string/move/convert intrinsics.
- `intrinsic_table.odin`: table intrinsic execution.
- `diagnostics.odin`: VM traps, source conversion, and stack traces.
- `runtime/*.odin`: runtime values, context, references, fields, tables, IO,
  traps, exceptions.
- `vm_test.odin`: execution tests against canonical IR.

Expected deleted files:

- `src/vm/executable_adapter.odin`
- any VM entry point that accepts `src/ir/bytecode.Module`.

### Other Packages

`src/semantic` remains responsible for semantic facts, symbol resolution, and
type facts. It should not gain VM knowledge.

`src/parser` and `src/ast` remain source/AST packages. They should not import
`ir` just to support execution.

`cmd/abap_frontend` may print or verify IR, but should depend on public `ir`
APIs rather than bytecode internals.

`cmd/abap_interpreter` should call VM execution over `ir.Module`.

## Core API Layout

The sketches below are intended to show ownership, identity, and package
surface. Field names can be adjusted during implementation, but the shape should
remain stable.

### IDs

```odin
package abap_frontend_ir

Module_Id :: distinct u32
Function_Id :: distinct u32
Block_Id :: distinct u32
Instruction_Id :: distinct u32
Value_Id :: distinct u32
Type_Id :: distinct u32
Constant_Id :: distinct u32
Global_Id :: distinct u32
Intrinsic_Id :: distinct u32
Metadata_Id :: distinct u32
Effect_Scope_Id :: distinct u32
Alias_Class_Id :: distinct u32
Use_Id :: distinct u32

INVALID_FUNCTION :: Function_Id(0xffffffff)
INVALID_BLOCK :: Block_Id(0xffffffff)
INVALID_INSTRUCTION :: Instruction_Id(0xffffffff)
INVALID_VALUE :: Value_Id(0xffffffff)
INVALID_TYPE :: Type_Id(0xffffffff)
INVALID_CONSTANT :: Constant_Id(0xffffffff)
INVALID_GLOBAL :: Global_Id(0xffffffff)
INVALID_INTRINSIC :: Intrinsic_Id(0xffffffff)
INVALID_METADATA :: Metadata_Id(0xffffffff)
INVALID_EFFECT_SCOPE :: Effect_Scope_Id(0xffffffff)
INVALID_ALIAS_CLASS :: Alias_Class_Id(0xffffffff)
INVALID_USE :: Use_Id(0xffffffff)
```

IDs are opaque handles. They are not debug names and should not be printed as
semantic identity. The printer should assign stable textual names such as `%0`
or use debug names when available.

### Module

```odin
Module :: struct {
    allocator: mem.Allocator,

    source_name: string,
    target: Target_Info,

    types: [dynamic]Type,
    type_intern: Type_Intern_Table,

    constants: [dynamic]Constant,
    globals: [dynamic]Global,
    intrinsics: [dynamic]Intrinsic_Decl,
    metadata: [dynamic]Metadata_Record,
    effect_scopes: [dynamic]Effect_Scope,
    alias_classes: [dynamic]Alias_Class,

    functions: [dynamic]Function,
    entries: [dynamic]Entry_Record,

    symbol_table: Symbol_Table,
}

Target_Info :: struct {
    pointer_bits: u32,
    default_integer_bits: u32,
    string_encoding: String_Encoding,
}

Entry_Record :: struct {
    name: string,
    function: Function_Id,
    role: Function_Role,
    source: Metadata_Id,
}

Function_Role :: enum {
    Internal,
    Report_Start,
    Report_Event,
    Form,
    Method,
    Function_Module,
    Constructor,
    Class_Constructor,
    Test_Entry,
}
```

The module owns all strings and arrays needed by IR execution and optimization.
Borrowed semantic or AST pointers must not be needed for verification or VM
execution.

### Function

```odin
Function :: struct {
    id: Function_Id,
    name: string,
    linkage: Linkage,
    role: Function_Role,
    signature: Function_Signature,

    entry: Block_Id,
    blocks: [dynamic]Basic_Block,
    block_order: [dynamic]Block_Id,

    values: [dynamic]Value,
    instructions: [dynamic]Instruction,
    uses: [dynamic]Use,

    debug: Metadata_Id,
    source: Metadata_Id,
    semantic: Metadata_Id,

    analysis_generation: u64,
    mutation_generation: u64,
}

Linkage :: enum {
    Private,
    Internal,
    Exported,
    External,
}

Function_Signature :: struct {
    params: []Type_Id,
    results: []Type_Id,
    calling_convention: Calling_Convention,
    effects: Effect_Set,
    can_throw: bool,
    can_trap: bool,
}

Calling_Convention :: enum {
    IR,
    ABAP_Report,
    ABAP_Form,
    ABAP_Method,
    ABAP_Function,
    Host_ABI,
}
```

Function arguments are represented as block arguments on the entry block. There
should not be a separate `Argument` value kind unless implementation details
require it.

### Basic Blocks and Edges

```odin
Basic_Block :: struct {
    id: Block_Id,
    name: string,

    args: [dynamic]Value_Id,
    instructions: [dynamic]Instruction_Id,
    terminator: Instruction_Id,

    source: Metadata_Id,
    debug: Metadata_Id,
}

Successor_Edge :: struct {
    target: Block_Id,
    args: []Value_Id,
    kind: Edge_Kind,
    source: Metadata_Id,
}

Edge_Kind :: enum {
    Normal,
    True,
    False,
    Switch_Case,
    Exception,
    Cleanup,
}
```

Block arguments are phi-equivalent values. A branch to a block must provide one
edge argument per target block argument, with exactly matching types.

### Values, Users, and Uses

```odin
Value :: struct {
    id: Value_Id,
    type: Type_Id,
    def: Value_Def,

    first_use: Use_Id,
    use_count: u32,

    debug_name: string,
    debug: Metadata_Id,
}

Value_Def :: union #no_nil {
    Block_Arg: Block_Arg_Def,
    Instruction_Result: Instruction_Result_Def,
    Constant: Constant_Id,
    Global: Global_Id,
    Function: Function_Id,
}

Block_Arg_Def :: struct {
    block: Block_Id,
    index: u32,
}

Instruction_Result_Def :: struct {
    instruction: Instruction_Id,
    index: u32,
}

Use :: struct {
    id: Use_Id,
    value: Value_Id,
    user: Instruction_Id,
    operand_index: u32,
    prev_for_value: Use_Id,
    next_for_value: Use_Id,
}
```

Every operand of every instruction is a `Use`. Terminator edge arguments are
stored as ordinary operands of the terminator, with edge slices describing which
operands belong to which successor.

### Instructions

```odin
Instruction :: struct {
    id: Instruction_Id,
    parent: Block_Id,
    opcode: Opcode,

    operands: []Use_Id,
    results: []Value_Id,
    successors: []Successor_Edge,

    attrs: Instruction_Attrs,
    effects: Effect_Set,
    memory: []Memory_Access,

    source: Metadata_Id,
    debug: Metadata_Id,
    semantic: Metadata_Id,
}

Opcode :: enum {
    // Constants and symbols.
    Const,
    Initial,
    Null_Ref,
    Global_Addr,
    Function_Addr,

    // Integer/logical/comparison/select.
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    And,
    Or,
    Xor,
    Not,
    Cmp,
    Select,

    // Casts and conversions.
    Cast,
    Int_Extend,
    Int_Truncate,
    Ref_Cast,
    Addr_Cast,

    // Address and memory.
    Alloca,
    Addr_Of,
    Deref,
    Field_Addr,
    Index_Addr,
    Table_Row_Addr,
    Load,
    Store,

    // Aggregate values.
    Struct_Init,
    Extract_Value,
    Insert_Value,

    // Calls and intrinsics.
    Call,
    Invoke,
    Intrinsic,

    // Control flow.
    Br,
    Cond_Br,
    Switch,
    Return,
    Unreachable,
    Trap,

    // Debug and migration escape hatches.
    Debug_Value,
    Unsupported,
}

Instruction_Attrs :: union {
    None: struct {},
    Constant: Constant_Id,
    Compare: Compare_Attrs,
    Cast: Cast_Attrs,
    Field: Field_Path,
    Call: Call_Attrs,
    Intrinsic: Intrinsic_Call_Attrs,
    Trap: Trap_Attrs,
    Unsupported: Unsupported_Attrs,
}

Compare_Attrs :: struct {
    predicate: Compare_Predicate,
    mode: Compare_Mode,
}

Compare_Predicate :: enum {
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
}

Compare_Mode :: enum {
    Predicate,
    Signed_Integer,
    Unsigned_Integer,
    Decimal,
    String,
    ABAP_Generic,
}
```

Operations with ABAP-specific conversion, table, string, SQL, or system
semantics should usually be intrinsics instead of permanent core opcodes.

## Type System

The IR type system needs to be low-level enough for execution and native
lowering while preserving ABAP layout and semantic information.

```odin
Type :: struct {
    id: Type_Id,
    kind: Type_Kind,
    name: string,
    data: Type_Data,
    semantic: Metadata_Id,
}

Type_Kind :: enum {
    Void,
    Token,
    Predicate,

    Integer,
    Decimal,
    Float,
    String,
    Char,
    Numc,
    Bytes,
    Date,
    Time,

    Struct,
    Table,
    Object,
    Interface,
    Exception,

    Address,
    Pointer,
    Reference,

    Function,
    Unknown,
}

Type_Data :: union {
    None: struct {},
    Integer: Integer_Type,
    Decimal: Decimal_Type,
    Scalar_Text: Scalar_Text_Type,
    Struct: Struct_Type,
    Table: Table_Type,
    Nominal: Nominal_Type,
    Address: Address_Type,
    Pointer: Pointer_Type,
    Reference: Reference_Type,
    Function: Function_Type,
}

Integer_Type :: struct {
    bits: u32,
    signed: bool,
}

Decimal_Type :: struct {
    digits: u32,
    decimals: u32,
}

Scalar_Text_Type :: struct {
    length: u32,
    variable: bool,
    encoding: String_Encoding,
}

Struct_Type :: struct {
    fields: []Struct_Field,
    layout: Layout_Info,
}

Struct_Field :: struct {
    name: string,
    type: Type_Id,
    offset_bits: u64,
    source: Metadata_Id,
    semantic: Metadata_Id,
}

Table_Type :: struct {
    row_type: Type_Id,
    key: Table_Key_Type,
    kind: Table_Kind,
}

Table_Kind :: enum {
    Standard,
    Sorted,
    Hashed,
    Unknown,
}

Table_Key_Type :: struct {
    unique: bool,
    components: []Field_Path,
}

Nominal_Type :: struct {
    object_name: string,
    unit_name: string,
}

Address_Type :: struct {
    pointee: Type_Id,
    space: Address_Space,
    mutable: bool,
}

Pointer_Type :: struct {
    pointee: Type_Id,
    space: Address_Space,
    nullable: bool,
}

Reference_Type :: struct {
    referent: Type_Id,
    mode: Reference_Mode,
    nullable: bool,
}

Reference_Mode :: enum {
    Data,
    Object,
    Field_Symbol,
    Table_Row,
    Runtime_Cell,
}

Address_Space :: enum {
    Local,
    Global,
    Heap,
    Object,
    Table_Row,
    System,
    Host,
}

Function_Type :: struct {
    params: []Type_Id,
    results: []Type_Id,
    effects: Effect_Set,
    can_throw: bool,
}
```

Important rules:

- SSA values are not storage locations.
- Mutable ABAP variables, fields, globals, and table rows are represented by
  address/reference values plus `load` and `store`.
- ABAP data refs and field symbols are runtime reference values, not merely
  compiler addresses.
- Structural types are allowed for locally known layout.
- Nominal metadata preserves ABAP class/interface/DDIC identity.
- `Unknown` is permitted during migration and source recovery, but verified
  executable functions should minimize unknown-typed values.

### Type Construction API

Type construction should go through module-owned interning helpers. Lowering and
tests should not manually append type records.

```odin
type_void :: proc(module: ^Module) -> Type_Id
type_token :: proc(module: ^Module) -> Type_Id
type_predicate :: proc(module: ^Module) -> Type_Id

type_integer :: proc(module: ^Module, bits: u32, signed: bool) -> Type_Id
type_abap_integer :: proc(module: ^Module) -> Type_Id
type_decimal :: proc(module: ^Module, digits: u32, decimals: u32) -> Type_Id
type_string :: proc(module: ^Module) -> Type_Id
type_char :: proc(module: ^Module, length: u32) -> Type_Id
type_numc :: proc(module: ^Module, length: u32) -> Type_Id
type_bytes :: proc(module: ^Module, length: u32, variable: bool) -> Type_Id

type_struct :: proc(
    module: ^Module,
    name: string,
    fields: []Struct_Field,
    semantic: Metadata_Id,
) -> Type_Id

type_table :: proc(
    module: ^Module,
    row_type: Type_Id,
    kind: Table_Kind,
    key: Table_Key_Type,
    semantic: Metadata_Id,
) -> Type_Id

type_address :: proc(
    module: ^Module,
    pointee: Type_Id,
    space: Address_Space,
    mutable: bool,
) -> Type_Id

type_reference :: proc(
    module: ^Module,
    referent: Type_Id,
    mode: Reference_Mode,
    nullable: bool,
) -> Type_Id

type_function :: proc(
    module: ^Module,
    params: []Type_Id,
    results: []Type_Id,
    effects: Effect_Set,
    can_throw: bool,
) -> Type_Id
```

## Constants and Globals

```odin
Constant :: struct {
    id: Constant_Id,
    type: Type_Id,
    data: Constant_Data,
}

Constant_Data :: union {
    Initial: struct {},
    Integer: i64,
    Decimal: Decimal_Literal,
    Predicate: bool,
    String: string,
    Null_Ref: struct {},
    Aggregate: []Constant_Id,
    Global_Address: Global_Id,
    Function_Address: Function_Id,
}

Global :: struct {
    id: Global_Id,
    name: string,
    type: Type_Id,
    mutable: bool,
    linkage: Linkage,
    initializer: Constant_Id,
    address_space: Address_Space,
    effects: Effect_Set,
    source: Metadata_Id,
    debug: Metadata_Id,
    semantic: Metadata_Id,
}
```

Constants are immutable typed values. Globals are addressable symbols. Loading a
global's value requires `global_addr` plus `load`, unless the global is declared
as an immutable constant and explicitly folded.

## Metadata

```odin
Metadata_Record :: struct {
    id: Metadata_Id,
    kind: Metadata_Kind,
    data: Metadata_Data,
}

Metadata_Kind :: enum {
    None,
    Source,
    Debug,
    Semantic,
    Trace,
}

Metadata_Data :: union {
    None: struct {},
    Source: Source_Record,
    Debug: Debug_Record,
    Semantic: Semantic_Record,
    Trace: Trace_Record,
}

Source_Record :: struct {
    path: string,
    start_line: u32,
    start_col: u32,
    end_line: u32,
    end_col: u32,
}

Debug_Record :: struct {
    name: string,
    scope: Metadata_Id,
    source: Metadata_Id,
}

Semantic_Record :: struct {
    object_name: string,
    entity_name: string,
    entity_kind: string,
    type_name: string,
    stable_id: string,
}

Trace_Record :: struct {
    note: string,
    parent: Metadata_Id,
}
```

Metadata must not be required for normal execution, except that VM diagnostics
and stack traces may read it for reporting.

Source metadata expectations:

- required for `Unsupported`, `Trap`, and user-visible intrinsic diagnostics;
- strongly preferred for calls, invokes, SQL, table operations, and system
  writes;
- optional for synthetic address, load/store, and branch operations derived
  from a surrounding source statement.

## Effects, Memory, and Alias Records

```odin
Effect_Set :: bit_set[Effect_Kind; u64]

Effect_Kind :: enum {
    Read_Local,
    Write_Local,
    Read_Global,
    Write_Global,
    Read_Heap,
    Write_Heap,
    Read_System,
    Write_System,
    Read_Table,
    Write_Table,
    SQL_Read,
    SQL_Write,
    IO,
    May_Trap,
    May_Throw,
    Calls_IR,
    Calls_Host,
    Allocates,
    Frees,
    Unsupported,
}

Effect_Scope :: struct {
    id: Effect_Scope_Id,
    name: string,
    kind: Effect_Scope_Kind,
    parent: Effect_Scope_Id,
}

Effect_Scope_Kind :: enum {
    Function_Local,
    Global,
    Heap,
    Object,
    Table,
    SQL,
    System,
    IO,
    Host,
    Unknown,
}

Alias_Class :: struct {
    id: Alias_Class_Id,
    name: string,
    scope: Effect_Scope_Id,
    may_alias_unknown: bool,
}

Memory_Access :: struct {
    kind: Memory_Access_Kind,
    scope: Effect_Scope_Id,
    alias_class: Alias_Class_Id,
    address_operand: u32,
    value_operand: u32,
    type: Type_Id,
}

Memory_Access_Kind :: enum {
    Read,
    Write,
    Read_Write,
    Allocate,
    Free,
    Unknown,
}
```

The initial migration can keep explicit `world`/`token` values for effect
ordering. The effect records still matter because later optimization and native
lowering need to know which operations read/write which regions.

Recommended first policy:

- pure arithmetic/logical ops: no effects;
- `load`: read effect for the addressed scope;
- `store`: write effect for the addressed scope;
- table mutation: table read/write plus may trap if key/type conversion can
  fail;
- SQL: SQL read/write and may trap;
- IO/message: IO and may trap;
- system writes: system write;
- calls/invokes: union of callee effects or conservative call effects.

## Intrinsic Model

Intrinsics represent operations whose semantics are known to the compiler but
are implemented by the VM runtime or future native runtime library. They are not
opaque callbacks.

```odin
Intrinsic_Decl :: struct {
    id: Intrinsic_Id,
    name: string,
    version: u32,
    family: Intrinsic_Family,
    op: Intrinsic_Op,
    signature: Function_Signature,
    payload_schema: Intrinsic_Payload_Kind,
    effects: Effect_Set,
    lowering: Intrinsic_Lowering,
}

Intrinsic_Family :: enum {
    ABAP,
    String,
    Table,
    SQL,
    System,
    Exception,
    Message,
    Runtime,
    Host,
    Unsupported,
}

Intrinsic_Op :: enum {
    // ABAP scalar/value semantics.
    ABAP_Move,
    ABAP_Clear,
    ABAP_Convert,
    ABAP_Compare,
    ABAP_Assign_Field_Symbol,
    ABAP_Unassign_Field_Symbol,

    // Strings.
    String_Concat,
    String_Condense,
    String_Translate,
    String_Split,
    String_Replace,
    String_Shift,
    String_Find,

    // Tables.
    Table_Read,
    Table_Loop_Begin,
    Table_Loop_Next,
    Table_Append,
    Table_Modify,
    Table_Delete,
    Table_Sort,
    Table_Clear,
    Table_Length,

    // SQL.
    SQL_Select,
    SQL_Insert,
    SQL_Update,
    SQL_Delete,
    SQL_Modify,

    // System and IO.
    System_Read_Field,
    System_Write_Field,
    Message_Emit,
    Write_Output,

    // Exceptions.
    Exception_Create,
    Exception_Raise,
    Exception_Catch_Match,
    Exception_Resume,

    // Calls and external integration.
    Runtime_Call_ABAP,
    Host_Call,

    Unsupported,
}

Intrinsic_Lowering :: enum {
    VM_Runtime,
    Native_Runtime,
    Host_ABI,
    Inline_Expansion,
    Not_Lowerable,
}

Intrinsic_Call_Attrs :: struct {
    intrinsic: Intrinsic_Id,
    payload: Intrinsic_Payload,
}

Intrinsic_Payload_Kind :: enum {
    None,
    Call,
    String,
    Table,
    SQL,
    System_Field,
    Message,
    Exception,
    Host,
    Unsupported,
}

Intrinsic_Payload :: union {
    None: struct {},
    Call: Intrinsic_Call_Payload,
    String: Intrinsic_String_Payload,
    Table: Intrinsic_Table_Payload,
    SQL: Intrinsic_SQL_Payload,
    System_Field: Intrinsic_System_Field_Payload,
    Message: Intrinsic_Message_Payload,
    Exception: Intrinsic_Exception_Payload,
    Host: Intrinsic_Host_Payload,
    Unsupported: Intrinsic_Unsupported_Payload,
}
```

### Shared Attribute and Payload Records

The canonical IR should keep operation attributes small and typed. Large ABAP
semantic details belong either in metadata or in intrinsic payload records.

```odin
Field_Path :: struct {
    root_type: Type_Id,
    components: []Field_Path_Component,
    source: Metadata_Id,
}

Field_Path_Component :: struct {
    name: string,
    index: u32,
    type: Type_Id,
    kind: Field_Component_Kind,
    source: Metadata_Id,
}

Field_Component_Kind :: enum {
    Struct_Field,
    Object_Attribute,
    Table_Row_Field,
    Dynamic,
}

Cast_Attrs :: struct {
    kind: Cast_Kind,
    checked: bool,
}

Cast_Kind :: enum {
    Bitcast,
    Integer_Extend,
    Integer_Truncate,
    Integer_To_Decimal,
    Decimal_To_Integer,
    Address_To_Pointer,
    Pointer_To_Address,
    Reference_Upcast,
    Reference_Downcast,
}

Call_Attrs :: struct {
    callee: Function_Id,
    calling_convention: Calling_Convention,
    direct: bool,
}

Trap_Attrs :: struct {
    reason: Trap_Reason,
    message: string,
}

Trap_Reason :: enum {
    Unsupported,
    Division_By_Zero,
    Invalid_Reference,
    Invalid_Field_Symbol,
    Conversion_Failed,
    Table_Key_Not_Found,
    SQL_Error,
    Runtime_Error,
}

Unsupported_Attrs :: struct {
    feature: string,
    message: string,
    recoverable: bool,
}
```

Intrinsic payloads should describe semantic shape without embedding VM
implementation details:

```odin
Intrinsic_Call_Payload :: struct {
    callee_name: string,
    target_function: Function_Id,
    kind: ABAP_Call_Kind,
    arguments: []ABAP_Call_Argument,
    dynamic: bool,
}

ABAP_Call_Kind :: enum {
    Form,
    Method,
    Function_Module,
    Constructor,
    Event,
    Dynamic,
}

ABAP_Call_Argument :: struct {
    name: string,
    section: ABAP_Call_Section,
    operand_index: u32,
    pass_mode: ABAP_Pass_Mode,
    source: Metadata_Id,
}

ABAP_Call_Section :: enum {
    None,
    Importing,
    Exporting,
    Changing,
    Receiving,
    Tables,
    Exceptions,
}

ABAP_Pass_Mode :: enum {
    By_Value,
    By_Address,
    By_Reference,
    Result_Code,
}

Intrinsic_String_Payload :: struct {
    operation: Intrinsic_Op,
    case_sensitive: bool,
    respecting_blanks: bool,
    source_count: u32,
}

Intrinsic_Table_Payload :: struct {
    operation: Intrinsic_Op,
    table_type: Type_Id,
    row_type: Type_Id,
    key_kind: Table_Key_Selection,
    key_components: []Table_Key_Component,
    result: Table_Result_Mode,
    binary_search: bool,
    stable: bool,
    dynamic: bool,
}

Table_Key_Selection :: enum {
    None,
    Primary,
    Secondary_Name,
    Component_List,
    Index,
    Dynamic,
}

Table_Key_Component :: struct {
    field: Field_Path,
    operand_index: u32,
    comparison: Compare_Predicate,
    source: Metadata_Id,
}

Table_Result_Mode :: enum {
    None,
    Value,
    Address,
    Reference,
    Iterator,
    Subrc,
    Count,
}

Intrinsic_SQL_Payload :: struct {
    operation: Intrinsic_Op,
    source_kind: SQL_Source_Kind,
    source_name: string,
    source_type: Type_Id,
    row_type: Type_Id,
    result: SQL_Result_Mode,
    projections: []SQL_Projection,
    dynamic: bool,
}

SQL_Source_Kind :: enum {
    Table,
    View,
    CDS,
    Join,
    Dynamic,
    Unknown,
}

SQL_Result_Mode :: enum {
    None,
    Scalar,
    Row,
    Table,
    Subrc,
    Dbcnt,
}

SQL_Projection :: struct {
    source_field: Field_Path,
    result_field: Field_Path,
    operand_index: u32,
    aggregate: SQL_Aggregate,
    source: Metadata_Id,
}

SQL_Aggregate :: enum {
    None,
    Count,
    Sum,
    Min,
    Max,
    Avg,
}

Intrinsic_System_Field_Payload :: struct {
    name: string,
    field_type: Type_Id,
    write: bool,
}

Intrinsic_Message_Payload :: struct {
    message_id: string,
    message_type: string,
    message_number: string,
    argument_count: u32,
}

Intrinsic_Exception_Payload :: struct {
    class_name: string,
    exception_type: Type_Id,
    resumable: bool,
    catch_classes: []string,
}

Intrinsic_Host_Payload :: struct {
    symbol: string,
    abi: Host_ABI,
    parameter_types: []Type_Id,
    result_types: []Type_Id,
}

Host_ABI :: enum {
    C,
    System,
    Custom,
}

Intrinsic_Unsupported_Payload :: struct {
    feature: string,
    message: string,
}
```

Intrinsic names should be stable and versioned:

- `abap.move.v1`
- `abap.convert.v1`
- `abap.string.concat.v1`
- `abap.table.read.v1`
- `abap.sql.select.v1`
- `abap.system.read_field.v1`
- `abap.message.emit.v1`
- `host.call.v1`

Changing semantics should create a new version rather than silently changing an
existing intrinsic declaration.

### VM Intrinsics vs Host Calls

VM intrinsics are portable ABAP semantics implemented by `src/vm/runtime`. They
should be available to the interpreter and to future native lowering through a
runtime library.

Host calls are external ABI calls. They require explicit host symbols,
marshalling, and effect declarations. They should not be used to model ABAP
semantics that the compiler understands.

## Operation Vocabulary

### Core Operations

Core operations should be small and language-neutral:

- constants and symbol references;
- arithmetic and boolean operations over known primitive types;
- comparisons;
- casts whose behavior is fully type-defined;
- address formation;
- load/store;
- aggregate construction and field extraction;
- call/invoke;
- branch/return/unreachable/trap.

### ABAP Semantics as Intrinsics

These should usually be intrinsics:

- ABAP generic assignment and conversion;
- packed/decimal conversion until layout rules are complete;
- string operations with ABAP semantics;
- field-symbol assignment/unassignment;
- table operations;
- SQL operations;
- system fields;
- messages and output;
- exception construction and class matching;
- dynamic calls.

### Control-Flow Operations

```odin
// Unconditional branch.
br ^target(args...)

// Conditional branch.
cond_br %cond, ^true(args...), ^false(args...)

// Multi-way branch, optional in the first migration.
switch %value, default ^bb(args...), cases [...]

// Return from function.
return values...

// No successor.
unreachable

// User/runtime-visible failure.
trap reason, operands...
```

`invoke` is the preferred operation for calls or intrinsics that may throw into
recoverable ABAP exception handling:

```odin
%results... = invoke @callee(args...)
    normal ^normal(args...)
    exception ^catch(exception_value, args...)
```

Recoverable exception flow must use `Invoke`; verifier defaults reject
non-`Invoke` `May_Throw` operations. The only compatibility policy is the
explicit legacy top-level propagation option, for transitional callers that
still need to surface an unhandled exception as a VM-level trap.

## Builder API

```odin
Builder :: struct {
    module: ^Module,
    function: Function_Id,
    block: Block_Id,
    current_effect: Value_Id,
}

builder_begin_module :: proc(allocator: mem.Allocator, source_name: string) -> Module
builder_destroy_module :: proc(module: ^Module)

builder_begin_function :: proc(
    module: ^Module,
    name: string,
    signature: Function_Signature,
    role: Function_Role,
    source: Metadata_Id,
) -> Function_Id

builder_append_block :: proc(
    module: ^Module,
    function: Function_Id,
    name: string,
    arg_types: []Type_Id,
    source: Metadata_Id,
) -> Block_Id

builder_position_at_end :: proc(builder: ^Builder, block: Block_Id)

builder_emit :: proc(
    builder: ^Builder,
    opcode: Opcode,
    operands: []Value_Id,
    result_types: []Type_Id,
    attrs: Instruction_Attrs,
    effects: Effect_Set,
    source: Metadata_Id,
) -> []Value_Id

builder_emit_intrinsic :: proc(
    builder: ^Builder,
    intrinsic: Intrinsic_Id,
    operands: []Value_Id,
    result_types: []Type_Id,
    payload: Intrinsic_Payload,
    source: Metadata_Id,
) -> []Value_Id

builder_emit_load :: proc(
    builder: ^Builder,
    address: Value_Id,
    result_type: Type_Id,
    access: Memory_Access,
    source: Metadata_Id,
) -> Value_Id

builder_emit_store :: proc(
    builder: ^Builder,
    address: Value_Id,
    value: Value_Id,
    access: Memory_Access,
    source: Metadata_Id,
)

builder_emit_br :: proc(
    builder: ^Builder,
    target: Block_Id,
    args: []Value_Id,
    source: Metadata_Id,
)

builder_emit_cond_br :: proc(
    builder: ^Builder,
    condition: Value_Id,
    true_target: Block_Id,
    true_args: []Value_Id,
    false_target: Block_Id,
    false_args: []Value_Id,
    source: Metadata_Id,
)

builder_emit_return :: proc(
    builder: ^Builder,
    values: []Value_Id,
    source: Metadata_Id,
)
```

The builder should be a convenience layer, not the only way to mutate IR.
Verifier correctness must not depend on builder-only state.

## Query API

```odin
module_function :: proc(module: ^Module, id: Function_Id) -> (^Function, bool)
module_type :: proc(module: ^Module, id: Type_Id) -> (^Type, bool)
module_constant :: proc(module: ^Module, id: Constant_Id) -> (^Constant, bool)
module_global :: proc(module: ^Module, id: Global_Id) -> (^Global, bool)
module_intrinsic :: proc(module: ^Module, id: Intrinsic_Id) -> (^Intrinsic_Decl, bool)

function_block :: proc(function: ^Function, id: Block_Id) -> (^Basic_Block, bool)
function_instruction :: proc(function: ^Function, id: Instruction_Id) -> (^Instruction, bool)
function_value :: proc(function: ^Function, id: Value_Id) -> (^Value, bool)

value_type :: proc(module: ^Module, function: ^Function, value: Value_Id) -> Type_Id
value_definition :: proc(function: ^Function, value: Value_Id) -> Value_Def

instruction_operands :: proc(function: ^Function, inst: Instruction_Id) -> []Value_Id
instruction_results :: proc(function: ^Function, inst: Instruction_Id) -> []Value_Id
instruction_successors :: proc(function: ^Function, inst: Instruction_Id) -> []Successor_Edge

is_terminator :: proc(opcode: Opcode) -> bool
instruction_has_side_effects :: proc(module: ^Module, function: ^Function, inst: Instruction_Id) -> bool
```

Queries should never silently manufacture missing data. They should return
`bool` or assertions depending on whether the call site is verifier-facing or
internal after verification.

## Mutation API

```odin
set_operand :: proc(
    function: ^Function,
    instruction: Instruction_Id,
    operand_index: u32,
    replacement: Value_Id,
)

replace_all_uses :: proc(
    function: ^Function,
    old_value: Value_Id,
    new_value: Value_Id,
)

replace_uses_if :: proc(
    function: ^Function,
    old_value: Value_Id,
    new_value: Value_Id,
    predicate: proc(use: Use) -> bool,
)

erase_instruction :: proc(
    module: ^Module,
    function: ^Function,
    instruction: Instruction_Id,
) -> bool

move_instruction_before :: proc(
    function: ^Function,
    instruction: Instruction_Id,
    before: Instruction_Id,
)

split_block :: proc(
    module: ^Module,
    function: ^Function,
    block: Block_Id,
    before: Instruction_Id,
    new_name: string,
) -> Block_Id

replace_terminator :: proc(
    module: ^Module,
    function: ^Function,
    block: Block_Id,
    new_terminator: Instruction_Id,
)
```

Mutation APIs must maintain use lists and invalidate CFG/dominance caches.
Passes should not open-code full scans for ordinary replacements.

Dead instruction deletion rule:

- no live result uses;
- not a terminator;
- no side effects;
- no memory writes;
- no `May_Trap`, `May_Throw`, `IO`, SQL write, host call, or unsupported
  effect.

## Analysis API

```odin
CFG_Analysis :: struct {
    function: Function_Id,
    generation: u64,
    predecessors: []Block_List,
    successors: []Block_List,
    reverse_postorder: []Block_Id,
}

Dominance_Analysis :: struct {
    function: Function_Id,
    generation: u64,
    idom: []Block_Id,
    dom_tree_children: []Block_List,
}

build_cfg :: proc(module: ^Module, function: ^Function, allocator: mem.Allocator) -> CFG_Analysis
build_dominance :: proc(
    module: ^Module,
    function: ^Function,
    cfg: ^CFG_Analysis,
    allocator: mem.Allocator,
) -> Dominance_Analysis

dominates_block :: proc(dom: ^Dominance_Analysis, a: Block_Id, b: Block_Id) -> bool
dominates_value_use :: proc(
    module: ^Module,
    function: ^Function,
    dom: ^Dominance_Analysis,
    value: Value_Id,
    user: Instruction_Id,
    operand_index: u32,
) -> bool
```

Analysis objects are invalidated by mutation generation changes.

## Verifier API and Rules

```odin
Verify_Options :: struct {
    require_sources_for_user_visible_ops: bool,
    require_entry_reachable: bool,
    allow_unknown_types: bool,
    allow_legacy_top_level_may_throw_propagation: bool,
    verify_effect_tokens: bool,
}

Verify_Result :: struct {
    ok: bool,
    diagnostics: [dynamic]Verify_Diagnostic,
}

Verify_Diagnostic :: struct {
    kind: Verify_Diagnostic_Kind,
    function: Function_Id,
    block: Block_Id,
    instruction: Instruction_Id,
    value: Value_Id,
    message: string,
}

verify_module :: proc(
    module: ^Module,
    allocator: mem.Allocator,
    options := Verify_Options{},
) -> Verify_Result

verify_function :: proc(
    module: ^Module,
    function: Function_Id,
    allocator: mem.Allocator,
    options := Verify_Options{},
) -> Verify_Result
```

Required verifier rules:

- every ID references an object owned by the expected module/function;
- every function has an entry block;
- every block has exactly one terminator;
- no normal instruction appears after a terminator;
- entry block arguments match the function signature;
- terminator successor edges target valid blocks;
- edge argument arity and types match target block arguments;
- branch conditions have predicate type;
- return operands match function result types;
- instruction result types match opcode/intrinsic rules;
- every operand type is legal for its opcode;
- `load` operand is address/pointer/reference of matching pointee type;
- `store` value type matches destination pointee type;
- field paths are valid for the base type;
- table operations use a table type and compatible row/key types;
- casts are legal or explicitly use ABAP conversion intrinsics;
- SSA dominance holds for all operands;
- same-block instruction result uses appear after the defining instruction;
- block arguments are treated as definitions at block entry;
- edge arguments are treated as uses in the predecessor terminator;
- pure operations have no side effects;
- effectful operations declare effects;
- memory accesses name valid scopes and alias classes;
- explicit effect/world tokens, if enabled, are linearly threaded;
- intrinsic ID, version, payload kind, signature, and effects match the
  declaration;
- recoverable may-throw operations use `Invoke` or valid exception edges;
- user-visible traps/unsupported operations have source metadata.

## Printer API

```odin
Print_Options :: struct {
    show_ids: bool,
    show_types: bool,
    show_effects: bool,
    show_metadata: bool,
    stable_names: bool,
}

print_module :: proc(module: ^Module, writer: io.Writer, options := Print_Options{}) -> bool
print_function :: proc(
    module: ^Module,
    function: Function_Id,
    writer: io.Writer,
    options := Print_Options{},
) -> bool
```

The printer should be deterministic and suitable for regression tests. Debug
names may be displayed, but stable numbering should not depend on them.

Example shape:

```text
func @start(%world: token) -> token effects(system, io) {
^entry(%world: token):
  %addr = global_addr @sy_subrc : addr<i32, system>
  %zero = const 0 : i32
  %world1 = store %zero, %addr [system.write] : token
  br ^exit(%world1)

^exit(%world2: token):
  return %world2
}
```

## Semantic Lowering Boundary

`src/ir/lower.odin` should emit canonical IR directly.

```odin
Lower_Options :: struct {
    emit_debug_metadata: bool,
    preserve_semantic_metadata: bool,
    allow_unsupported_ops: bool,
}

Lower_Result :: struct {
    module: Module,
    diagnostics: [dynamic]Lower_Diagnostic,
}

lower_project_to_ir :: proc(
    project: ^semantic.Project_Analysis,
    allocator: mem.Allocator,
    options := Lower_Options{},
) -> Lower_Result
```

Lowering policy:

- ABAP variables, globals, fields, and table rows lower to addresses plus
  loads/stores.
- Source-level expression values lower to SSA values.
- ABAP generic move/convert semantics lower to intrinsics.
- Primitive integer operations lower to core arithmetic only when ABAP
  conversion semantics are already resolved.
- String, table, SQL, system, message, and exception semantics lower to
  intrinsics.
- Control flow lowers to blocks with block arguments.
- Loops use block arguments for carried values/effect tokens.
- Exception-capable calls lower to `Invoke` once catch edges are modeled.
- Unsupported source constructs lower to `Unsupported` instructions with source
  metadata and an explicit `Unsupported` effect.

## VM Boundary

The long-term VM entry point should accept canonical IR:

```odin
package abap_frontend_vm

Execute_Options :: struct {
    entry_name: string,
    capture_io: bool,
    max_steps: u64,
    trace: bool,
}

Execute_Result :: struct {
    ok: bool,
    trap: runtime.Trap,
    events: []runtime.Event,
    final_values: []runtime.Value,
}

execute_module :: proc(
    module: ^ir.Module,
    options: Execute_Options,
    allocator: mem.Allocator,
) -> Execute_Result
```

The VM may prepare a private execution cache:

```odin
Prepared_Module :: struct {
    source: ^ir.Module,
    functions: []Prepared_Function,
    entries: []Prepared_Entry,
}

Prepared_Function :: struct {
    ir_function: ir.Function_Id,
    frame_layout: Frame_Layout,
    block_layouts: []Prepared_Block,
    instruction_layouts: []Prepared_Instruction,
}

prepare_module :: proc(
    module: ^ir.Module,
    allocator: mem.Allocator,
) -> (Prepared_Module, Prepare_Diagnostic_List)
```

That prepared form belongs to `vm`, not `ir`. It may use dense indices,
register-like frame slots, or dispatch-friendly instruction records internally,
but it should not become the public compiler IR.

## VM Runtime Boundary

`src/vm/runtime` owns:

- runtime `Value`;
- mutable cells;
- references and field symbols;
- table storage;
- system fields;
- SQL stubs or adapters;
- IO/message capture;
- traps;
- runtime exception state.

`src/ir` may describe an intrinsic such as `abap.table.read.v1`. The VM decides
how to implement that intrinsic against runtime tables.

## Migration Plan

### Phase 1: Canonical IR Schema

Goal:

- Establish the final `src/ir` data model.
- Replace high-level operation taxonomy with low-level opcodes and intrinsics.
- Add real value/use lists.
- Add verifier coverage for the new representation.

Likely files touched:

- `src/ir/ids.odin`
- `src/ir/module.odin`
- `src/ir/type.odin`
- `src/ir/value.odin`
- `src/ir/instruction.odin`
- `src/ir/block.odin`
- `src/ir/function.odin`
- `src/ir/effect.odin`
- `src/ir/intrinsic.odin`
- `src/ir/metadata.odin`
- `src/ir/builder.odin`
- `src/ir/query.odin`
- `src/ir/mutate.odin`
- `src/ir/verify.odin`
- `src/ir/print.odin`
- `src/ir/*_test.odin`

Validation:

- focused `src/ir` unit tests;
- builder/verifier/printer round-trip-style tests;
- dominance and block-argument tests;
- use-list mutation tests.

Can delete after this phase:

- nothing required yet, but new code should not depend on `src/ir/bytecode`.

### Phase 2: Direct Semantic Lowering

Goal:

- Rewrite lowering to emit canonical IR.
- Map existing high-level ABAP payload concepts into typed intrinsic payloads.
- Stop creating the old high-level graph as the source of execution.

Likely files touched:

- `src/ir/lower.odin`
- `src/ir/intrinsic.odin`
- `src/ir/type.odin`
- `src/ir/builder.odin`
- `src/ir/verify.odin`
- `src/semantic/*` only where existing semantic facts are insufficient.

Validation:

- existing lowering tests ported to canonical printer expectations;
- verifier run after every lowered function;
- smoke lower reports with table, SQL, string, system field, and exception
  constructs.

Can delete after this phase:

- old high-level-only operation constructors;
- old payload variants that have canonical intrinsic replacements.

### Phase 3: VM Executes Canonical IR

Goal:

- Change VM execution to accept `^ir.Module`.
- Add VM-private preparation if needed.
- Implement canonical op dispatch and intrinsic dispatch.

Likely files touched:

- `src/vm/api.odin`
- `src/vm/machine.odin`
- `src/vm/prepare.odin`
- `src/vm/prepare_types.odin`
- `src/vm/prepare_lowering.odin`
- `src/vm/frame.odin`
- `src/vm/scalar_ops.odin`
- `src/vm/memory_ops.odin`
- `src/vm/intrinsic*.odin`
- `src/vm/diagnostics.odin`
- `src/vm/runtime/*.odin`
- `src/vm/vm_test.odin`

Validation:

- VM tests over canonical IR builders;
- ABAP smoke programs through CLI;
- table, SQL stub, system field, string, message, and exception execution tests;
- verifier before VM prepare.

Can delete after this phase:

- `src/vm/executable_adapter.odin`.

### Phase 4: Delete Bytecode Bridge

Goal:

- Remove the public and internal dependency on `src/ir/bytecode`.
- Make CLI IR output print canonical IR only.
- Make interpreter execution go through canonical IR plus VM.

Likely files touched:

- `src/ir/bytecode/*.odin`
- `src/ir/executable*.odin`
- `cmd/abap_frontend/main.odin`
- `cmd/abap_interpreter/main.odin`
- `src/cli/cli.odin`
- tests that reference bytecode output.

Validation:

- full `src/ir` tests;
- full `src/vm` tests;
- CLI smoke tests;
- root test script.

Can delete after this phase:

- `src/ir/bytecode`
- `src/ir/executable*.odin`
- bytecode printer tests;
- bytecode lowering tests;
- runtime callback bridge tests.

### Phase 5: Optimization Readiness

Goal:

- Add initial canonical passes over real SSA.
- Prove mutation APIs and verifier are strong enough for optimization.

Likely files touched:

- `src/ir/analysis_cfg.odin`
- `src/ir/analysis_dominance.odin`
- `src/ir/mutate.odin`
- `src/ir/pass_dce.odin`
- `src/ir/pass_fold.odin`
- `src/ir/verify.odin`

Validation:

- verifier after each pass in tests;
- dead-code deletion tests;
- constant folding tests;
- dominance-preserving mutation tests.

Can delete after this phase:

- any old pass code that scans executable registers or replaces instructions
  with `Nop` tombstones.

## Open Questions

### Effect Tokens

The current world-token discipline is useful and easy to verify. It also makes
effect ordering explicit for the VM. The long-term question is whether it should
remain in the IR permanently or become an early lowering/verification device
that can later be replaced by memory/effect analysis.

Recommendation for the migration:

- keep explicit token values for effectful ABAP lowering;
- also record effect metadata;
- allow later passes to reason from metadata without removing tokens too early.

### ABAP Type Precision

The first canonical slice can support integers, predicates, strings, structures,
tables, addresses, and references. Packed decimals, char/numc lengths, dates,
times, and exact DDIC layout can be added without changing the core shape if the
type arena is designed for them now.

Recommendation:

- include the type variants now;
- allow some records to carry unknown/incomplete layout;
- make verifier strict only where execution depends on exact layout.

### Exceptions

ABAP exceptions can be modeled with intrinsics initially, but the target IR
should have `Invoke` and explicit exception edges.

Recommendation:

- design `Invoke` now;
- use it for lowered `TRY`/`CATCH` once VM support exists;
- avoid baking exception flow into opaque runtime callbacks.

### Semantic Identity

Current metadata can hold borrowed semantic pointers. That is fragile for a
canonical executable IR.

Recommendation:

- metadata should store stable names/IDs from semantic analysis;
- borrowed pointers may remain only as non-executing debug convenience while the
  lowering pass is active.

### Native Lowering

The IR should not overfit LLVM before ABAP layout is mature, but it should avoid
choices that block LLVM lowering.

Recommendation:

- keep typed SSA, explicit control flow, explicit memory operations, explicit
  effects, and explicit calls;
- avoid VM-only concepts in `ir`;
- keep VM preparation private to `vm`.
