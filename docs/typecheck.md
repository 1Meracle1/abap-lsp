# Typecheck Architecture

This document describes the current semantic typecheck pass in the Odin ABAP
frontend. It is intended as implementation documentation for follow-up work.

The pass is deliberately conservative: it should emit no diagnostic when a type
fact, call signature, field path, table row, or SQL source cannot be proven with
high confidence.

## Source Files

- `src/semantic/analyze/typecheck.odin`: typecheck entry points, compatibility
  rules, and typecheck diagnostic messages.
- `src/semantic/analyze/def_map.odin`: semantic data structures used by the
  pass, including `Type_Fact_Data`, `Operand_Data`, `Assignment_Site_Data`,
  `Call_Site_Data`, `Sql_Target_Data`, and `Diagnostic`.
- `src/semantic/analyze/facts.odin`: collector-side creation of assignment
  sites, call sites, SQL targets, references, and initial type facts.
- `src/semantic/analyze/infer.odin`: fixpoint inference for expression facts,
  operands, call results, table-line assignment rows, and inline data updates.
- `src/semantic/analyze/validate.odin`: validation orchestration and diagnostic
  deduplication.
- `cmd/abap_frontend/main.odin`: CLI diagnostic severity rendering.

## Pipeline

The project analysis pipeline relevant to typecheck is:

```text
parse
  -> collect declarations, references, assignment sites, call sites, SQL facts
  -> resolve local scopes
  -> resolve cross-unit project references
  -> resolve Open SQL names
  -> infer semantic facts to a fixpoint
  -> validate diagnostics
       -> validate_typecheck_diagnostics
```

`check_project_bodies_for_units` runs inference before validation. The inference
loop updates `Unit_Analysis.expression_facts`, `Unit_Analysis.operands`,
assignment site LHS/RHS facts, concatenate source facts, and inferred inline
symbol types until no more project facts change.

`validate_unit_diagnostics` keeps collector diagnostics that must survive
revalidation, runs the core validators, and then calls
`validate_typecheck_diagnostics`. Typecheck diagnostics are appended through
`append_diag`, which deduplicates by kind, range, and message.

`validate_typecheck_diagnostics` exits immediately unless the checked unit has
`source_mode == .Full`. Dependency-interface units are not checked directly,
but complete dependency-interface signatures can be trusted as providers for
calls made from full-source units.

## Semantic Data Model

`Type_Data` is the canonical type arena entry. Current kinds are `Unknown`,
`Builtin`, `Named`, `Structure`, `Table`, `Ref`, `Class`, and `Interface`.

`Field_Type_Ref_Data` stores source-level declared type syntax: namespace,
`REF TO`, base name, and optional field path. It is still used for diagnostics,
signature facts, dependency hints, and exact declared-type equality.

`Type_Fact_Data` is the checked type fact used by this pass:

- `type_id` and `type_unit` point into a unit type arena.
- `structure` and `structure_unit` identify known structure shape.
- `declared_type`, `has_declared_type`, and `type_clause_display` retain source
  type information for exact checks and messages.
- `table_line` optionally stores an explicit row fact for table values.
- `confidence` is `.High` or `.Low`.

`Operand_Data` is the operand-like expression annotation. It records a range,
mode, type fact, optional symbol handle, and an `Assignable` flag. Typecheck
uses operands to build range-based type fact and writable indexes.

`Assignment_Site_Data` records statement-level assignment shape: LHS/RHS
ranges, LHS access path, LHS/RHS type facts, and flags such as
`Assigns_Table_Line`, `Is_Corresponding`, and `Is_Downcast`.

`Call_Site_Data` records method, implicit-method, and function calls with
ordered `Call_Argument_Data`. Signatures are read from the callee entity's
`Decl_Info_Data.signature_parameters`.

`Sql_Target_Data` records Open SQL `INTO`/`APPENDING` targets by query id,
target range, target name, and flags such as `Is_Table` and
`Is_Corresponding`.

## Confidence

Typecheck diagnostics require high-confidence facts. Unknown or low-confidence
facts normally make the check silent.

High-confidence sources currently include:

- resolved local/full-source symbols and their type facts;
- builtin literals and builtin routine return facts;
- string templates with exact string facts;
- direct structure fields where the provider fact is high confidence;
- direct object attribute selectors such as `ref->attr` and
  `ref->attr-field`, when member lookup proves the first selector is an
  attribute;
- direct local full-source method call results when the method is not a
  redefinition and has a known `RETURNING` or `RECEIVING` parameter;
- complete dependency-interface method/function signatures, but only when every
  parameter has a name, complete section/passing metadata, a declared type, and
  a high-confidence parameter fact;
- Open SQL projection fields from full-source providers, or complete scalar
  dependency-interface/DDIC-like projection facts.

Low-confidence or silent sources include:

- unknown, display-only, or unresolved facts;
- method chains and larger expressions that merely contain calls;
- inherited/redefinition/alias call results that do not resolve to a trusted
  direct result fact;
- dynamic selectors, data-reference dereferences, and multiple object
  traversals such as `lo->child->name`;
- non-complete dependency-interface signatures;
- `CORRESPONDING` and `MOVE-CORRESPONDING` assignment sites;
- table rows that cannot be extracted from the RHS table fact;
- SQL targets with table/corresponding targets, unknown targets, non-column
  projections, or ambiguous/missing SQL sources.

## Assignment Typecheck

`typecheck_assignments` checks `Unit_Analysis.assignment_sites`.

Preconditions:

- the unit is full source;
- the site is not `Is_Corresponding`;
- LHS and RHS facts are high confidence;
- if the RHS range is exactly a call site, the call result must also produce a
  high-confidence fact.

Inference rewrites `Assigns_Table_Line` RHS facts to the source table row fact
before validation. If the row is not known, the assignment stays silent.

Compatibility rules:

- identical declared type references are compatible;
- reference assignments are checked when both `REF TO` targets are known;
- exact same reference target is compatible;
- concrete data refs can widen to `REF TO data`;
- class/interface refs can widen to `REF TO object`;
- subclass refs can widen to superclass refs;
- class/interface refs can widen to exposed interface refs;
- `?=` downcast assignments are accepted for proven object/class/interface
  downcast relationships;
- impossible data/object boundary crossings and unrelated concrete class refs
  are errors;
- generic builtin destinations such as `any` and `data` accept known matching
  source families;
- scalar assignment conversion uses groups: numeric, character, byte, date,
  time, and generic simple;
- `d` to `t` and `t` to `d` are incompatible;
- other known scalar cross-group assignments are currently accepted;
- table-to-table assignment is accepted, table/scalar is silent;
- structure-to-structure assignment is accepted, structure/scalar is silent.

Diagnostic:

- `Incompatible_Assignment_Type`

The diagnostic range is the RHS range. The message is:

```text
The type of '<rhs text>' cannot be converted to the type of '<lhs text>'
```

When both type names are known, the message appends:

```text
(current type '<rhs type>', expected type '<lhs type>')
```

## Call Typecheck

`typecheck_calls` checks `Unit_Analysis.call_sites`.

Only these target kinds currently resolve to signatures for typecheck:

- `Method`
- `Implicit_Method`
- `Function`

Constructors, reports, events, regular routines, and exception sections do not
currently produce type compatibility diagnostics in this pass.

Signature trust rules:

- full-source signature units are trusted;
- dependency-interface signatures are trusted only when
  `typecheck_external_signature_complete` proves all parameters complete;
- missing-required checks additionally reject redefinitions, event handlers,
  aliases, non-method/function targets, and non-direct method signatures.

Argument mapping:

- named arguments map by case-insensitive formal parameter name and compatible
  ABAP section direction;
- duplicate named arguments are diagnosed by section plus name;
- unknown named parameters are reported only for simple names, not selector-like
  names containing `-`, `>`, or `~`;
- positional mapping is high confidence only when it resolves back to the same
  parameter chosen by `typecheck_call_parameter`;
- unsectioned positional calls are accepted only for method-like calls with a
  single actual and exactly one candidate formal;
- sectioned positional calls map by ordinal within the section;
- redefinitions and aliases disable positional mapping.

Writable checks:

- actuals supplied to `IMPORTING`, `CHANGING`, `RECEIVING`, or `TABLES` call
  sections must cover an assignable operand range;
- non-writable actuals are reported as `Incompatible_Argument_Type`.

Type compatibility checks:

- actual facts come from the range fact index, falling back to the collected
  argument fact;
- if the actual range is exactly a call site, a high-confidence call result fact
  is used;
- larger actual expressions that merely contain a call are silent;
- formal facts come from the callee signature parameter;
- both facts must be high confidence;
- only generic builtin formal types are currently considered for argument
  typecheck;
- `numeric`, `decfloat`, and `clike` can reject known incompatible actuals;
- other generic builtin formals are currently accepted by this pass;
- concrete formal type compatibility is not currently emitted as a diagnostic.

Diagnostics:

- `Duplicate_Named_Parameter`
- `Unknown_Named_Parameter`
- `Incompatible_Argument_Type`
- `Missing_Required_Parameter`

Messages:

```text
Duplicate formal parameter '<name>'
Formal parameter does not exist: '<name>'
The field cannot be modified: '<actual text>'
'<actual text>' is not type-compatible with formal parameter '<formal name>'
Missing required formal parameter: '<formal name>'
```

For argument type mismatches, when both type names are known, the message
appends:

```text
(current type '<actual type>', expected type '<formal type>')
```

Required parameters are:

- function `IMPORTING`, `CHANGING`, and `TABLES` parameters;
- method `IMPORTING` and `CHANGING` parameters.

`OPTIONAL` and `DEFAULT` parameters are not required.

## Open SQL Target Typecheck

`typecheck_open_sql_targets` checks scalar Open SQL targets.

Preconditions:

- the unit is full source;
- the target is not marked `Is_Table`;
- the target is not marked `Is_Corresponding`;
- the target has a simple target name;
- the query has exactly one column projection usable by this pass;
- the query has exactly one matching external source;
- the source resolves through `Project_Index` to a structured symbol;
- the projected field resolves in that structure;
- the target name resolves to a value handle;
- source and target facts are high confidence.

Compatibility uses scalar assignment conversion only. Unknown source/target
types, structures, tables, expression projections, aggregate projections, stars,
and ambiguous sources stay silent.

Diagnostic:

- `Invalid_Open_Sql_Into_Target`

Message:

```text
Open SQL target is not compatible: '<target name>'
```

When both type names are known, the message appends:

```text
(current type '<source type>', expected type '<target type>')
```

## Diagnostic Type Names

`typecheck_message_with_type_detail` appends type details only when
`typecheck_diagnostic_type_name` can name both sides.

Name sources, in priority order:

1. explicit declared type with no field path;
2. builtin type name;
3. `REF TO <target>` for references;
4. named structure;
5. raw type arena names for builtin, named, structure, class, or interface
   types;
6. `TABLE OF <row type>` for known table rows.

Unknown or low-information sides do not render as `unknown`; the old message is
kept unchanged.

## Diagnostic Severity

`Diagnostic` stores only `kind`, `range`, and `message`. There is no severity
field in semantic analysis.

The CLI maps severities when printing:

- warnings: `Shadowed_Symbol` and `Unreachable_Code`, unless
  `warnings_as_errors` is set;
- errors: every other diagnostic kind;
- info diagnostics: none.

All typecheck-originated diagnostics listed in this document are currently
rendered as errors. The typecheck pass emits no warnings and no info
diagnostics.

## Typecheck Diagnostic Catalog

| Kind | Severity | Emitted By | Trigger |
| --- | --- | --- | --- |
| `Incompatible_Assignment_Type` | Error | `typecheck_assignments` | High-confidence assignment where compatibility is known and false. |
| `Duplicate_Named_Parameter` | Error | `typecheck_calls` | Same named argument appears more than once in the same call section. |
| `Unknown_Named_Parameter` | Error | `typecheck_calls` | Simple named argument cannot be mapped to a trusted signature parameter. |
| `Incompatible_Argument_Type` | Error | `typecheck_calls` | Non-writable actual for an output-like section, or high-confidence generic formal argument mismatch. |
| `Missing_Required_Parameter` | Error | `typecheck_calls` | Trusted complete signature has a required formal not supplied by named or high-confidence positional mapping. |
| `Invalid_Open_Sql_Into_Target` | Error | `typecheck_open_sql_targets` | High-confidence scalar SQL projection cannot be converted to the scalar target type. |

No typecheck warning or info diagnostic kinds exist today.

## Follow-up Guidance

Preserve the current rule: unknown should be silent. Before enabling a new
diagnostic path, make the producing fact or signature high confidence and add a
focused test that proves both the positive diagnostic and the silent unknown
case.

Good next places to improve are:

- concrete formal argument checks after formal type facts are reliable enough;
- inherited/redefined/interface method result trust;
- constructor call signatures;
- richer Open SQL projection and target shapes;
- replacing remaining range-fact lookups with direct operand annotations.
