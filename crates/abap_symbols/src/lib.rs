mod builtins;
mod collector;
mod compatibility;
mod def_map;
mod dossier;
mod facts;
mod ids;
#[doc(hidden)]
pub mod perf_api;
#[cfg(test)]
mod perf_tests;
mod project;
mod resolver;
mod routine_analysis;
mod scope;
mod semantic;
mod semantic_queries;
mod validate;

pub use builtins::{
    BuiltinRoutineParamSpec, BuiltinRoutineSpec, builtin_routine_spec,
    builtin_structure_field_description, well_known_external_structure_field_description,
    well_known_external_structure_field_type,
};
pub use compatibility::{call_section_matches_parameter, parameter_is_required};
pub use def_map::{
    AssignmentSiteData, CallArgumentData, CallSiteData, ClassInheritanceData, ClassMemberData,
    ClassMemberKind, ClassMemberParameterData, Diagnostic, DiagnosticKind, ExpressionFactData,
    ExpressionFactKind, FieldAccess, FieldAccessSegment, FieldTypeRefData, FormParameterData,
    FormParameterPassingKind, FormParameterSection, FormRoutineData, FunctionModuleData,
    FunctionModuleExceptionData, FunctionModuleParameterData, FunctionModuleParameterSection,
    ImplementedInterfaceData, IncludeEdge, MemberAliasData, MethodParameterSection,
    NamedArgumentAccess, NamedArgumentSection, NamedArgumentTarget, PerformArgumentData,
    PerformCallData, PerformParameterSection, ReferenceData, ReferenceKind, Resolution,
    SqlNameRefData, SqlNameRefKind, SqlPredicateData, SqlPredicateKind, SqlProjectionData,
    SqlProjectionKind, SqlQueryData, SqlResolution, SqlSourceData, SqlSourceKind, SqlTargetData,
    SqlTargetKind, StructureData, StructureFieldData, StructureFieldInfo, StructureFieldShape,
    SymbolData, SymbolKind, TypeFactData, UnitAnalysis, ValueFlowEdgeData, ValueFlowKind,
    ValueFlowTargetData, Visibility,
};
pub use dossier::*;
pub use ids::{ReferenceId, ScopeId, StructureId, SymbolHandle, SymbolId, UnitId};
pub use project::{
    ProjectAnalysis, ProjectInput, analyze_project, analyze_project_from_units, analyze_unit,
    analyze_unit_locally,
};
pub use routine_analysis::{
    BlockDataflowSummary, DataflowValueId, DataflowValueKind, InstructionDataflowSummary,
    ProjectRoutineAnalysis, ProjectRoutineAnalysisMetrics, RoutineAnalysis, RoutineBlock,
    RoutineBlockId, RoutineBlockKind, RoutineCfg, RoutineDataflowInputs, RoutineDataflowResult,
    RoutineDataflowValue, RoutineDescriptor, RoutineEdge, RoutineEdgeKind, RoutineId,
    RoutineInstrId, RoutineInstruction, RoutineInstructionKind, RoutineInstructionSite, RoutineIr,
    RoutineKind, build_project_routine_analysis,
};
pub use scope::{Namespace, ScopeData, ScopeKind};
pub use semantic_queries::SemanticQueries;

#[cfg(test)]
mod tests {
    use abap_parser::parse;
    use std::collections::HashMap;
    use std::sync::Arc;

    use super::{Namespace, ReferenceKind, SymbolKind, analyze_unit};
    use crate::ids::{ScopeId, UnitId};
    use crate::project::{ProjectAnalysis, analyze_unit_locally};
    use crate::resolver::build_scope_index;
    use crate::validate::validate_project_with_scope_indexes;

    #[test]
    fn builtin_syst_field_descriptions_are_registered() {
        let subrc = super::builtin_structure_field_description("syst", "subrc").expect("subrc");
        assert!(
            subrc.contains("Return code") || subrc.contains("return code"),
            "{subrc}"
        );
        assert!(super::builtin_structure_field_description("syst", "msgv1").is_some());
        assert!(super::builtin_structure_field_description("syst", "zonlo").is_some());
        assert!(
            super::well_known_external_structure_field_description("bapiret2", "type").is_some()
        );
        assert!(
            super::well_known_external_structure_field_description("bapiret2", "message_v1")
                .is_some()
        );
        assert!(super::builtin_structure_field_description("nope", "subrc").is_none());
    }

    #[test]
    fn collects_definitions_and_references() {
        let src = "DATA lv_value TYPE i. lv_value = lv_value + 1.";
        let parsed = parse(src);
        let unit = analyze_unit("file:///demo.abap", src, &parsed);

        assert!(unit.symbols.iter().any(
            |symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "lv_value"
        ));
        assert!(unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Value
                && reference.name.as_ref() == "lv_value"
                && reference.resolution.is_some()
        }));
    }

    #[test]
    fn event_block_uses_full_hyphenated_keyword_as_symbol_name() {
        let src = "START-OF-SELECTION.\n  DATA lv TYPE i.\n";
        let parsed = parse(src);
        let unit = analyze_unit("file:///event.abap", src, &parsed);

        let event = unit
            .symbols
            .iter()
            .find(|symbol| symbol.kind == SymbolKind::Event)
            .expect("event symbol");
        assert_eq!(event.name.as_ref(), "start-of-selection");
        assert_eq!(&src[event.decl_range.clone()], "START-OF-SELECTION");
    }

    #[test]
    fn catch_clause_collects_exception_type_and_into_target_refs() {
        let src = r#"
CLASS cx_demo DEFINITION.
ENDCLASS.

FORM run.
  DATA lo_error TYPE REF TO cx_demo.
  TRY.
    WRITE 'x'.
  CATCH cx_demo INTO lo_error ##no_handler.
    WRITE 'y'.
  ENDTRY.
ENDFORM.
"#;
        let parsed = parse(src);
        let unit = analyze_unit("file:///catch_refs.abap", src, &parsed);

        let cx_refs: Vec<_> = unit
            .references
            .iter()
            .filter(|reference| {
                reference.kind == ReferenceKind::TypeRef
                    && reference.namespace == Namespace::Type
                    && reference.name.as_ref() == "cx_demo"
                    && reference.resolution.is_some()
            })
            .collect();
        assert!(
            cx_refs.len() >= 2,
            "expected type refs from DATA and CATCH, got: {:?}",
            unit.references
        );

        assert!(unit.references.iter().any(|reference| {
            reference.kind == ReferenceKind::Identifier
                && reference.namespace == Namespace::Value
                && reference.name.as_ref() == "lo_error"
                && reference.resolution.is_some()
        }));
    }

    #[test]
    fn catch_clause_inline_data_declares_typed_exception_target() {
        let src = r#"
CLASS cx_demo DEFINITION INHERITING FROM cx_root.
ENDCLASS.

FORM run.
  TRY.
    WRITE 'x'.
  CATCH cx_demo INTO DATA(lo_error) ##no_handler.
    lo_error->get_text( ).
  ENDTRY.
ENDFORM.
"#;
        let parsed = parse(src);
        let unit = analyze_unit("file:///catch_inline.abap", src, &parsed);

        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "lo_error"
            })
            .expect("inline catch symbol");
        let declared_type = symbol
            .declared_type
            .as_ref()
            .expect("declared type for inline catch symbol");
        assert_eq!(declared_type.namespace, Namespace::Type);
        assert!(declared_type.is_ref);
        assert_eq!(declared_type.base_name.as_ref(), "cx_demo");

        assert!(unit.references.iter().any(|reference| {
            reference.kind == ReferenceKind::Identifier
                && reference.namespace == Namespace::Value
                && reference.name.as_ref() == "lo_error"
                && reference.resolution.is_some()
        }));
    }

    #[test]
    fn dereference_after_structure_component_does_not_raise_unknown_field() {
        let src = r#"
TYPES: BEGIN OF ty_xmlparse,
         xi_data TYPE REF TO data,
       END OF ty_xmlparse.

DATA ls_xmlparse TYPE ty_xmlparse.
FIELD-SYMBOLS <ls_raw_data> TYPE any.

ASSIGN ls_xmlparse-xi_data->* TO <ls_raw_data>.
"#;
        let parsed = parse(src);
        let unit = analyze_unit("file:///selector_deref_struct_field.abap", src, &parsed);

        assert!(
            unit.diagnostics
                .iter()
                .all(|diagnostic| !diagnostic.message.contains("unknown field '*'")),
            "{:?}",
            unit.diagnostics
        );
    }

    #[test]
    fn sql_name_ref_query_finds_narrowest_match_at_offset() {
        let src = "SELECT carrid FROM scarr INTO TABLE @DATA(lt_scarr).";
        let parsed = parse(src);
        let unit = analyze_unit("file:///sql_query.abap", src, &parsed);
        let semantic = unit.semantic();

        let source_offset = src.find("scarr").expect("sql source");
        let sql_ref = semantic
            .sql()
            .name_ref_at_offset(source_offset)
            .expect("sql name ref at source offset");

        assert_eq!(sql_ref.kind, super::SqlNameRefKind::Source);
        assert_eq!(sql_ref.name.as_ref(), "scarr");
    }

    #[test]
    fn reference_queries_find_resolved_symbol_uses() {
        let src = "DATA lv_value TYPE i. lv_value = lv_value + 1.";
        let parsed = parse(src);
        let unit = analyze_unit("file:///refs_query.abap", src, &parsed);
        let semantic = unit.semantic();

        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "lv_value"
            })
            .expect("lv_value symbol");

        let use_offset = src.rfind("lv_value").expect("last lv_value");
        let reference = semantic
            .refs()
            .reference_at_offset(use_offset)
            .expect("reference at use offset");
        assert_eq!(reference.name.as_ref(), "lv_value");

        let refs: Vec<_> = semantic
            .refs()
            .resolving_to(super::SymbolHandle {
                unit: unit.unit_id,
                symbol: symbol.id,
            })
            .collect();
        assert_eq!(refs.len(), 2);
        assert!(
            refs.iter()
                .all(|reference| reference.name.as_ref() == "lv_value")
        );
    }

    #[test]
    fn dd_like_queries_find_type_refs_and_sql_sources_by_name() {
        let src = "TYPES ty_scarr TYPE scarr. SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).";
        let parsed = parse(src);
        let unit = analyze_unit("file:///dd_like_query.abap", src, &parsed);
        let semantic = unit.semantic();

        let type_offset = src.find("TYPE scarr").expect("type ref") + "TYPE ".len();
        let type_ref = semantic
            .refs()
            .type_reference_at_offset(type_offset)
            .expect("type reference at offset");
        assert_eq!(type_ref.kind, ReferenceKind::TypeRef);
        assert_eq!(type_ref.name.as_ref(), "scarr");

        let type_refs: Vec<_> = semantic.refs().type_named("SCARR").collect();
        assert_eq!(type_refs.len(), 1);
        assert!(semantic.sql().has_source_named("scarr"));

        let sql_sources: Vec<_> = semantic.sql().source_name_refs_named("SCARR").collect();
        assert_eq!(sql_sources.len(), 1);
        assert_eq!(sql_sources[0].kind, super::SqlNameRefKind::Source);
    }

    #[test]
    fn symbol_queries_find_symbols_and_class_members_at_offset() {
        let src = r#"
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

DATA gv_value TYPE i.
"#;
        let parsed = parse(src);
        let unit = analyze_unit("file:///symbol_query.abap", src, &parsed);
        let semantic = unit.semantic();

        let method_offset = src.find("run").expect("method name");
        let member = semantic
            .decls()
            .class_member_at_offset(method_offset)
            .expect("class member at offset");
        assert_eq!(member.kind, super::ClassMemberKind::Method);
        assert_eq!(member.name.as_ref(), "run");

        let global_offset = src.find("gv_value").expect("global variable");
        let global = semantic
            .decls()
            .symbol_at_offset(global_offset)
            .expect("global symbol at offset");
        assert_eq!(global.kind, SymbolKind::Variable);
        assert_eq!(global.name.as_ref(), "gv_value");

        let by_range = semantic
            .decls()
            .symbol_with_kind_and_decl_range(SymbolKind::Variable, &global.decl_range)
            .expect("symbol by range");
        assert_eq!(by_range.id, global.id);
    }

    #[test]
    fn interfaces_statement_collects_type_references_in_class_and_interface_defs() {
        let src = r#"
INTERFACE zif_parent.
ENDINTERFACE.

INTERFACE zif_child.
  INTERFACES zif_parent.
ENDINTERFACE.

CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_child.
ENDCLASS.
"#;
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///interfaces_stmt.abap", src, &parsed);
        let refs: Vec<_> = unit
            .references
            .iter()
            .filter(|reference| {
                reference.kind == ReferenceKind::TypeRef
                    && reference.namespace == Namespace::Type
                    && matches!(reference.name.as_ref(), "zif_parent" | "zif_child")
            })
            .collect();
        assert_eq!(refs.len(), 2, "{refs:#?}");
        assert!(refs.iter().all(|reference| reference.resolution.is_some()));
    }

    #[test]
    fn structure_field_query_finds_field_at_offset() {
        let src = r#"
TYPES: BEGIN OF ty_demo,
         comp TYPE i,
       END OF ty_demo.
"#;
        let parsed = parse(src);
        let unit = analyze_unit("file:///struct_field_query.abap", src, &parsed);
        let semantic = unit.semantic();

        let field_offset = src.find("comp").expect("field name");
        let field = semantic
            .decls()
            .structure_field_at_offset(field_offset)
            .expect("structure field at offset");
        assert_eq!(field.name.as_ref(), "comp");
        assert!(field.decl_range.is_some());
    }

    #[test]
    fn semantic_facade_exposes_query_surface() {
        let src = "DATA lv_value TYPE i. lv_value = lv_value + 1.";
        let parsed = parse(src);
        let unit = analyze_unit("file:///semantic_facade.abap", src, &parsed);
        let semantic = unit.semantic();

        let decl_offset = src.find("lv_value").expect("decl");
        let use_offset = src.rfind("lv_value").expect("use");

        let symbol = semantic
            .decls()
            .symbol_at_offset(decl_offset)
            .expect("symbol at decl");
        assert_eq!(symbol.name.as_ref(), "lv_value");

        let reference = semantic
            .refs()
            .reference_at_offset(use_offset)
            .expect("ref at use");
        assert_eq!(reference.name.as_ref(), "lv_value");

        let refs: Vec<_> = semantic
            .refs()
            .resolving_to(super::SymbolHandle {
                unit: unit.unit_id,
                symbol: symbol.id,
            })
            .collect();
        assert_eq!(refs.len(), 2);
    }

    #[test]
    fn semantic_facade_supports_domain_slices() {
        let src = "TYPES ty_scarr TYPE scarr. SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).";
        let parsed = parse(src);
        let unit = analyze_unit("file:///semantic_slices.abap", src, &parsed);
        let semantic = unit.semantic();

        let type_offset = src.find("TYPE scarr").expect("type ref") + "TYPE ".len();
        assert!(
            semantic
                .refs()
                .type_reference_at_offset(type_offset)
                .is_some()
        );
        assert!(semantic.sql().has_source_named("scarr"));

        let source_offset = src.find("FROM scarr").expect("sql source") + "FROM ".len();
        assert!(semantic.sql().name_ref_at_offset(source_offset).is_some());
        assert!(semantic.decls().symbol_at_offset(type_offset).is_none());
    }

    #[test]
    fn validation_ignores_out_of_range_scope_ids_in_analyzed_data() {
        let src = "DATA lv_value TYPE i. lv_value = lv_value + 1.";
        let parsed = parse(src);
        let mut unit = analyze_unit_locally(UnitId(0), "file:///stale_scope.abap", src, &parsed);
        unit.references[0].scope = ScopeId(999);

        let scope_indexes = vec![build_scope_index(&unit)];
        let uri = Arc::<str>::from("file:///stale_scope.abap");
        let mut project = ProjectAnalysis {
            units: vec![unit],
            uri_to_unit: HashMap::from([(Arc::clone(&uri), UnitId(0))]),
            provided_name_to_unit: HashMap::new(),
            diagnostics: Vec::new(),
        };

        validate_project_with_scope_indexes(&mut project, &scope_indexes);
    }

    #[test]
    fn validation_accepts_interface_selector_when_only_qualified_member_is_available() {
        let src = r#"
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.

CLASS super DEFINITION.
  PUBLIC SECTION.
    INTERFACES i1.
ENDCLASS.

CLASS super IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
ENDCLASS.

CLASS sub DEFINITION INHERITING FROM super.
  PUBLIC SECTION.
    METHODS i1~meth REDEFINITION.
ENDCLASS.

CLASS sub IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
ENDCLASS.

DATA lo_obj TYPE REF TO sub.
lo_obj->i1~meth( ).
"#;
        let parsed = parse(src);
        let mut unit = analyze_unit_locally(
            UnitId(0),
            "file:///qualified_member_only.abap",
            src,
            &parsed,
        );
        unit.implemented_interfaces.clear();

        let scope_indexes = vec![build_scope_index(&unit)];
        let uri = Arc::<str>::from("file:///qualified_member_only.abap");
        let mut project = ProjectAnalysis {
            units: vec![unit],
            uri_to_unit: HashMap::from([(Arc::clone(&uri), UnitId(0))]),
            provided_name_to_unit: HashMap::new(),
            diagnostics: Vec::new(),
        };

        validate_project_with_scope_indexes(&mut project, &scope_indexes);

        assert!(
            !project.units[0].diagnostics.iter().any(|diag| {
                diag.kind == super::DiagnosticKind::UnknownField
                    && (diag.message.contains("unknown member 'i1' for class 'sub'")
                        || diag.message.contains("unknown member 'meth'"))
            }),
            "{:#?}",
            project.units[0].diagnostics
        );
    }
}
