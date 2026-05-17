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
mod static_analysis;
mod validate;

pub use builtins::{
    BuiltinRoutineParamSpec, BuiltinRoutineSpec, builtin_routine_spec,
    builtin_structure_field_description, well_known_external_structure_field_description,
    well_known_external_structure_field_type,
};
pub use compatibility::{call_section_matches_parameter, parameter_is_required};
pub use def_map::{
    AssignmentSiteData, CallArgumentData, CallSiteData, CaseRegionData, ClassDefinitionData,
    ClassInheritanceData, ClassMemberData, ClassMemberImplementationData, ClassMemberKind,
    ClassMemberParameterData, ConstructorForBindingData, Diagnostic, DiagnosticKind,
    ExpressionFactData, ExpressionFactKind, FieldAccess, FieldAccessSegment,
    FieldSymbolStateCheckData, FieldSymbolStateCheckKind, FieldTypeRefData, FormParameterData,
    FormParameterPassingKind, FormParameterSection, FormRoutineData, FunctionModuleData,
    FunctionModuleExceptionData, FunctionModuleParameterData, FunctionModuleParameterSection,
    IfRegionData, ImplementedInterfaceData, IncludeEdge, InternalTableOrderData, LoopRegionData,
    MemberAliasData, MessageClassEntryData, MessageClassUseData, MessageUseData,
    MethodParameterSection, NamedArgumentAccess, NamedArgumentSection, NamedArgumentTarget,
    PerformArgumentData, PerformCallData, PerformParameterSection, PerformProgramData,
    ReadTableBinarySearchData, ReferenceData, ReferenceKind, Resolution, RoutineControlRegionData,
    RoutineLoopKind, RoutineSiteData, RoutineSiteKind, SqlDynamicFragmentData,
    SqlDynamicFragmentKind, SqlNameRefData, SqlNameRefKind, SqlPredicateData, SqlPredicateKind,
    SqlProjectionData, SqlProjectionKind, SqlQueryData, SqlResolution, SqlSourceData,
    SqlSourceKind, SqlTargetData, SqlTargetKind, StructureData, StructureFieldData,
    StructureFieldInfo, StructureFieldShape, SymbolData, SymbolKind, SystemFieldStatementKind,
    SystemFieldUpdateData, TryRegionData, TypeFactData, UnitAnalysis, ValueFlowEdgeData,
    ValueFlowKind, ValueFlowTargetData, ValueStateCheckData, ValueStateCheckKind, Visibility,
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
    RoutineBlockId, RoutineBlockKind, RoutineBranchKind, RoutineCfg, RoutineDataflowInputs,
    RoutineDataflowResult, RoutineDataflowValue, RoutineDescriptor, RoutineEdge, RoutineEdgeKind,
    RoutineId, RoutineInstrId, RoutineInstruction, RoutineInstructionKind, RoutineInstructionSite,
    RoutineIr, RoutineKind, RoutineTerminatorKind, build_project_routine_analysis,
    build_project_routine_analysis_for_units,
};
pub use scope::{Namespace, ScopeData, ScopeKind};
pub use semantic_queries::SemanticQueries;
pub use static_analysis::{
    ProjectStaticAnalysisSummary, ProjectStaticAnalysisSummaryMetrics,
    RoutineStaticAnalysisFindingCounts, RoutineStaticAnalysisSummary, StaticAnalysisFinding,
    StaticAnalysisFindingKind, build_project_static_analysis_summary,
};

#[cfg(test)]
mod tests {
    use abap_parser::parse;
    use std::collections::HashMap;
    use std::sync::Arc;

    use super::{
        DiagnosticKind, NamedArgumentTarget, Namespace, ProjectInput, ReferenceKind, Resolution,
        RoutineInstructionKind, RoutineInstructionSite, RoutineTerminatorKind, ScopeKind,
        SymbolKind, analyze_project, analyze_project_from_units, analyze_unit,
        build_project_routine_analysis,
    };
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
        assert!(super::builtin_structure_field_description("syst", "abcde").is_some());
        assert!(super::builtin_structure_field_description("syst", "fdpos").is_some());
        assert!(super::builtin_structure_field_description("syst", "msgv1").is_some());
        assert!(super::builtin_structure_field_description("syst", "tvar9").is_some());
        let xform = super::builtin_structure_field_description("syst", "xform").expect("xform");
        assert_eq!(xform, "ABAP System Field: Internal Use");
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
    fn dynpro_screen_support_resolves_and_affects_routine_analysis() {
        let src = r#"
REPORT z_screen_demo.

DATA ls_screen TYPE screen.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN INTO ls_screen.
    IF ls_screen-name = 'P_FOO'.
      ls_screen-input = 0.
      MODIFY SCREEN FROM ls_screen.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.
  CALL SCREEN 9000.

MODULE status_9000 OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_BAR'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  LEAVE TO SCREEN 0.
ENDMODULE.
"#;

        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let project = analyze_project_from_units(vec![analyze_unit(
            "file:///screen_demo.abap",
            src,
            &parsed,
        )]);
        let unit = &project.units[0];

        let screen_type = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::BuiltinType && symbol.name.as_ref() == "screen"
            })
            .expect("builtin screen type");
        assert!(screen_type.structure.is_some());

        let screen_var = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::BuiltinVariable && symbol.name.as_ref() == "screen"
            })
            .expect("builtin screen variable");
        assert!(screen_var.structure.is_some());

        let ls_screen = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "ls_screen"
            })
            .expect("ls_screen");
        let ls_screen_structure = ls_screen
            .structure
            .and_then(|id| unit.structures.get(id.as_usize()))
            .expect("ls_screen structure");
        assert_eq!(ls_screen_structure.name.as_ref(), "screen");

        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.name.as_ref() == "screen"
                    && reference.resolution.is_some()
            }),
            "{:#?}",
            unit.references
        );
        assert!(
            unit.diagnostics.iter().all(|diagnostic| {
                !diagnostic.message.contains("unknown symbol 'screen'")
                    && !diagnostic.message.contains("unknown field 'name'")
                    && !diagnostic.message.contains("unknown field 'input'")
            }),
            "{:#?}",
            unit.diagnostics
        );

        let routine_analysis = build_project_routine_analysis(&project);
        let start_of_selection = routine_analysis
            .routines
            .iter()
            .find(|routine| routine.descriptor.name.as_ref() == "start-of-selection")
            .expect("start-of-selection routine");
        assert!(
            start_of_selection
                .ir
                .instructions
                .iter()
                .any(|instruction| {
                    instruction.kind() == RoutineInstructionKind::UnknownEffect
                        && src[instruction.range.clone()].contains("CALL SCREEN")
                }),
            "{:#?}",
            start_of_selection.ir.instructions
        );

        let status_9000 = routine_analysis
            .routines
            .iter()
            .find(|routine| routine.descriptor.name.as_ref() == "status_9000")
            .expect("status_9000 routine");
        assert!(
            status_9000.ir.instructions.iter().any(|instruction| {
                matches!(
                    instruction.site,
                    RoutineInstructionSite::Terminator {
                        kind: RoutineTerminatorKind::Leave
                    }
                ) && src[instruction.range.clone()].contains("LEAVE TO SCREEN")
            }),
            "{:#?}",
            status_9000.ir.instructions
        );
    }

    #[test]
    fn stop_statement_is_event_block_terminator() {
        let src = "\
START-OF-SELECTION.\n\
  STOP.\n\
  WRITE 'unreachable'.\n\
END-OF-SELECTION.\n\
  WRITE 'done'.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///stop_statement.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);
        let start_of_selection = routine_analysis
            .routines
            .iter()
            .find(|routine| routine.descriptor.name.as_ref() == "start-of-selection")
            .expect("start-of-selection routine");

        assert!(
            start_of_selection
                .ir
                .instructions
                .iter()
                .any(|instruction| {
                    matches!(
                        instruction.site,
                        RoutineInstructionSite::Terminator {
                            kind: RoutineTerminatorKind::Stop
                        }
                    ) && src[instruction.range.clone()].contains("STOP")
                }),
            "{:#?}",
            start_of_selection.ir.instructions
        );
        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .any(|diagnostic| {
                    diagnostic.kind == DiagnosticKind::UnreachableCode
                        && src[diagnostic.range.clone()].contains("WRITE 'unreachable'")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
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
    fn at_group_key_resolves_against_loop_row_fields() {
        let src = "\
TYPES: BEGIN OF ty_row,\n\
         a TYPE i,\n\
       END OF ty_row.\n\
FORM run.\n\
  DATA itab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.\n\
  LOOP AT itab INTO DATA(ls_row).\n\
    AT NEW a.\n\
      WRITE ls_row-a.\n\
    ENDAT.\n\
  ENDLOOP.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///at_group_key.abap", src, &parsed);

        let header_offset = src.find("AT NEW a").expect("AT NEW header") + "AT NEW ".len();
        let header_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(header_offset)
            .expect("AT header reference");
        assert_eq!(header_ref.name.as_ref(), "a");
        assert!(matches!(header_ref.resolution, Some(Resolution::Symbol(_))));
        assert!(
            unit.scopes
                .iter()
                .any(|scope| scope.kind == ScopeKind::AtBlock)
        );
        assert!(
            unit.diagnostics
                .iter()
                .all(|diagnostic| !diagnostic.message.contains("unknown symbol 'a'")),
            "{:#?}",
            unit.diagnostics
        );
    }

    #[test]
    fn at_group_keys_resolve_for_loop_assigning_field_symbols() {
        let src = "\
TYPES: BEGIN OF ty_row,\n\
         src_plant TYPE i,\n\
       END OF ty_row.\n\
FORM run.\n\
  DATA itab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.\n\
  FIELD-SYMBOLS <lfs_final_data> TYPE ty_row.\n\
  LOOP AT itab ASSIGNING <lfs_final_data>.\n\
    AT NEW src_plant.\n\
      WRITE <lfs_final_data>-src_plant.\n\
    ENDAT.\n\
    AT END OF src_plant.\n\
      WRITE <lfs_final_data>-src_plant.\n\
    ENDAT.\n\
  ENDLOOP.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///at_group_assigning.abap", src, &parsed);

        let header_offsets = [
            src.find("AT NEW src_plant").expect("AT NEW header") + "AT NEW ".len(),
            src.find("AT END OF src_plant").expect("AT END OF header") + "AT END OF ".len(),
        ];
        for header_offset in header_offsets {
            let header_ref = unit
                .semantic()
                .refs()
                .reference_at_offset(header_offset)
                .expect("AT header reference");
            assert_eq!(header_ref.name.as_ref(), "src_plant");
            assert!(matches!(header_ref.resolution, Some(Resolution::Symbol(_))));
        }

        assert_eq!(
            unit.scopes
                .iter()
                .filter(|scope| scope.kind == ScopeKind::AtBlock)
                .count(),
            2
        );
        assert!(
            unit.diagnostics
                .iter()
                .all(|diagnostic| !diagnostic.message.contains("unknown symbol 'src_plant'")),
            "{:#?}",
            unit.diagnostics
        );
    }

    #[test]
    fn at_group_headers_do_not_trigger_definite_assignment_warnings() {
        let src = "\
TYPES: BEGIN OF ty_row,\n\
         src_plant TYPE i,\n\
       END OF ty_row.\n\
FORM run.\n\
  DATA itab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.\n\
  FIELD-SYMBOLS <lfs_final_data> TYPE ty_row.\n\
  LOOP AT itab ASSIGNING <lfs_final_data>.\n\
    AT NEW src_plant.\n\
      WRITE <lfs_final_data>-src_plant.\n\
    ENDAT.\n\
    AT END OF src_plant.\n\
      WRITE <lfs_final_data>-src_plant.\n\
    ENDAT.\n\
  ENDLOOP.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///at_group_header_warnings.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        let header_ranges = [
            {
                let start = src.find("AT NEW src_plant").expect("AT NEW header") + "AT NEW ".len();
                start..start + "src_plant".len()
            },
            {
                let start =
                    src.find("AT END OF src_plant").expect("AT END OF header") + "AT END OF ".len();
                start..start + "src_plant".len()
            },
        ];
        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !header_ranges.contains(&diagnostic.range)
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn value_for_in_iterator_is_definitely_assigned() {
        let src = r#"
FORM process_reload.
  TYPES: BEGIN OF ty_job,
           job_status TYPE c LENGTH 1,
           jobname TYPE string,
         END OF ty_job.
  TYPES tt_jobs TYPE STANDARD TABLE OF ty_job WITH EMPTY KEY.
  TYPES: BEGIN OF ty_range,
           sign TYPE c LENGTH 1,
           option TYPE c LENGTH 2,
           low TYPE string,
           high TYPE string,
         END OF ty_range.
  TYPES tt_range TYPE STANDARD TABLE OF ty_range WITH EMPTY KEY.
  CONSTANTS lc_status_a TYPE c LENGTH 1 VALUE 'A'.
  CONSTANTS lc_sign_i TYPE c LENGTH 1 VALUE 'I'.
  CONSTANTS lc_opt_eq TYPE c LENGTH 2 VALUE 'EQ'.
  DATA lt_rel_data TYPE tt_jobs.
  DATA lr_jobname TYPE tt_range.

  lr_jobname = VALUE #(
    FOR ls_jobs IN lt_rel_data
    WHERE ( job_status = lc_status_a )
    ( sign = lc_sign_i
      option = lc_opt_eq
      low = ls_jobs-jobname
      high = '' ) ).
ENDFORM.
"#;
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///value_for_iterator_assignment.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !diagnostic.message.contains("ls_jobs")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn unknown_importing_inline_call_argument_is_definitely_assigned() {
        let src = "\
CLASS lcl_demo DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS get_data.\n\
ENDCLASS.\n\
\n\
CLASS lcl_demo IMPLEMENTATION.\n\
  METHOD get_data.\n\
    /sttp/cl_rr_ru_utilities=>get_safedata_key(\n\
      IMPORTING\n\
        ev_key = DATA(lv_content_key2)\n\
    ).\n\
\n\
    /sttp/cl_safe_data=>get_data(\n\
      EXPORTING\n\
        iv_content_key = lv_content_key2\n\
    ).\n\
  ENDMETHOD.\n\
ENDCLASS.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///unknown_importing_inline.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        let read_start = src
            .find("iv_content_key = lv_content_key2")
            .expect("content key read")
            + "iv_content_key = ".len();
        let read_range = read_start..read_start + "lv_content_key2".len();

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || diagnostic.range != read_range
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn call_transformation_result_inline_data_is_definitely_assigned() {
        let src = "\
FORM process_reload.\n\
  DATA lt_objects_email TYPE string.\n\
  lt_objects_email = 'x'.\n\
  CALL TRANSFORMATION id SOURCE root = lt_objects_email\n\
                         RESULT XML DATA(lv_xstring).\n\
  DATA(lv_copy) = lv_xstring.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///call_transformation_inline.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !diagnostic.message.contains("lv_xstring")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn message_into_target_is_definitely_assigned() {
        let src = "\
FORM get_data.\n\
  CONSTANTS lc_rfc_dest TYPE string VALUE 'RFC'.\n\
  MESSAGE e102(/sttp/rep_msg_ru) WITH lc_rfc_dest INTO DATA(lv_dummy_msg).\n\
  MESSAGE lv_dummy_msg TYPE 'S'.\n\
\n\
  DATA lv_existing_msg TYPE string.\n\
  MESSAGE 'existing' TYPE 'S' INTO lv_existing_msg.\n\
  MESSAGE lv_existing_msg TYPE 'S'.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///message_into_assignment.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        for name in ["lv_dummy_msg", "lv_existing_msg"] {
            assert!(
                routine_analysis
                    .diagnostics_for_unit(unit.unit_id)
                    .iter()
                    .all(|diagnostic| {
                        diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                            || !diagnostic.message.contains(name)
                    }),
                "{:#?}",
                routine_analysis.diagnostics_for_unit(unit.unit_id)
            );
        }
    }

    #[test]
    fn implicit_me_is_definitely_assigned_in_instance_method_bodies() {
        let src = "\
CLASS lcl_demo DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS constructor IMPORTING iv_value TYPE i.\n\
    METHODS get_value RETURNING VALUE(rv_value) TYPE i.\n\
  PRIVATE SECTION.\n\
    DATA mv_value TYPE i.\n\
ENDCLASS.\n\
\n\
CLASS lcl_demo IMPLEMENTATION.\n\
  METHOD constructor.\n\
    me->mv_value = iv_value.\n\
  ENDMETHOD.\n\
\n\
  METHOD get_value.\n\
    rv_value = me->mv_value.\n\
  ENDMETHOD.\n\
ENDCLASS.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///implicit_me_dataflow.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !diagnostic.message.contains("'me'")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn implicit_me_resolves_in_instance_method_body_split_across_includes() {
        let main_src = "\
REPORT z_demo.\n\
INCLUDE z_demo_top.\n\
INCLUDE z_demo_f01.\n";
        let top_src = "\
CLASS lcl_demo DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS get_value RETURNING VALUE(rv_value) TYPE i.\n\
  PRIVATE SECTION.\n\
    DATA mv_value TYPE i.\n\
ENDCLASS.\n";
        let f01_src = "\
CLASS lcl_demo IMPLEMENTATION.\n\
  METHOD get_value.\n\
    rv_value = me->mv_value.\n\
  ENDMETHOD.\n\
ENDCLASS.\n";
        let main_parse = parse(main_src);
        let top_parse = parse(top_src);
        let f01_parse = parse(f01_src);
        let project = analyze_project(&[
            ProjectInput {
                uri: "file:///workspace/z_demo/z_demo.abap",
                source: main_src,
                parse: &main_parse,
            },
            ProjectInput {
                uri: "file:///workspace/z_demo/Includes/z_demo_top.abap",
                source: top_src,
                parse: &top_parse,
            },
            ProjectInput {
                uri: "file:///workspace/z_demo/Includes/z_demo_f01.abap",
                source: f01_src,
                parse: &f01_parse,
            },
        ]);
        let unit = project
            .unit_by_uri("file:///workspace/z_demo/Includes/z_demo_f01.abap")
            .expect("method include unit");

        assert!(
            unit.diagnostics
                .iter()
                .all(|diag| !diag.message.contains("unknown symbol 'me'")),
            "{:#?}",
            unit.diagnostics
        );
        assert!(
            unit.references.iter().any(|reference| {
                reference.name.as_ref() == "me"
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "{:#?}",
            unit.references
        );
    }

    #[test]
    fn delete_where_row_field_from_project_table_type_does_not_trigger_definite_assignment_warning()
    {
        let main_src = r#"
FORM f_bapi_outb_deliv.
  DATA t_to_display TYPE /sttp/t_to_display.
  DATA ls_ltak TYPE /sttp/s_ltak.

  ls_ltak-tanum = 1.
  DELETE t_to_display WHERE tanum = ls_ltak-tanum.
ENDFORM.
"#;
        let table_src = r#"
TYPES /sttp/t_to_display TYPE STANDARD TABLE OF /sttp/s_to_display WITH EMPTY KEY.
"#;
        let row_src = r#"
TYPES: BEGIN OF /sttp/s_to_display,
         tanum TYPE i,
       END OF /sttp/s_to_display.
"#;
        let ltak_src = r#"
TYPES: BEGIN OF /sttp/s_ltak,
         tanum TYPE i,
       END OF /sttp/s_ltak.
"#;

        let main_parse = parse(main_src);
        let table_parse = parse(table_src);
        let row_parse = parse(row_src);
        let ltak_parse = parse(ltak_src);
        let project = analyze_project(&[
            ProjectInput {
                uri: "file:///main.abap",
                source: main_src,
                parse: &main_parse,
            },
            ProjectInput {
                uri: "file:///ddic_table.abap",
                source: table_src,
                parse: &table_parse,
            },
            ProjectInput {
                uri: "file:///ddic_row.abap",
                source: row_src,
                parse: &row_parse,
            },
            ProjectInput {
                uri: "file:///ddic_ltak.abap",
                source: ltak_src,
                parse: &ltak_parse,
            },
        ]);
        let unit = project.unit_by_uri("file:///main.abap").expect("main unit");
        let routine_analysis = build_project_routine_analysis(&project);

        let first_tanum_start = main_src
            .find("tanum = ls_ltak-tanum")
            .expect("DELETE WHERE field");
        let first_tanum_range = first_tanum_start..first_tanum_start + "tanum".len();
        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || diagnostic.range != first_tanum_range
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn delete_where_field_resolves_against_prior_include_table_type_when_include_is_target() {
        let main_src = "\
REPORT z_demo.
INCLUDE z_demo_top.
INCLUDE z_demo_f01.
";
        let top_src = "\
TYPES: BEGIN OF ty_b2p_outs,
         sgtin TYPE string,
         objid TYPE string,
       END OF ty_b2p_outs,
       tt_b2p_outs TYPE STANDARD TABLE OF ty_b2p_outs WITH EMPTY KEY.
DATA gt_b2p_outs TYPE tt_b2p_outs.
";
        let f01_src = "\
FORM get_data.
  DELETE gt_b2p_outs WHERE sgtin = ''.
ENDFORM.
";
        let main_parse = parse(main_src);
        let top_parse = parse(top_src);
        let f01_parse = parse(f01_src);
        let project = analyze_project(&[
            ProjectInput {
                uri: "file:///workspace/z_demo/z_demo.abap",
                source: main_src,
                parse: &main_parse,
            },
            ProjectInput {
                uri: "file:///workspace/z_demo/Includes/z_demo_top.abap",
                source: top_src,
                parse: &top_parse,
            },
            ProjectInput {
                uri: "file:///workspace/z_demo/Includes/z_demo_f01.abap",
                source: f01_src,
                parse: &f01_parse,
            },
        ]);
        let unit = project
            .unit_by_uri("file:///workspace/z_demo/Includes/z_demo_f01.abap")
            .expect("include unit");

        assert!(
            unit.diagnostics
                .iter()
                .all(|diag| !diag.message.contains("unknown symbol 'sgtin'")),
            "{:#?}",
            unit.diagnostics
        );
    }

    #[test]
    fn at_group_processing_branches_affect_dataflow() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           a TYPE i,\n\
         END OF ty_row.\n\
  TYPES: BEGIN OF ty_state,\n\
           x TYPE i,\n\
         END OF ty_state.\n\
  DATA itab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.\n\
  DATA ls_state TYPE ty_state.\n\
  DATA lv_sink TYPE i.\n\
  LOOP AT itab INTO DATA(ls_row).\n\
    AT END OF a.\n\
      ls_state-x = 1.\n\
    ENDAT.\n\
    lv_sink = ls_state-x.\n\
  ENDLOOP.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///at_group_dataflow.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .any(|diagnostic| {
                    diagnostic.kind == DiagnosticKind::UseBeforeDefiniteAssignment
                        && src[diagnostic.range.clone()].contains("ls_state")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );

        let src_without_at = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           a TYPE i,\n\
         END OF ty_row.\n\
  TYPES: BEGIN OF ty_state,\n\
           x TYPE i,\n\
         END OF ty_state.\n\
  DATA itab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.\n\
  DATA ls_state TYPE ty_state.\n\
  DATA lv_sink TYPE i.\n\
  LOOP AT itab INTO DATA(ls_row).\n\
    ls_state-x = 1.\n\
    lv_sink = ls_state-x.\n\
  ENDLOOP.\n\
ENDFORM.\n";
        let parsed_without_at = parse(src_without_at);
        assert!(
            parsed_without_at.errors.is_empty(),
            "{:?}",
            parsed_without_at.errors
        );
        let unit_without_at = analyze_unit(
            "file:///at_group_dataflow_no_at.abap",
            src_without_at,
            &parsed_without_at,
        );
        let project_without_at = analyze_project_from_units(vec![unit_without_at.clone()]);
        let routine_analysis_without_at = build_project_routine_analysis(&project_without_at);

        assert!(
            routine_analysis_without_at
                .diagnostics_for_unit(unit_without_at.unit_id)
                .iter()
                .all(|diagnostic| diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment),
            "{:#?}",
            routine_analysis_without_at.diagnostics_for_unit(unit_without_at.unit_id)
        );
    }

    #[test]
    fn open_sql_into_target_does_not_trigger_definite_assignment_warning() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  SELECT SINGLE carrid\n\
    FROM scarr\n\
    INTO ls_row.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///open_sql_into_target.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        let target_start = src.find("ls_row.").expect("SELECT INTO target");
        let target_range = target_start..target_start + "ls_row".len();
        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || diagnostic.range != target_range
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_into_target_remains_possibly_unassigned_without_success_guard() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lv_copy TYPE scarr-carrid.\n\
  SELECT SINGLE carrid\n\
    FROM scarr\n\
    INTO ls_row.\n\
  lv_copy = ls_row-carrid.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///open_sql_into_unguarded.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .any(|diagnostic| {
                    diagnostic.kind == DiagnosticKind::UseBeforeDefiniteAssignment
                        && src[diagnostic.range.clone()].contains("ls_row")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_into_target_is_definitely_assigned_after_sy_subrc_equals_zero_guard() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lv_copy TYPE scarr-carrid.\n\
  SELECT SINGLE carrid\n\
    FROM scarr\n\
    INTO ls_row.\n\
  IF sy-subrc = 0.\n\
    lv_copy = ls_row-carrid.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///open_sql_into_guarded_eq.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_into_target_is_definitely_assigned_after_sy_subrc_is_initial_guard() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lv_copy TYPE scarr-carrid.\n\
  SELECT SINGLE carrid\n\
    FROM scarr\n\
    INTO ls_row.\n\
  IF sy-subrc IS INITIAL.\n\
    lv_copy = ls_row-carrid.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///open_sql_into_guarded_initial.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_endselect_into_target_is_definitely_assigned_after_sy_subrc_is_initial_guard() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
           connid TYPE sflight-connid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lv_copy TYPE scarr-carrid.\n\
  SELECT carrid\n\
         connid\n\
    FROM sflight\n\
    UP TO 1 ROWS\n\
    INTO ls_row.\n\
  ENDSELECT.\n\
  IF sy-subrc IS INITIAL.\n\
    lv_copy = ls_row-carrid.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///open_sql_endselect_guarded_initial.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_into_target_is_safe_after_is_not_initial_guard() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lv_copy TYPE scarr-carrid.\n\
  SELECT SINGLE carrid\n\
    FROM scarr\n\
    INTO ls_row.\n\
  IF ls_row IS NOT INITIAL.\n\
    lv_copy = ls_row-carrid.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///open_sql_into_guarded_not_initial.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_inline_table_target_is_safe_after_is_not_initial_guard_for_read_table() {
        let src = "\
FORM run.\n\
  SELECT param_name, param_value\n\
    FROM zparams\n\
    INTO TABLE @DATA(lt_parameters).\n\
  IF lt_parameters IS NOT INITIAL.\n\
    READ TABLE lt_parameters ASSIGNING FIELD-SYMBOL(<fs_buffer_time>)\n\
      WITH KEY param_name = 'TOKEN'.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///open_sql_inline_table_guarded_read_table.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !src[diagnostic.range.clone()].contains("lt_parameters")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn read_table_source_does_not_require_definite_assignment() {
        let src = "\
FORM run.\n\
  SELECT carrid\n\
    FROM scarr\n\
    INTO TABLE @DATA(lt_rows).\n\
  SORT lt_rows BY carrid.\n\
  READ TABLE lt_rows INTO DATA(lv_row) INDEX 1.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///read_table_source_table.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !src[diagnostic.range.clone()].contains("lt_rows")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn read_table_inline_sql_table_source_does_not_require_definite_assignment() {
        let src = "\
FORM run.\n\
  IF 1 = 1.\n\
    SELECT carrid\n\
      FROM scarr\n\
      INTO TABLE @DATA(lt_rows).\n\
    READ TABLE lt_rows INTO DATA(ls_row) INDEX 1.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///read_table_inline_sql_source.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !src[diagnostic.range.clone()].contains("lt_rows")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn loop_at_inline_sql_table_source_does_not_require_definite_assignment() {
        let src = "\
FORM run.\n\
  IF 1 = 1.\n\
    SELECT carrid\n\
      FROM scarr\n\
      INTO TABLE @DATA(lt_rows).\n\
    LOOP AT lt_rows INTO DATA(ls_row).\n\
    ENDLOOP.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///loop_at_inline_sql_source.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !src[diagnostic.range.clone()].contains("lt_rows")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_appending_inline_table_target_is_safe_after_negative_sy_subrc_return_guard() {
        let src = "\
FORM run.\n\
  SELECT carrid\n\
    FROM scarr\n\
    APPENDING TABLE @DATA(lt_rows).\n\
  IF sy-subrc IS NOT INITIAL.\n\
    RETURN.\n\
  ENDIF.\n\
  DATA(lv_count) = lines( lt_rows ).\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///open_sql_appending_inline_table_guarded_return.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !src[diagnostic.range.clone()].contains("lt_rows")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_inline_table_target_is_safe_after_negative_sy_subrc_or_empty_return_guard() {
        let src = "\
FORM run.\n\
  SELECT carrid\n\
    FROM scarr\n\
    INTO TABLE @DATA(lt_files).\n\
  IF sy-subrc <> 0 OR lines( lt_files ) = 0.\n\
    RETURN.\n\
  ENDIF.\n\
  DATA(lv_count) = lines( lt_files ).\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///open_sql_inline_table_guarded_subrc_or_empty.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment
                        || !src[diagnostic.range.clone()].contains("lt_files")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_into_target_is_safe_after_not_is_initial_guard() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lv_copy TYPE scarr-carrid.\n\
  SELECT SINGLE carrid\n\
    FROM scarr\n\
    INTO ls_row.\n\
  IF NOT ls_row IS INITIAL.\n\
    lv_copy = ls_row-carrid.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///open_sql_into_guarded_not_prefix_initial.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| diagnostic.kind != DiagnosticKind::UseBeforeDefiniteAssignment),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_into_target_stays_possibly_unassigned_after_sy_subrc_not_equals_zero_guard() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lv_copy TYPE scarr-carrid.\n\
  SELECT SINGLE carrid\n\
    FROM scarr\n\
    INTO ls_row.\n\
  IF sy-subrc <> 0.\n\
    lv_copy = ls_row-carrid.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///open_sql_into_guarded_ne.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .any(|diagnostic| {
                    diagnostic.kind == DiagnosticKind::UseBeforeDefiniteAssignment
                        && src[diagnostic.range.clone()].contains("ls_row")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_into_target_stays_possibly_unassigned_after_is_initial_guard() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lv_copy TYPE scarr-carrid.\n\
  SELECT SINGLE carrid\n\
    FROM scarr\n\
    INTO ls_row.\n\
  IF ls_row IS INITIAL.\n\
    lv_copy = ls_row-carrid.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///open_sql_into_guarded_is_initial.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .any(|diagnostic| {
                    diagnostic.kind == DiagnosticKind::UseBeforeDefiniteAssignment
                        && src[diagnostic.range.clone()].contains("ls_row")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn assign_field_symbol_is_bound_after_sy_subrc_equals_zero_guard() {
        let src = "\
FORM run.\n\
  FIELD-SYMBOLS <year> TYPE n.\n\
  ASSIGN sy-datlo+0(4) TO <year>.\n\
  IF sy-subrc = 0.\n\
    WRITE: / <year>.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///assign_field_symbol_guarded_eq.abap", src, &parsed);
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| diagnostic.kind != DiagnosticKind::PossiblyUnboundFieldSymbol),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn assign_field_symbol_is_bound_after_sy_subrc_is_initial_guard() {
        let src = "\
FORM run.\n\
  FIELD-SYMBOLS <year> TYPE n.\n\
  ASSIGN sy-datlo+0(4) TO <year>.\n\
  IF sy-subrc IS INITIAL.\n\
    WRITE: / <year>.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///assign_field_symbol_guarded_initial.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| diagnostic.kind != DiagnosticKind::PossiblyUnboundFieldSymbol),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn assign_field_symbol_is_bound_after_is_assigned_guard() {
        let src = "\
FORM run.\n\
  FIELD-SYMBOLS <year> TYPE n.\n\
  ASSIGN sy-datlo+0(4) TO <year>.\n\
  IF <year> IS ASSIGNED.\n\
    WRITE: / <year>.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///assign_field_symbol_guarded_assigned.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| diagnostic.kind != DiagnosticKind::PossiblyUnboundFieldSymbol),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn assign_field_symbol_is_bound_after_negative_is_assigned_guard_exits() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           text TYPE string,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lt_names TYPE STANDARD TABLE OF string WITH EMPTY KEY.\n\
  FIELD-SYMBOLS <text> TYPE string.\n\
  LOOP AT lt_names INTO DATA(lv_name).\n\
    ASSIGN COMPONENT lv_name OF STRUCTURE ls_row TO <text>.\n\
    IF NOT <text> IS ASSIGNED.\n\
      CONTINUE.\n\
    ENDIF.\n\
    WRITE: / <text>.\n\
  ENDLOOP.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///assign_field_symbol_negative_guard_exits.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::PossiblyUnboundFieldSymbol
                        || !src[diagnostic.range.clone()].contains("<text>")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn loop_assigning_field_symbol_target_is_bound_inside_loop_body() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           objid TYPE string,\n\
         END OF ty_row.\n\
  TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.\n\
  FIELD-SYMBOLS <lt_records> TYPE ty_tab.\n\
  FIELD-SYMBOLS <ls_dm_obj_ids> TYPE ty_row.\n\
  DATA lt_range_objid_for_update TYPE STANDARD TABLE OF string WITH EMPTY KEY.\n\
  LOOP AT <lt_records> ASSIGNING <ls_dm_obj_ids>.\n\
    APPEND <ls_dm_obj_ids>-objid TO lt_range_objid_for_update.\n\
  ENDLOOP.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///loop_assigning_field_symbol_target.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .all(|diagnostic| {
                    diagnostic.kind != DiagnosticKind::PossiblyUnboundFieldSymbol
                        || !src[diagnostic.range.clone()].contains("<ls_dm_obj_ids>")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn open_sql_into_target_stays_possibly_unassigned_after_irrelevant_not_initial_guard() {
        let src = "\
FORM run.\n\
  TYPES: BEGIN OF ty_row,\n\
           carrid TYPE scarr-carrid,\n\
         END OF ty_row.\n\
  DATA ls_row TYPE ty_row.\n\
  DATA lv_other TYPE scarr-carrid VALUE 'LH'.\n\
  DATA lv_copy TYPE scarr-carrid.\n\
  SELECT SINGLE carrid\n\
    FROM scarr\n\
    INTO ls_row.\n\
  IF lv_other IS NOT INITIAL.\n\
    lv_copy = ls_row-carrid.\n\
  ENDIF.\n\
ENDFORM.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit(
            "file:///open_sql_into_guarded_irrelevant_not_initial.abap",
            src,
            &parsed,
        );
        let project = analyze_project_from_units(vec![unit.clone()]);
        let routine_analysis = build_project_routine_analysis(&project);

        assert!(
            routine_analysis
                .diagnostics_for_unit(unit.unit_id)
                .iter()
                .any(|diagnostic| {
                    diagnostic.kind == DiagnosticKind::UseBeforeDefiniteAssignment
                        && src[diagnostic.range.clone()].contains("ls_row")
                }),
            "{:#?}",
            routine_analysis.diagnostics_for_unit(unit.unit_id)
        );
    }

    #[test]
    fn perform_arguments_collect_resolved_variable_references() {
        let src = "\
FORM f USING VALUE(iv_input) TYPE i CHANGING cv_text TYPE string.
  cv_text = |{ iv_input }|.
ENDFORM.

START-OF-SELECTION.
  DATA lv_input TYPE i VALUE 1.
  DATA lv_text TYPE string.
  PERFORM f USING lv_input CHANGING lv_text.
";
        let parsed = parse(src);
        let unit = analyze_unit("file:///perform_refs.abap", src, &parsed);

        let using_offset = src.rfind("lv_input").expect("perform using arg") + 1;
        let using_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(using_offset)
            .expect("using reference");
        assert_eq!(using_ref.name.as_ref(), "lv_input");
        assert!(matches!(using_ref.resolution, Some(Resolution::Symbol(_))));

        let changing_offset = src.rfind("lv_text").expect("perform changing arg") + 1;
        let changing_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(changing_offset)
            .expect("changing reference");
        assert_eq!(changing_ref.name.as_ref(), "lv_text");
        assert!(matches!(
            changing_ref.resolution,
            Some(Resolution::Symbol(_))
        ));
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
    fn at_line_selection_uses_full_event_header_as_symbol_name() {
        let src = "AT LINE-SELECTION.\n  WRITE 'x'.\n";
        let parsed = parse(src);
        let unit = analyze_unit("file:///line_event.abap", src, &parsed);

        let event = unit
            .symbols
            .iter()
            .find(|symbol| symbol.kind == SymbolKind::Event)
            .expect("event symbol");
        assert_eq!(event.name.as_ref(), "at line-selection");
        assert_eq!(&src[event.decl_range.clone()], "AT LINE-SELECTION");
    }

    #[test]
    fn class_events_collect_members_and_handler_parameters() {
        let src = "\
CLASS lcl_sender DEFINITION.\n\
  PUBLIC SECTION.\n\
    EVENTS changed EXPORTING VALUE(text) TYPE string.\n\
    CLASS-EVENTS finished EXPORTING VALUE(code) TYPE i.\n\
ENDCLASS.\n\
\n\
CLASS lcl_handler DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS on_changed FOR EVENT changed OF lcl_sender IMPORTING text sender.\n\
ENDCLASS.\n\
\n\
CLASS lcl_sender IMPLEMENTATION.\n\
ENDCLASS.\n\
\n\
CLASS lcl_handler IMPLEMENTATION.\n\
  METHOD on_changed.\n\
    DATA lv_text TYPE string.\n\
    lv_text = text.\n\
    sender = sender.\n\
  ENDMETHOD.\n\
ENDCLASS.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let project = analyze_project_from_units(vec![analyze_unit(
            "file:///class_events.abap",
            src,
            &parsed,
        )]);
        let unit = &project.units[0];

        let changed = unit
            .class_members
            .iter()
            .find(|member| {
                member.kind == super::ClassMemberKind::Event
                    && member.name.as_ref() == "changed"
                    && unit.symbol(member.class_symbol).name.as_ref() == "lcl_sender"
            })
            .expect("changed event");
        assert_eq!(changed.kind, super::ClassMemberKind::Event);
        assert!(!changed.is_static);
        assert_eq!(changed.parameters.len(), 1);
        assert_eq!(changed.parameters[0].name.as_ref(), "text");
        assert_eq!(
            changed.parameters[0].type_clause_display.as_deref(),
            Some("string")
        );

        let finished = unit
            .class_members
            .iter()
            .find(|member| {
                member.kind == super::ClassMemberKind::Event
                    && member.name.as_ref() == "finished"
                    && unit.symbol(member.class_symbol).name.as_ref() == "lcl_sender"
            })
            .expect("finished event");
        assert_eq!(finished.kind, super::ClassMemberKind::Event);
        assert!(finished.is_static);

        let text_offset = src.rfind("text.").expect("text use") + 1;
        let text_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(text_offset)
            .expect("text reference");
        assert_eq!(text_ref.name.as_ref(), "text");
        assert!(matches!(text_ref.resolution, Some(Resolution::Symbol(_))));

        let sender_offset = src.rfind("sender.").expect("sender use") + 1;
        let sender_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(sender_offset)
            .expect("sender reference");
        let Resolution::Symbol(sender_handle) = sender_ref.resolution.expect("sender resolution")
        else {
            panic!("unexpected sender resolution: {:?}", sender_ref.resolution);
        };
        let sender_symbol = unit.symbol(sender_handle.symbol);
        let sender_type = sender_symbol.declared_type.as_ref().expect("sender type");
        assert!(sender_type.is_ref);
        assert_eq!(sender_type.base_name.as_ref(), "lcl_sender");
    }

    #[test]
    fn raise_event_uses_event_signature_for_validation() {
        let src = "\
CLASS lcl_sender DEFINITION.\n\
  PUBLIC SECTION.\n\
    EVENTS changed EXPORTING VALUE(value) TYPE i.\n\
    METHODS trigger.\n\
ENDCLASS.\n\
\n\
CLASS lcl_sender IMPLEMENTATION.\n\
  METHOD trigger.\n\
    RAISE EVENT changed EXPORTING wrong = 1.\n\
  ENDMETHOD.\n\
ENDCLASS.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let project = analyze_project_from_units(vec![analyze_unit(
            "file:///raise_event_validate.abap",
            src,
            &parsed,
        )]);
        let unit = &project.units[0];

        assert!(unit.call_sites.iter().any(|call_site| {
            matches!(
                &call_site.target,
                NamedArgumentTarget::Event {
                    qualifier: None,
                    event_name
                } if event_name.as_ref() == "changed"
            )
        }));
        assert!(unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownNamedParameter
                && src[diag.range.clone()].contains("wrong")
        }));
        assert!(unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::MissingRequiredParameter
                && diag.message.contains("event 'changed'")
        }));
    }

    #[test]
    fn implemented_interface_events_raise_unqualified() {
        let src = "\
INTERFACE lif_source.\n\
  EVENTS changed EXPORTING VALUE(value) TYPE string.\n\
ENDINTERFACE.\n\
\n\
CLASS lcl_sender DEFINITION.\n\
  PUBLIC SECTION.\n\
    INTERFACES lif_source.\n\
    METHODS trigger.\n\
ENDCLASS.\n\
\n\
CLASS lcl_sender IMPLEMENTATION.\n\
  METHOD trigger.\n\
    RAISE EVENT changed EXPORTING value = 'x'.\n\
  ENDMETHOD.\n\
ENDCLASS.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let project = analyze_project_from_units(vec![analyze_unit(
            "file:///interface_event_raise.abap",
            src,
            &parsed,
        )]);
        let unit = &project.units[0];

        assert!(
            unit.diagnostics
                .iter()
                .all(|diag| diag.kind != DiagnosticKind::UnknownNamedParameter),
            "{:#?}",
            unit.diagnostics
        );
        assert!(unit.call_sites.iter().any(|call_site| {
            matches!(
                &call_site.target,
                NamedArgumentTarget::Event {
                    qualifier: None,
                    event_name
                } if event_name.as_ref() == "changed"
            )
        }));
    }

    #[test]
    fn submit_statement_collects_report_call_sites_and_dynamic_target_refs() {
        let src = "\
REPORT zsubmit_demo.

DATA lv_report TYPE syrepid VALUE 'RSNAST0D'.

START-OF-SELECTION.
  SUBMIT rsnast00.
  SUBMIT (lv_report) AND RETURN.
";
        let parsed = parse(src);
        let unit = analyze_unit("file:///submit.abap", src, &parsed);

        assert!(unit.call_sites.iter().any(|call_site| {
            matches!(
                &call_site.target,
                NamedArgumentTarget::Report { report_name } if report_name.as_ref() == "rsnast00"
            )
        }));
        assert!(
            unit.routine_sites
                .iter()
                .any(|site| site.kind == super::RoutineSiteKind::Leave)
        );
        assert!(
            unit.routine_sites
                .iter()
                .any(|site| site.kind == super::RoutineSiteKind::UnknownEffect)
        );

        let dynamic_offset = src.rfind("lv_report").expect("dynamic submit target") + 1;
        let dynamic_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(dynamic_offset)
            .expect("dynamic submit reference");
        assert_eq!(dynamic_ref.name.as_ref(), "lv_report");
        assert!(matches!(
            dynamic_ref.resolution,
            Some(Resolution::Symbol(_))
        ));
    }

    #[test]
    fn submit_statement_collects_refs_for_full_option_set() {
        let src = "\
REPORT zsubmit_full.

DATA:
  lv_report  TYPE syrepid VALUE 'RSNAST0D',
  lv_variant TYPE c LENGTH 14,
  lv_prog    TYPE syrepid,
  lt_rspar   TYPE STANDARD TABLE OF rsparams WITH EMPTY KEY,
  lv_bukrs   TYPE bukrs,
  lv_low     TYPE datum,
  lv_high    TYPE datum,
  lv_sign    TYPE c LENGTH 1,
  lt_vkorg   TYPE RANGE OF vkorg,
  lt_texpr   TYPE rsds_texpr,
  lv_width   TYPE i,
  lv_lines   TYPE i,
  ls_pri     TYPE pri_params,
  ls_arc     TYPE arc_params,
  lv_user    TYPE syuname,
  lv_job     TYPE tbtcjob-jobname,
  lv_count   TYPE tbtcjob-jobcount,
  lv_lang    TYPE sylangu.

START-OF-SELECTION.
  SUBMIT (lv_report)
    USING SELECTION-SCREEN '1100'
    USING SELECTION-SET lv_variant
    USING SELECTION-SETS OF PROGRAM lv_prog
    WITH SELECTION-TABLE lt_rspar
    WITH p_bukrs EQ lv_bukrs
    WITH s_erdat NOT BETWEEN lv_low AND lv_high SIGN lv_sign
    WITH s_vkorg IN lt_vkorg
    WITH FREE SELECTIONS lt_texpr
    LINE-SIZE lv_width
    LINE-COUNT lv_lines
    TO SAP-SPOOL
    SPOOL PARAMETERS ls_pri
    ARCHIVE PARAMETERS ls_arc
    WITHOUT SPOOL DYNPRO
    USER lv_user
    VIA JOB lv_job NUMBER lv_count LANGUAGE lv_lang
    AND RETURN.
";
        let parsed = parse(src);
        let unit = analyze_unit("file:///submit_full.abap", src, &parsed);

        for name in [
            "lv_report",
            "lv_variant",
            "lv_prog",
            "lt_rspar",
            "lv_bukrs",
            "lv_low",
            "lv_high",
            "lv_sign",
            "lt_vkorg",
            "lt_texpr",
            "lv_width",
            "lv_lines",
            "ls_pri",
            "ls_arc",
            "lv_user",
            "lv_job",
            "lv_count",
            "lv_lang",
        ] {
            let offset = src
                .rfind(name)
                .unwrap_or_else(|| panic!("missing offset for {name}"))
                + 1;
            let reference = unit
                .semantic()
                .refs()
                .reference_at_offset(offset)
                .unwrap_or_else(|| panic!("missing reference for {name}"));
            assert_eq!(reference.name.as_ref(), name);
            assert!(matches!(reference.resolution, Some(Resolution::Symbol(_))));
        }

        assert!(
            unit.routine_sites
                .iter()
                .any(|site| site.kind == super::RoutineSiteKind::UnknownEffect)
        );
    }

    #[test]
    fn parameters_declare_symbols_and_at_selection_screen_becomes_event_block() {
        let src = "\
REPORT z_demo.\n\
\n\
PARAMETERS:\n\
  p_text TYPE string LOWER CASE OBLIGATORY,\n\
  p_pub  TYPE localfile LOWER CASE OBLIGATORY,\n\
  p_app  TYPE ssfappl   DEFAULT 'DFAULT',\n\
  p_sym  TYPE ssfencr   DEFAULT 'AES128-CBC'.\n\
\n\
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pub.\n\
  PERFORM pick_public_key_file CHANGING p_pub.\n\
\n\
START-OF-SELECTION.\n\
  WRITE p_text.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///params_event.abap", src, &parsed);

        for name in ["p_text", "p_pub", "p_app", "p_sym"] {
            assert!(
                unit.symbols.iter().any(|symbol| {
                    symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == name
                }),
                "missing parameter symbol {name}: {:?}",
                unit.symbols
            );
        }

        let event = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::Event
                    && symbol.name.as_ref() == "at selection-screen on value-request for p_pub"
            })
            .expect("selection-screen event symbol");
        assert_eq!(
            &src[event.decl_range.clone()],
            "AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pub"
        );

        let header_ref_offset = src.find("FOR p_pub").expect("header ref") + "FOR ".len() + 1;
        let header_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(header_ref_offset)
            .expect("header reference");
        assert_eq!(header_ref.name.as_ref(), "p_pub");
        assert!(matches!(header_ref.resolution, Some(Resolution::Symbol(_))));

        assert!(
            unit.diagnostics.iter().all(|diagnostic| {
                !diagnostic.message.contains("unknown symbol 'p_pub'")
                    && !diagnostic.message.contains("unknown symbol 'p_app'")
                    && !diagnostic.message.contains("unknown symbol 'p_sym'")
                    && !diagnostic.message.contains("unknown symbol 'selection'")
                    && !diagnostic.message.contains("unknown symbol 'request'")
            }),
            "{:#?}",
            unit.diagnostics
        );
    }

    #[test]
    fn parameter_declare_symbols_and_at_selection_screen_becomes_event_block() {
        let src = "\
REPORT z_demo.\n\
\n\
PARAMETER p_pub TYPE string.\n\
\n\
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pub.\n\
  PERFORM pick_public_key_file CHANGING p_pub.\n\
\n\
START-OF-SELECTION.\n\
  WRITE p_pub.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///parameter_event.abap", src, &parsed);

        assert!(unit.symbols.iter().any(|symbol| {
            symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "p_pub"
        }));

        let event = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::Event
                    && symbol.name.as_ref() == "at selection-screen on value-request for p_pub"
            })
            .expect("selection-screen event symbol");
        assert_eq!(
            &src[event.decl_range.clone()],
            "AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pub"
        );

        let header_ref_offset = src.find("FOR p_pub").expect("header ref") + "FOR ".len() + 1;
        let header_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(header_ref_offset)
            .expect("header reference");
        assert_eq!(header_ref.name.as_ref(), "p_pub");
        assert!(matches!(header_ref.resolution, Some(Resolution::Symbol(_))));

        assert!(
            unit.diagnostics.iter().all(|diagnostic| {
                !diagnostic.message.contains("unknown symbol 'p_pub'")
                    && !diagnostic.message.contains("unknown symbol 'selection'")
                    && !diagnostic.message.contains("unknown symbol 'request'")
            }),
            "{:#?}",
            unit.diagnostics
        );
    }

    #[test]
    fn selection_screen_block_title_resolves_without_screen_false_positive() {
        let src = "\
DATA gv_fselc TYPE string.\n\
SELECTION-SCREEN BEGIN OF BLOCK fsc WITH FRAME TITLE gv_fselc.\n\
SELECTION-SCREEN END OF BLOCK fsc.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///selection_screen_block.abap", src, &parsed);

        let title_offset = src.rfind("gv_fselc").expect("title variable") + 1;
        let title_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(title_offset)
            .expect("title reference");
        assert_eq!(title_ref.name.as_ref(), "gv_fselc");
        assert!(matches!(title_ref.resolution, Some(Resolution::Symbol(_))));

        let screen_offset = src.find("SCREEN BEGIN").expect("selection-screen keyword") + 1;
        assert!(
            unit.semantic()
                .refs()
                .reference_at_offset(screen_offset)
                .is_none(),
            "{:#?}",
            unit.references
        );
        assert!(
            unit.diagnostics
                .iter()
                .all(|diagnostic| !diagnostic.message.contains("unknown symbol 'screen'")),
            "{:#?}",
            unit.diagnostics
        );
    }

    #[test]
    fn chained_selection_screen_comments_do_not_create_false_references() {
        let src = "\
SELECTION-SCREEN: BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02,\n\
COMMENT /1(79) TEXT-003,\n\
COMMENT /1(79) TEXT-004,\n\
COMMENT /1(79) TEXT-005,\n\
COMMENT /1(79) TEXT-999,\n\
COMMENT /1(79) TEXT-006,\n\
COMMENT /1(79) TEXT-007,\n\
COMMENT /1(79) TEXT-008,\n\
END OF BLOCK b02.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///selection_screen_comments.abap", src, &parsed);

        for keyword in [
            "selection",
            "screen",
            "comment",
            "text",
            "end",
            "block",
            "b02",
        ] {
            assert!(
                unit.references
                    .iter()
                    .all(|reference| !reference.name.eq_ignore_ascii_case(keyword)),
                "unexpected `{keyword}` reference: {:#?}",
                unit.references
            );
        }
        assert!(unit.diagnostics.is_empty(), "{:#?}", unit.diagnostics);
    }

    #[test]
    fn selection_screen_layout_operands_resolve_without_keyword_refs() {
        let src = "\
REPORT zsyntax_selection_screen.\n\
DATA gv_title TYPE string.\n\
PARAMETERS p_carr TYPE c LENGTH 3.\n\
\n\
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE gv_title.\n\
SELECTION-SCREEN BEGIN OF LINE.\n\
SELECTION-SCREEN COMMENT 1(10) TEXT-002 FOR FIELD p_carr.\n\
PARAMETERS p_flag AS CHECKBOX.\n\
SELECTION-SCREEN END OF LINE.\n\
SELECTION-SCREEN PUSHBUTTON /1(20) TEXT-003 USER-COMMAND go.\n\
SELECTION-SCREEN ULINE /1(30).\n\
SELECTION-SCREEN SKIP 1.\n\
SELECTION-SCREEN POSITION 33.\n\
SELECTION-SCREEN END OF BLOCK b1.\n\
SELECTION-SCREEN FUNCTION KEY 1.\n\
SELECTION-SCREEN: BEGIN OF TABBED BLOCK tabs FOR 5 LINES,\n\
  TAB (20) tab1 USER-COMMAND tabgo DEFAULT SCREEN 100,\n\
  END OF BLOCK tabs.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///selection_screen_layout.abap", src, &parsed);

        for needle in ["TITLE gv_title", "FOR FIELD p_carr"] {
            let offset = src.find(needle).expect(needle) + needle.rfind(' ').unwrap() + 2;
            let reference = unit
                .semantic()
                .refs()
                .reference_at_offset(offset)
                .unwrap_or_else(|| panic!("missing reference for {needle}"));
            assert!(matches!(reference.resolution, Some(Resolution::Symbol(_))));
        }

        for name in ["go", "tabgo", "b1", "tabs", "tab1"] {
            assert!(
                unit.references
                    .iter()
                    .all(|reference| !reference.name.eq_ignore_ascii_case(name)),
                "unexpected `{name}` reference: {:#?}",
                unit.references
            );
        }
        assert!(
            !unit
                .diagnostics
                .iter()
                .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference),
            "{:#?}",
            unit.diagnostics
        );
    }

    #[test]
    fn select_options_declare_range_table_symbol_and_resolve_for_operand() {
        let src = "\
DATA lv_rogln TYPE string.\n\
SELECT-OPTIONS: s_rogln FOR lv_rogln.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///select_options.abap", src, &parsed);

        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "s_rogln")
            .expect("select-options symbol");
        assert!(symbol.declared_type.is_none());
        assert_eq!(
            symbol.type_clause_display.as_deref(),
            Some("RANGE OF lv_rogln")
        );

        let structure = symbol
            .structure
            .and_then(|id| unit.structures.get(id.as_usize()))
            .expect("range structure");
        let fields = structure
            .fields
            .iter()
            .map(|field| field.name.as_ref())
            .collect::<Vec<_>>();
        assert_eq!(fields, vec!["sign", "option", "low", "high"]);

        let operand_offset = src.rfind("lv_rogln").expect("for operand") + 1;
        let operand_ref = unit
            .semantic()
            .refs()
            .reference_at_offset(operand_offset)
            .expect("for operand reference");
        assert_eq!(operand_ref.name.as_ref(), "lv_rogln");
        assert!(matches!(
            operand_ref.resolution,
            Some(Resolution::Symbol(_))
        ));
    }

    #[test]
    fn select_options_collect_matchcode_and_dynamic_for_references() {
        let src = "\
DATA gv_gln TYPE string.\n\
DATA lv_type TYPE string.\n\
SELECT-OPTIONS:
  s_gln FOR gv_gln NO INTERVALS LOWER CASE MATCHCODE OBJECT /sttp/h_loc_gln HELP-REQUEST FOR LOW VALUE-REQUEST FOR HIGH,
  s_dyn FOR (lv_type) NO-DISPLAY.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///select_options_matchcode.abap", src, &parsed);

        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "s_gln")
            .expect("select-options symbol");
        assert_eq!(
            symbol.type_clause_display.as_deref(),
            Some("RANGE OF gv_gln")
        );

        let search_help_ref = unit
            .references
            .iter()
            .find(|reference| reference.name.as_ref() == "/sttp/h_loc_gln")
            .expect("search help reference");
        assert_eq!(search_help_ref.kind, ReferenceKind::TypeRef);
        assert_eq!(search_help_ref.namespace, Namespace::Type);

        let dynamic_for_ref = unit
            .references
            .iter()
            .find(|reference| {
                reference.name.as_ref() == "lv_type"
                    && reference.kind == ReferenceKind::Identifier
                    && reference.namespace == Namespace::Value
            })
            .expect("dynamic FOR operand reference");
        assert!(matches!(
            dynamic_for_ref.resolution,
            Some(Resolution::Symbol(_))
        ));
    }

    #[test]
    fn checkbox_parameters_default_to_abap_bool() {
        let src = "PARAMETERS: c_rom AS CHECKBOX.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///checkbox_param.abap", src, &parsed);

        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "c_rom")
            .expect("checkbox parameter");
        let declared_type = symbol.declared_type.as_ref().expect("declared type");
        assert_eq!(declared_type.namespace, Namespace::Type);
        assert_eq!(declared_type.base_name.as_ref(), "abap_bool");
        assert!(declared_type.field_path.is_empty());
    }

    #[test]
    fn radio_parameter_user_command_addition_is_not_a_reference() {
        let src = "\
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.\n\
PARAMETERS: p_backgr RADIOBUTTON GROUP g01 DEFAULT 'X' USER-COMMAND upd,\n\
            p_manual RADIOBUTTON GROUP g01.\n\
SELECTION-SCREEN END OF BLOCK b01.\n";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let unit = analyze_unit("file:///radio_user_command.abap", src, &parsed);

        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "p_backgr"
            })
            .expect("radio parameter");
        assert_eq!(symbol.value_clause_display.as_deref(), Some("'X'"));

        for name in ["user", "command", "upd"] {
            assert!(
                unit.references
                    .iter()
                    .all(|reference| reference.name.as_ref() != name),
                "unexpected `{name}` reference: {:#?}",
                unit.references
            );
            assert!(
                unit.diagnostics
                    .iter()
                    .all(|diagnostic| !diagnostic.message.contains(&format!("'{name}'"))),
                "{:#?}",
                unit.diagnostics
            );
        }
    }

    #[test]
    fn leave_list_processing_does_not_collect_list_as_identifier() {
        let src = r#"
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    LEAVE LIST-PROCESSING.
  ENDMETHOD.
ENDCLASS.
"#;
        let parsed = parse(src);
        let unit = analyze_unit("file:///leave_list_processing.abap", src, &parsed);

        assert!(
            unit.references
                .iter()
                .all(|reference| reference.name.as_ref() != "list"),
            "{:?}",
            unit.references
        );
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
