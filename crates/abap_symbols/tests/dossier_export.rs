use std::collections::HashMap;
use std::sync::Arc;

use abap_parser::parse;
use abap_symbols::{
    ProjectAnalysis, SemanticDossierContext, analyze_unit, build_project_routine_analysis,
    build_project_static_analysis_summary, build_semantic_dossier,
};

#[test]
fn dossier_exports_simple_local_resolution() {
    let src = "DATA lv_value TYPE i. lv_value = lv_value + 1.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///local.abap", src, &parsed);
    let dossier = build_semantic_dossier(
        &unit,
        SemanticDossierContext {
            parse_errors: &parsed.errors,
            project: None,
            static_analysis: None,
            target_path: Some("D:\\local.abap"),
            object_name: None,
            is_dependency: false,
            workspace_root_uri: None,
            manifest_present: false,
            project_unit_count: None,
            dependency_unit_count: None,
        },
    );

    let symbol = dossier
        .symbols
        .iter()
        .find(|symbol| symbol.name == "lv_value" && symbol.kind == "variable")
        .expect("lv_value symbol");
    let resolved_refs: Vec<_> = dossier
        .references
        .iter()
        .filter(|reference| {
            reference.name == "lv_value"
                && matches!(
                    &reference.resolution,
                    Some(abap_symbols::ReferenceResolutionDossier::Symbol { .. })
                )
        })
        .collect();

    assert_eq!(symbol.scope_id, 0);
    assert_eq!(resolved_refs.len(), 2);
    assert_eq!(dossier.summary.unresolved_reference_count, 0);
}

#[test]
fn dossier_exports_class_members_inheritance_and_calls() {
    let src = r#"
CLASS zcl_base DEFINITION.
  PUBLIC SECTION.
    METHODS base.
ENDCLASS.
CLASS zcl_base IMPLEMENTATION.
  METHOD base.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_child DEFINITION INHERITING FROM zcl_base.
  PUBLIC SECTION.
    DATA mv_flag TYPE i.
    METHODS run.
ENDCLASS.
CLASS zcl_child IMPLEMENTATION.
  METHOD run.
    me->mv_flag = 1.
    me->base( ).
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///class.abap", src, &parsed);
    let dossier = build_semantic_dossier(
        &unit,
        SemanticDossierContext {
            parse_errors: &parsed.errors,
            project: None,
            static_analysis: None,
            target_path: Some("D:\\class.abap"),
            object_name: None,
            is_dependency: false,
            workspace_root_uri: None,
            manifest_present: false,
            project_unit_count: None,
            dependency_unit_count: None,
        },
    );

    assert!(
        dossier
            .classes
            .members
            .iter()
            .any(|member| member.class_name.as_deref() == Some("zcl_child")
                && member.name == "mv_flag"
                && member.kind == "attribute")
    );
    assert!(dossier.classes.inheritance.iter().any(
        |inheritance| inheritance.class_name.as_deref() == Some("zcl_child")
            && inheritance.superclass_name == "zcl_base"
    ));
    assert!(dossier.call_sites.iter().any(|call_site| {
        matches!(
            &call_site.target,
            abap_symbols::CallTargetDossier::Method {
                base_name,
                method_name,
                ..
            } if base_name == "me" && method_name == "base"
        )
    }));
}

#[test]
fn dossier_exports_open_sql_queries_and_touched_objects() {
    let src = r#"
DATA iv_carrid TYPE c LENGTH 3.
SELECT carrid, carrname
  FROM scarr
  INTO TABLE @DATA(lt_scarr)
  WHERE carrid = @iv_carrid.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///sql.abap", src, &parsed);
    let dossier = build_semantic_dossier(
        &unit,
        SemanticDossierContext {
            parse_errors: &parsed.errors,
            project: None,
            static_analysis: None,
            target_path: Some("D:\\sql.abap"),
            object_name: None,
            is_dependency: false,
            workspace_root_uri: None,
            manifest_present: false,
            project_unit_count: None,
            dependency_unit_count: None,
        },
    );

    let query = dossier.sql.queries.first().expect("sql query");
    assert_eq!(dossier.sql.touched_objects, vec!["scarr".to_string()]);
    assert!(query.sources.iter().any(|source| source.name == "scarr"));
    assert!(
        query
            .projections
            .iter()
            .any(|projection| projection.name.as_deref() == Some("carrid"))
    );
    assert!(
        query
            .predicates
            .iter()
            .any(|predicate| predicate.kind == "where")
    );
    assert!(
        query
            .targets
            .iter()
            .any(|target| target.target_name.as_deref() == Some("lt_scarr"))
    );
}

#[test]
fn dossier_buckets_unresolved_references() {
    let src = "lv_missing = 1.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///unknown.abap", src, &parsed);
    let dossier = build_semantic_dossier(
        &unit,
        SemanticDossierContext {
            parse_errors: &parsed.errors,
            project: None,
            static_analysis: None,
            target_path: Some("D:\\unknown.abap"),
            object_name: None,
            is_dependency: false,
            workspace_root_uri: None,
            manifest_present: false,
            project_unit_count: None,
            dependency_unit_count: None,
        },
    );

    assert!(
        dossier
            .unresolved_names
            .references
            .iter()
            .any(|reference| reference.name == "lv_missing")
    );
    assert!(
        dossier
            .semantic_diagnostics
            .iter()
            .any(|diagnostic| diagnostic.kind == "unresolved_reference")
    );
    assert_eq!(dossier.summary.unresolved_reference_count, 1);
}

#[test]
fn dossier_exports_expression_facts_and_value_flow_edges() {
    let src = r#"
TYPES: BEGIN OF scarr,
         carrid TYPE string,
       END OF scarr.

CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS make_row RETURNING VALUE(rs_row) TYPE scarr.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD make_row.
  ENDMETHOD.
ENDCLASS.

DATA lo_demo TYPE REF TO zcl_demo.
DATA ls_row TYPE scarr.

SELECT carrid FROM scarr INTO TABLE @DATA(lt_scarr).
ls_row = lo_demo->make_row( ).
WRITE lt_scarr.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///facts_dossier.abap", src, &parsed);
    let dossier = build_semantic_dossier(
        &unit,
        SemanticDossierContext {
            parse_errors: &parsed.errors,
            project: None,
            static_analysis: None,
            target_path: Some("D:\\facts_dossier.abap"),
            object_name: None,
            is_dependency: false,
            workspace_root_uri: None,
            manifest_present: false,
            project_unit_count: None,
            dependency_unit_count: None,
        },
    );

    assert_eq!(dossier.schema_version, 3);
    assert!(dossier.summary.expression_fact_count > 0);
    assert!(dossier.summary.value_flow_edge_count > 0);
    assert!(dossier.expression_facts.iter().any(|fact| {
        fact.kind == "call_result"
            && fact
                .type_fact
                .declared_type
                .as_ref()
                .is_some_and(|type_ref| type_ref.base_name == "scarr")
    }));
    assert!(dossier.expression_facts.iter().any(|fact| {
        fact.kind == "reference"
            && fact
                .type_fact
                .table_line
                .as_ref()
                .is_some_and(|line| line.known)
    }));
    assert!(
        dossier
            .value_flow_edges
            .iter()
            .any(|edge| edge.kind == "assignment")
    );
}

#[test]
fn dossier_exports_compact_static_analysis_summary() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    RETURN.
    lv_value = 1.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///static_analysis.abap", src, &parsed);
    let unit_id = unit.unit_id;
    let uri = Arc::clone(&unit.uri);
    let project = ProjectAnalysis {
        units: vec![unit.clone()],
        uri_to_unit: HashMap::from([(uri, unit_id)]),
        provided_name_to_unit: HashMap::new(),
        diagnostics: Vec::new(),
    };
    let routine_analysis = build_project_routine_analysis(&project);
    let static_analysis = build_project_static_analysis_summary(&project, &routine_analysis);
    let mut unit = unit;
    for diagnostic in routine_analysis.diagnostics_for_unit(unit_id) {
        if !unit.diagnostics.contains(diagnostic) {
            unit.diagnostics.push(diagnostic.clone());
        }
    }
    let dossier = build_semantic_dossier(
        &unit,
        SemanticDossierContext {
            parse_errors: &parsed.errors,
            project: None,
            static_analysis: Some(&static_analysis),
            target_path: Some("D:\\static_analysis.abap"),
            object_name: None,
            is_dependency: false,
            workspace_root_uri: None,
            manifest_present: false,
            project_unit_count: None,
            dependency_unit_count: None,
        },
    );

    let static_analysis = dossier
        .static_analysis
        .as_ref()
        .expect("static analysis section");
    assert_eq!(dossier.schema_version, 3);
    assert_eq!(dossier.summary.static_analysis_routine_count, 1);
    assert_eq!(static_analysis.routine_count, 1);
    assert_eq!(dossier.summary.static_analysis_finding_count, 1);
    assert!(static_analysis.routines.iter().any(|routine| {
        routine.kind == "method"
            && routine
                .findings
                .iter()
                .any(|finding| finding.kind == "unreachable_code")
    }));
}
