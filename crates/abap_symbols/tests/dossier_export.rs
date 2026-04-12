use abap_parser::parse;
use abap_symbols::{SemanticDossierContext, analyze_unit, build_semantic_dossier};

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
