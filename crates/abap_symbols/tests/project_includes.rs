use abap_parser::parse;

use abap_symbols::{
    DiagnosticKind, Namespace, ProjectInput, ReferenceKind, Resolution, SqlNameRefKind,
    analyze_project,
};

#[test]
fn resolves_symbols_from_included_units() {
    let root_src = "INCLUDE zinc. lv_inc = 1.";
    let include_src = "DATA lv_inc TYPE i.";
    let root_parse = parse(root_src);
    let include_parse = parse(include_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "zinc.abap",
            source: include_src,
            parse: &include_parse,
        },
    ]);

    let root = project.unit_by_uri("zmain.abap").expect("root unit");
    assert!(root.include_edges.iter().any(|edge| edge.target.is_some()));
    assert!(root.references.iter().any(|reference| {
        reference.name.as_ref() == "lv_inc"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
}

#[test]
fn reports_unresolved_include_targets() {
    let root_src = "INCLUDE zmissing. lv_inc = 1.";
    let root_parse = parse(root_src);
    let project = analyze_project(&[ProjectInput {
        uri: "zmain.abap",
        source: root_src,
        parse: &root_parse,
    }]);

    assert!(
        project
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedInclude)
    );
}

#[test]
fn resolves_symbols_from_second_chained_include_unit() {
    let root_src = "INCLUDE: zinc_first, zinc_second. lv_second = 1.";
    let first_include_src = "\" first include intentionally empty";
    let second_include_src = "DATA lv_second TYPE i.";
    let root_parse = parse(root_src);
    let first_include_parse = parse(first_include_src);
    let second_include_parse = parse(second_include_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "zinc_first.abap",
            source: first_include_src,
            parse: &first_include_parse,
        },
        ProjectInput {
            uri: "zinc_second.abap",
            source: second_include_src,
            parse: &second_include_parse,
        },
    ]);

    let root = project.unit_by_uri("zmain.abap").expect("root unit");
    assert_eq!(root.include_edges.len(), 2);
    assert!(
        root.include_edges
            .iter()
            .any(|edge| edge.name.as_ref() == "zinc_second" && edge.target.is_some())
    );
    assert!(root.references.iter().any(|reference| {
        reference.name.as_ref() == "lv_second"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
}

#[test]
fn reports_missing_method_calls_across_include_units() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
START-OF-SELECTION.
  CREATE OBJECT gr_demo.
  CALL METHOD gr_demo->get_data1.
"#;
    let top_src = "DATA gr_demo TYPE REF TO lcl_demo.";
    let f01_src = r#"
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS: get_response IMPORTING iv_x TYPE i,
      get_data.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD get_response.
  ENDMETHOD.

  METHOD get_data.
  ENDMETHOD.
ENDCLASS.
"#;
    let root_parse = parse(root_src);
    let top_parse = parse(top_src);
    let f01_parse = parse(f01_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "ztop.abap",
            source: top_src,
            parse: &top_parse,
        },
        ProjectInput {
            uri: "zf01.abap",
            source: f01_src,
            parse: &f01_parse,
        },
    ]);

    let root = project.unit_by_uri("zmain.abap").expect("root unit");
    assert!(
        root.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("get_data1")
        }),
        "expected UnknownField for missing method across include units, got {:?}",
        root.diagnostics
    );
}

#[test]
fn reports_call_method_on_attribute_across_include_units() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
START-OF-SELECTION.
  CREATE OBJECT gr_demo.
  CALL METHOD gr_demo->gv_rule_proc_count.
"#;
    let top_src = "DATA gr_demo TYPE REF TO lcl_demo.";
    let f01_src = r#"
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    DATA gv_rule_proc_count TYPE i.
    METHODS get_data.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD get_data.
  ENDMETHOD.
ENDCLASS.
"#;
    let root_parse = parse(root_src);
    let top_parse = parse(top_src);
    let f01_parse = parse(f01_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "ztop.abap",
            source: top_src,
            parse: &top_parse,
        },
        ProjectInput {
            uri: "zf01.abap",
            source: f01_src,
            parse: &f01_parse,
        },
    ]);

    let root = project.unit_by_uri("zmain.abap").expect("root unit");
    assert!(
        root.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField
                && diag.message.contains("gv_rule_proc_count")
                && diag.message.contains("not a method")
        }),
        "expected UnknownField for CALL METHOD on attribute across include units, got {:?}",
        root.diagnostics
    );
}

#[test]
fn classic_open_sql_where_globals_from_top_include_are_not_collected_as_sql_columns() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
"#;
    let top_src = r#"
DATA p_lgnum TYPE string.
DATA p_lgtyp TYPE string.
DATA p_lgpla TYPE string.
"#;
    let f01_src = r#"
FORM run.
  DATA lw_lgpla TYPE string.
  DATA lw_skzsi TYPE string.

  SELECT SINGLE lgpla
                skzsi
    FROM lagp
    INTO (lw_lgpla, lw_skzsi)
    WHERE lgnum = p_lgnum
      AND lgtyp = p_lgtyp
      AND lgpla = p_lgpla.
ENDFORM.
"#;
    let root_parse = parse(root_src);
    let top_parse = parse(top_src);
    let f01_parse = parse(f01_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "ztop.abap",
            source: top_src,
            parse: &top_parse,
        },
        ProjectInput {
            uri: "zf01.abap",
            source: f01_src,
            parse: &f01_parse,
        },
    ]);

    let unit = project.unit_by_uri("zf01.abap").expect("include unit");

    for name in ["p_lgnum", "p_lgtyp", "p_lgpla"] {
        assert!(
            !unit.sql_name_refs.iter().any(|reference| {
                reference.kind == SqlNameRefKind::Column && reference.name.as_ref() == name
            }),
            "include global {name} must not be recorded as Open SQL column: {:?}",
            unit.sql_name_refs
        );
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected include global {name} to resolve as value reference, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }
}

#[test]
fn reports_ddic_table_type_use_without_tables_in_report_include_closure() {
    let root_src = r#"
REPORT zmain.
INCLUDE zf01.
"#;
    let include_src = "DATA ls_lagp TYPE lagp.";
    let table_src = "TYPES lagp TYPE string.";
    let root_parse = parse(root_src);
    let include_parse = parse(include_src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "zf01.abap",
            source: include_src,
            parse: &include_parse,
        },
        ProjectInput {
            uri: "/sap/bc/adt/ddic/tables/lagp",
            source: table_src,
            parse: &table_parse,
        },
    ]);

    let include = project.unit_by_uri("zf01.abap").expect("include unit");
    assert!(
        include.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::MissingTablesDeclaration && diag.message.contains("lagp")
        }),
        "expected missing TABLES diagnostic, got {:?}",
        include.diagnostics
    );
}

#[test]
fn accepts_ddic_table_type_use_when_tables_is_declared_in_relevant_include() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
"#;
    let top_src = "TABLES lagp.";
    let include_src = "DATA ls_lagp TYPE lagp.";
    let table_src = "TYPES lagp TYPE string.";
    let root_parse = parse(root_src);
    let top_parse = parse(top_src);
    let include_parse = parse(include_src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "ztop.abap",
            source: top_src,
            parse: &top_parse,
        },
        ProjectInput {
            uri: "zf01.abap",
            source: include_src,
            parse: &include_parse,
        },
        ProjectInput {
            uri: "/sap/bc/adt/ddic/tables/lagp",
            source: table_src,
            parse: &table_parse,
        },
    ]);

    let include = project.unit_by_uri("zf01.abap").expect("include unit");
    assert!(
        include
            .diagnostics
            .iter()
            .all(|diag| diag.kind != DiagnosticKind::MissingTablesDeclaration),
        "unexpected missing TABLES diagnostic: {:?}",
        include.diagnostics
    );
}

#[test]
fn reports_ddic_table_field_type_use_without_tables_work_area() {
    let root_src = r#"
REPORT zmain.
INCLUDE zf01.
"#;
    let include_src = "DATA lv_jobname TYPE tbtco-jobname.";
    let table_src = "TYPES: BEGIN OF tbtco, jobname TYPE string, END OF tbtco.";
    let root_parse = parse(root_src);
    let include_parse = parse(include_src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "zf01.abap",
            source: include_src,
            parse: &include_parse,
        },
        ProjectInput {
            uri: "/sap/bc/adt/vit/wb/object_type/tabldt/object_name/TBTCO",
            source: table_src,
            parse: &table_parse,
        },
    ]);

    let include = project.unit_by_uri("zf01.abap").expect("include unit");
    assert!(
        include.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::MissingTablesDeclaration && diag.message.contains("tbtco")
        }),
        "expected missing TABLES diagnostic for tbtco-jobname, got {:?}",
        include.diagnostics
    );
}

#[test]
fn reports_ddic_view_field_type_use_without_tables_work_area() {
    let src = r#"
REPORT zmain.
SELECT-OPTIONS so_st FOR v_op-status.
"#;
    let view_src = "TYPES: BEGIN OF v_op, status TYPE string, END OF v_op.";
    let parse_src = parse(src);
    let view_parse = parse(view_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: src,
            parse: &parse_src,
        },
        ProjectInput {
            uri: "/sap/bc/adt/vit/wb/object_type/viewdv/object_name/V_OP",
            source: view_src,
            parse: &view_parse,
        },
    ]);

    let unit = project.unit_by_uri("zmain.abap").expect("report unit");
    assert!(
        unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::MissingTablesDeclaration && diag.message.contains("v_op")
        }),
        "expected missing TABLES diagnostic for v_op-status, got {:?}",
        unit.diagnostics
    );
}

#[test]
fn accepts_ddic_view_field_type_use_when_tables_is_declared() {
    let src = r#"
REPORT zmain.
TABLES v_op.
SELECT-OPTIONS so_st FOR v_op-status.
"#;
    let view_src = "TYPES: BEGIN OF v_op, status TYPE string, END OF v_op.";
    let parse_src = parse(src);
    let view_parse = parse(view_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: src,
            parse: &parse_src,
        },
        ProjectInput {
            uri: "/sap/bc/adt/vit/wb/object_type/viewdv/object_name/V_OP",
            source: view_src,
            parse: &view_parse,
        },
    ]);

    let unit = project.unit_by_uri("zmain.abap").expect("report unit");
    assert!(
        unit.diagnostics
            .iter()
            .all(|diag| diag.kind != DiagnosticKind::MissingTablesDeclaration),
        "unexpected missing TABLES diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_inherited_protected_attribute_across_project_units() {
    let parent_src = r#"
CLASS zcl_parent DEFINITION.
  PROTECTED SECTION.
    DATA gv_dummy_msg TYPE string.
ENDCLASS.

CLASS zcl_parent IMPLEMENTATION.
ENDCLASS.
"#;
    let child_src = r#"
CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS m.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD m.
    gv_dummy_msg = 'x'.
  ENDMETHOD.
ENDCLASS.
"#;
    let parent_parse = parse(parent_src);
    let child_parse = parse(child_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zcl_parent.abap",
            source: parent_src,
            parse: &parent_parse,
        },
        ProjectInput {
            uri: "zcl_child.abap",
            source: child_src,
            parse: &child_parse,
        },
    ]);

    let parent = project.unit_by_uri("zcl_parent.abap").expect("parent unit");
    let child = project.unit_by_uri("zcl_child.abap").expect("child unit");

    assert!(
        !child.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("gv_dummy_msg")
        }),
        "unexpected unresolved inherited attribute diagnostic: {:?}",
        child.diagnostics
    );

    let reference = child
        .references
        .iter()
        .find(|reference| reference.name.as_ref() == "gv_dummy_msg")
        .expect("gv_dummy_msg reference");
    let Resolution::Symbol(handle) = reference.resolution.expect("resolved reference") else {
        panic!("expected symbol resolution, got {:?}", reference.resolution);
    };
    assert_eq!(handle.unit, parent.unit_id);
    let symbol = &parent.symbols[handle.symbol.as_usize()];
    assert_eq!(symbol.name.as_ref(), "gv_dummy_msg");
}
