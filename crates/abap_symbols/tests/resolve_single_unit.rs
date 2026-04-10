use abap_parser::parse;

use abap_symbols::{
    DiagnosticKind, Namespace, ProjectInput, ReferenceKind, Resolution, SqlNameRefKind,
    SqlPredicateKind, SqlProjectionKind, SqlSourceKind, SqlTargetKind, StructureFieldShape,
    SymbolHandle, analyze_project, analyze_unit,
};

#[test]
fn resolves_do_times_count_variable_in_header() {
    let src = r#"
FORM f.
  DATA lv_max_len TYPE i.
  DO lv_max_len TIMES.
    WRITE / 'x'.
  ENDDO.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///do_times_header.abap", src, &parsed);

    let refs: Vec<_> = unit
        .references
        .iter()
        .filter(|r| r.name.as_ref() == "lv_max_len")
        .collect();
    assert_eq!(
        refs.len(),
        1,
        "expected one reference in DO header, got {:?}",
        refs
    );
    assert!(
        refs[0].resolution.is_some(),
        "expected lv_max_len in DO header to resolve"
    );
    assert!(
        !unit.diagnostics.iter().any(|d| {
            d.kind == DiagnosticKind::UnresolvedReference && d.message.contains("lv_max_len")
        }),
        "unexpected unresolved lv_max_len: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_references_inside_do_enddo_body() {
    let src = r#"
FORM f.
  DATA lv TYPE i.
  DO 3 TIMES.
    lv = lv + 1.
  ENDDO.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///do_body.abap", src, &parsed);

    let lv_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|r| r.name.as_ref() == "lv")
        .collect();
    assert!(
        !lv_refs.is_empty(),
        "expected references to lv inside DO body, got: {:?}",
        unit.references
    );
    assert!(
        lv_refs.iter().all(|r| r.resolution.is_some()),
        "expected lv references to resolve: {:?}",
        lv_refs
    );
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|d| { d.kind == DiagnosticKind::UnresolvedReference && d.message.contains("lv") }),
        "unexpected unresolved lv: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_case_when_branch_header_symbols() {
    let src = r#"
FORM f.
  CONSTANTS lc_rs_agg_op TYPE string VALUE 'SUM'.
  DATA lv_kind TYPE string.

  CASE lv_kind.
    WHEN lc_rs_agg_op.
      WRITE / lc_rs_agg_op.
    WHEN OTHERS.
      WRITE / lv_kind.
  ENDCASE.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///case_when_header.abap", src, &parsed);

    let refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.name.as_ref() == "lc_rs_agg_op" && reference.kind == ReferenceKind::Identifier
        })
        .collect();
    assert_eq!(
        refs.len(),
        2,
        "expected header and body references for lc_rs_agg_op, got {:?}",
        refs
    );
    assert!(
        refs.iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected lc_rs_agg_op references to resolve: {:?}",
        refs
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("lc_rs_agg_op")
        }),
        "unexpected unresolved lc_rs_agg_op: {:?}",
        unit.diagnostics
    );
}

#[test]
fn commit_and_rollback_work_do_not_report_work_as_unknown_symbol() {
    let src = r#"
FORM f.
  COMMIT WORK.
  ROLLBACK WORK.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///commit_rollback_work.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("unknown symbol 'work'")
        }),
        "unexpected unresolved WORK diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn type_pools_statement_is_ignored_for_semantic_analysis() {
    let src = r#"
FORM f.
  TYPE-POOLS abap.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///type_pools.abap", src, &parsed);

    assert!(
        !unit
            .references
            .iter()
            .any(|reference| reference.name.as_ref() == "abap"),
        "unexpected semantic refs from TYPE-POOLS: {:?}",
        unit.references
    );
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("abap")),
        "unexpected TYPE-POOLS diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_form_changing_parameter_in_body() {
    let src = r#"
FORM some_form CHANGING cv_result TYPE string.
    DATA:
        lv_var1 TYPE i,
        lv_var2 TYPE string.

    lv_var2 = 'hello'.

    cv_result = lv_var2.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///form.abap", src, &parsed);

    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == abap_symbols::SymbolKind::Parameter && symbol.name.as_ref() == "cv_result"
    }));

    let refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| reference.name.as_ref() == "cv_result")
        .collect();
    assert!(
        refs.iter().all(|reference| reference.resolution.is_some()),
        "expected cv_result references to resolve, got: {:?}",
        refs
    );

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("cv_result")
        }),
        "unexpected unresolved diagnostic: {:?}",
        unit.diagnostics
    );

    let cv = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Parameter
                && symbol.name.as_ref() == "cv_result"
        })
        .expect("cv_result parameter");
    let dt = cv.declared_type.as_ref().expect("parameter declared type");
    assert_eq!(dt.namespace, abap_symbols::Namespace::Type);
    assert_eq!(dt.base_name.as_ref(), "string");
    assert!(dt.field_path.is_empty());
}

#[test]
fn form_value_parameter_records_declared_type() {
    let src = r#"
FORM f USING VALUE(iv) TYPE i.
  iv = 1.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_param.abap", src, &parsed);

    let iv = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Parameter && symbol.name.as_ref() == "iv"
        })
        .expect("iv parameter");
    let dt = iv.declared_type.as_ref().expect("declared type");
    assert_eq!(dt.namespace, abap_symbols::Namespace::Type);
    assert_eq!(dt.base_name.as_ref(), "i");
    assert!(dt.field_path.is_empty());
}

#[test]
fn form_header_collects_structured_type_ref() {
    let src = r#"
INTERFACE zif_demo.
  TYPES ty_row TYPE string.
ENDINTERFACE.

FORM run USING VALUE(io_row) TYPE REF TO zif_demo=>ty_row.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///form_type_ref.abap", src, &parsed);

    let io_row = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Parameter && symbol.name.as_ref() == "io_row"
        })
        .expect("io_row parameter");
    let dt = io_row.declared_type.as_ref().expect("declared type");
    assert_eq!(dt.namespace, Namespace::Type);
    assert!(dt.is_ref);
    assert_eq!(dt.base_name.as_ref(), "zif_demo");
    assert_eq!(dt.field_path.len(), 1);
    assert_eq!(dt.field_path[0].as_ref(), "ty_row");

    assert!(unit.references.iter().any(|reference| {
        reference.kind == ReferenceKind::TypeRef
            && reference.namespace == Namespace::Type
            && reference.name.as_ref() == "zif_demo"
    }));
}

#[test]
fn chained_data_with_table_type_declares_all_symbols_without_unresolved_diagnostics() {
    let src = r#"
FORM some_form.
  DATA: ls_event TYPE string,
        ls_choice TYPE string, " inline comment
        lt_split TYPE STANDARD TABLE OF string,
        ls_split TYPE string,
        lv_lines TYPE i.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///chained_data_table_type.abap", src, &parsed);

    for name in ["ls_event", "ls_choice", "lt_split", "ls_split", "lv_lines"] {
        assert!(
            unit.symbols.iter().any(|symbol| {
                symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == name
            }),
            "expected variable symbol for `{name}`, symbols={:?}",
            unit.symbols
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected unresolved diagnostic for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn chained_data_with_value_clause_declares_all_symbols_without_unresolved_diagnostics() {
    let src = r#"
FORM some_form.
  DATA: lv_curr_node TYPE string,
        lv_curr_node_nopref TYPE string,
        lv_value TYPE string,
        lv_counter TYPE int2 VALUE 1.

  lv_curr_node = lv_value.
  lv_curr_node_nopref = lv_curr_node.
  lv_counter = lv_counter + 1.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///chained_data_value_clause.abap", src, &parsed);

    for name in [
        "lv_curr_node",
        "lv_curr_node_nopref",
        "lv_value",
        "lv_counter",
    ] {
        assert!(
            unit.symbols.iter().any(|symbol| {
                symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == name
            }),
            "expected variable symbol for `{name}`, symbols={:?}",
            unit.symbols
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected unresolved diagnostic for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn collects_type_member_reference_inside_table_wrapper_type() {
    let src = r#"
INTERFACE zif_demo.
  TYPES ty_row TYPE string.
ENDINTERFACE.

TYPES ty_tab TYPE STANDARD TABLE OF zif_demo=>ty_row WITH DEFAULT KEY.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///type_member_table_type.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.kind == ReferenceKind::TypeRef
            && reference.namespace == Namespace::Type
            && reference.name.as_ref() == "zif_demo"
    }));
    assert!(unit.field_accesses.iter().any(|access| {
        access.in_type_position
            && access.base_namespace == Namespace::Type
            && access.base_name.as_ref() == "zif_demo"
            && access.field_path.len() == 1
            && access.field_path[0].name.as_ref() == "ty_row"
    }));
}

#[test]
fn resolves_valid_perform_call_to_form() {
    let src = r#"
FORM process_data
    USING pv_mode TYPE string
    CHANGING cv_count TYPE i.
ENDFORM.

DATA lv_count TYPE i.
PERFORM process_data USING 'demo' CHANGING lv_count.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///perform_valid.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.kind == ReferenceKind::RoutineCall
            && reference.namespace == Namespace::Routine
            && reference.name.as_ref() == "process_data"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::InvalidPerformCall),
        "unexpected PERFORM validation diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn rejects_invalid_perform_argument_shapes() {
    let cases = [
        "PERFORM process_data USING CHANGING lv_count.",
        "PERFORM process_data USING 'sdf'.",
        "PERFORM process_data USING lv_count.",
        "PERFORM process_data USING CHANGING.",
        "PERFORM process_data CHANGING lv_count.",
        "PERFORM process_data CHANGING lv_coun.",
        "PERFORM process_data.",
    ];

    for call in cases {
        let src = format!(
            r#"
FORM process_data
    USING pv_mode TYPE string
    CHANGING cv_count TYPE i.
ENDFORM.

DATA lv_count TYPE i.
{call}
"#
        );
        let parsed = parse(&src);
        let unit = analyze_unit("file:///perform_invalid.abap", &src, &parsed);

        assert!(
            unit.diagnostics
                .iter()
                .any(|diag| diag.kind == DiagnosticKind::InvalidPerformCall),
            "expected invalid PERFORM diagnostic for {call:?}, got {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_local_references_in_scope() {
    let src = "DATA lv_value TYPE i. lv_value = lv_value + 1.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///resolve.abap", src, &parsed);

    let resolved_value_refs = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.name.as_ref() == "lv_value"
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        })
        .count();

    assert!(resolved_value_refs >= 2);
}

#[test]
fn resolves_loop_source_and_inline_into_target() {
    let src = r#"
DATA lt_rows TYPE string.

LOOP AT lt_rows INTO DATA(ls_row).
  ls_row = ls_row.
ENDLOOP.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///loop_inline_data.abap", src, &parsed);

    let lt_rows_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "lt_rows"
        })
        .expect("LOOP source reference");
    assert!(
        matches!(lt_rows_ref.resolution, Some(Resolution::Symbol(_))),
        "expected LOOP source to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );

    let ls_row_symbol = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "ls_row")
        .expect("inline LOOP target symbol");
    assert_eq!(ls_row_symbol.kind, abap_symbols::SymbolKind::Variable);

    let ls_row_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "ls_row"
        })
        .collect();
    assert_eq!(
        ls_row_refs.len(),
        2,
        "expected body references, got {ls_row_refs:?}"
    );
    assert!(
        ls_row_refs
            .iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected LOOP target references to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("lt_rows") || diag.message.contains("ls_row"))
        }),
        "unexpected LOOP diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_inline_data_declared_in_if_after_endif() {
    let src = r#"
METHOD run.
  DATA ls_bj2_max TYPE ty_param.

  IF ls_bj2_max IS NOT INITIAL.
    DATA(lv_bj2_max) = ls_bj2_max-param_value.
  ENDIF.

  SELECT * FROM demo
    INTO TABLE @DATA(lt_rows)
    UP TO @lv_bj2_max ROWS.
ENDMETHOD.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///if_inline_scope.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("unknown symbol 'lv_bj2_max'")
        }),
        "unexpected unresolved symbol diagnostic, diagnostics={:?}",
        unit.diagnostics
    );

    let lv_bj2_max_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.namespace == Namespace::Value
                && reference.name.as_ref() == "lv_bj2_max"
                && reference.kind == ReferenceKind::Identifier
        })
        .expect("lv_bj2_max reference");

    assert!(
        matches!(lv_bj2_max_ref.resolution, Some(Resolution::Symbol(_))),
        "expected lv_bj2_max reference to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
}

#[test]
fn if_not_condition_does_not_report_not_as_unknown_symbol() {
    let src = r#"
METHOD run.
  DATA iv_flag TYPE abap_bool.

  IF NOT
     iv_flag = abap_true.
    WRITE / iv_flag.
  ENDIF.
ENDMETHOD.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///if_not_condition.abap", src, &parsed);

    assert!(
        parsed.errors.is_empty(),
        "unexpected parse errors: {:?}",
        parsed.errors
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("unknown symbol 'not'")
        }),
        "unexpected unresolved NOT diagnostic: {:?}",
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("unknown symbol 'iv_flag'")
        }),
        "unexpected unresolved iv_flag diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_table_line_pseudo_field_in_loop_where_for_scalar_line_type() {
    let src = r#"
DATA lt TYPE STANDARD TABLE OF string WITH EMPTY KEY.

LOOP AT lt TRANSPORTING NO FIELDS WHERE table_line IS NOT INITIAL.
ENDLOOP.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///loop_table_line.abap", src, &parsed);

    let table_line_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref().eq_ignore_ascii_case("table_line")
        })
        .expect("table_line reference in LOOP WHERE");
    assert_eq!(
        table_line_ref.resolution,
        Some(Resolution::InternalTableLine),
        "refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|d| d.kind == DiagnosticKind::UnresolvedReference),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn table_line_in_loop_where_stays_unresolved_for_multi_field_row_type() {
    let src = r#"
TYPES: BEGIN OF ty_row,
  a TYPE i,
  b TYPE i,
END OF ty_row.
DATA lt TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

LOOP AT lt TRANSPORTING NO FIELDS WHERE table_line IS NOT INITIAL.
ENDLOOP.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///loop_table_line_struct.abap", src, &parsed);

    let table_line_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref().eq_ignore_ascii_case("table_line")
        })
        .expect("table_line reference");
    assert!(
        table_line_ref.resolution.is_none(),
        "expected unresolved table_line for multi-field row, got {:?}",
        table_line_ref.resolution
    );
    assert!(
        unit.diagnostics.iter().any(|d| {
            d.kind == DiagnosticKind::UnresolvedReference && d.message.contains("table_line")
        }),
        "expected unknown symbol diagnostic for table_line, got {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_loop_source_and_inline_assigning_field_symbol() {
    let src = r#"
DATA lt_rows TYPE string.

LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<ls_row>).
  <ls_row> = <ls_row>.
ENDLOOP.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///loop_inline_fs.abap", src, &parsed);

    let lt_rows_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "lt_rows"
        })
        .expect("LOOP source reference");
    assert!(
        matches!(lt_rows_ref.resolution, Some(Resolution::Symbol(_))),
        "expected LOOP source to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );

    let fs_symbol = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "<ls_row>")
        .expect("inline field-symbol target");
    assert_eq!(fs_symbol.kind, abap_symbols::SymbolKind::FieldSymbol);

    let fs_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "<ls_row>"
        })
        .collect();
    assert_eq!(
        fs_refs.len(),
        2,
        "expected body references, got {fs_refs:?}"
    );
    assert!(
        fs_refs
            .iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected LOOP field-symbol references to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("lt_rows") || diag.message.contains("<ls_row>"))
        }),
        "unexpected LOOP diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_append_source_and_target() {
    let src = r#"
DATA ls_evt TYPE string.
TYPES ty_evt_tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_evt TYPE ty_evt_tab.

APPEND ls_evt TO lt_evt.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///append_stmt.abap", src, &parsed);

    for name in ["ls_evt", "lt_evt"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved APPEND reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected APPEND diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_append_lines_of_source_and_target() {
    let src = r#"
TYPES ty_evt_tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_src TYPE ty_evt_tab.
DATA lt_dst TYPE ty_evt_tab.

APPEND LINES OF lt_src TO lt_dst.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///append_lines_stmt.abap", src, &parsed);

    for name in ["lt_src", "lt_dst"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved APPEND LINES OF reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected APPEND LINES OF diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_append_initial_line_target_and_assigning_field_symbol() {
    let src = r#"
TYPES ty_evt_tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_dst TYPE ty_evt_tab.
FIELD-SYMBOLS <ls_evt> TYPE any.

APPEND INITIAL LINE TO lt_dst ASSIGNING <ls_evt>.
<ls_evt> = <ls_evt>.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///append_initial_line_stmt.abap", src, &parsed);

    for name in ["lt_dst", "<ls_evt>"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved APPEND INITIAL LINE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected APPEND INITIAL LINE diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_move_corresponding_source_and_target() {
    let src = r#"
DATA ls_general TYPE string.
DATA ls_ord_head TYPE string.

MOVE-CORRESPONDING ls_general TO ls_ord_head.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///move_corresponding_stmt.abap", src, &parsed);

    for name in ["ls_general", "ls_ord_head"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved MOVE-CORRESPONDING reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected MOVE-CORRESPONDING diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_modify_source_and_target() {
    let src = r#"
TYPES ty_trans TYPE BEGIN OF ty_trans,
        id TYPE i,
      END OF ty_trans.
DATA ls_trans TYPE ty_trans.
DATA zatt_trans_cust TYPE ty_trans.

MODIFY zatt_trans_cust FROM ls_trans.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///modify_stmt.abap", src, &parsed);

    for name in ["zatt_trans_cust", "ls_trans"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved MODIFY reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected MODIFY diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_read_table_source_and_target() {
    let src = r#"
TYPES ty_trn_tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lt_trn TYPE ty_trn_tab.
DATA ls_trn TYPE string.

READ TABLE lt_trn INTO ls_trn INDEX 1.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///read_table_stmt.abap", src, &parsed);

    for name in ["lt_trn", "ls_trn"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved READ TABLE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected READ TABLE diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_read_table_inline_into_before_with_key() {
    let src = r#"
TYPES: BEGIN OF ty_param,
  param_name TYPE string,
  param_value TYPE string,
END OF ty_param.
TYPES ty_param_tab TYPE STANDARD TABLE OF ty_param WITH DEFAULT KEY.
DATA lt_t_param TYPE ty_param_tab.
CONSTANTS lc_rs_bj2_max TYPE string VALUE 'RS_BJ2_MAX'.

READ TABLE lt_t_param INTO DATA(ls_bj2_max) WITH KEY param_name = lc_rs_bj2_max.
ls_bj2_max-param_value = ls_bj2_max-param_value.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///read_table_inline_into_with_key.abap", src, &parsed);

    for name in ["lt_t_param", "ls_bj2_max", "lc_rs_bj2_max"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved READ TABLE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected READ TABLE diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_get_time_stamp_field_target() {
    let src = r#"
DATA lv_current_ts TYPE string.

GET TIME STAMP FIELD lv_current_ts.
lv_current_ts = lv_current_ts.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///get_time_stamp_field.abap", src, &parsed);

    let refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "lv_current_ts"
        })
        .collect();
    assert!(
        refs.len() >= 3,
        "expected GET TIME STAMP target and body refs, got {refs:?}"
    );
    assert!(
        refs.iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected GET TIME STAMP target to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("lv_current_ts")
        }),
        "unexpected GET TIME STAMP diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_clear_targets() {
    let src = r#"
DATA ls_trans TYPE string.
DATA lv_state TYPE string.

CLEAR ls_trans.
CLEAR: lv_state, ls_trans.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///clear_stmt.abap", src, &parsed);

    for name in ["ls_trans", "lv_state"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved CLEAR reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected CLEAR diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_convert_date_time_operands_and_statics_type_refs() {
    let src = r#"
FORM run USING iv_date TYPE d
               iv_time TYPE t
               iv_tzone TYPE tznzone.
  STATICS sv_last_tzone TYPE tznzone.
  STATICS sv_last_offset TYPE string.
  DATA lv_timestamp TYPE timestamp.

  CONVERT DATE iv_date TIME iv_time INTO TIME STAMP lv_timestamp TIME ZONE iv_tzone.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///convert_stmt.abap", src, &parsed);

    for (name, type_name) in [("sv_last_tzone", "tznzone"), ("sv_last_offset", "string")] {
        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == name
            })
            .unwrap_or_else(|| {
                panic!(
                    "expected STATICS symbol `{name}`, symbols={:?}",
                    unit.symbols
                )
            });
        let declared_type = symbol
            .declared_type
            .as_ref()
            .unwrap_or_else(|| panic!("expected declared type for `{name}`"));
        assert_eq!(declared_type.base_name.as_ref(), type_name);
    }

    for name in ["iv_date", "iv_time", "lv_timestamp", "iv_tzone"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved CONVERT reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected CONVERT diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_get_time_stamp_inline_data_target() {
    let src = r#"
GET TIME STAMP FIELD DATA(lv_current_ts).
lv_current_ts = lv_current_ts.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///get_time_stamp_inline.abap", src, &parsed);

    let symbol = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable
                && symbol.name.as_ref() == "lv_current_ts"
        })
        .expect("inline GET TIME STAMP target");
    assert_eq!(symbol.kind, abap_symbols::SymbolKind::Variable);

    let refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "lv_current_ts"
        })
        .collect();
    assert_eq!(refs.len(), 2, "expected body references, got {refs:?}");
    assert!(
        refs.iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected inline GET TIME STAMP refs to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("lv_current_ts")
        }),
        "unexpected inline GET TIME STAMP diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_flat_select_inline_table_target_after_statement() {
    let src = r#"
SELECT rfcdest FROM rfcdes INTO TABLE @DATA(lt_rfcdes).
WRITE lt_rfcdes.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///select_inline_table.abap", src, &parsed);

    let symbol = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == "lt_rfcdes"
        })
        .expect("inline SELECT target");
    assert_eq!(symbol.kind, abap_symbols::SymbolKind::Variable);

    let refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "lt_rfcdes"
        })
        .collect();
    assert!(
        refs.iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected inline SELECT refs to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("lt_rfcdes")
        }),
        "unexpected inline SELECT diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn collects_sql_semantics_for_host_var_select() {
    let src = r#"
DATA lv_carrid TYPE string.

SELECT carrid
  FROM sflight
  INTO TABLE @DATA(lt_flights)
  WHERE carrid = @lv_carrid
  ORDER BY PRIMARY KEY.

WRITE lt_flights.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///sql_host_var.abap", src, &parsed);

    assert_eq!(unit.sql_queries.len(), 1, "{:?}", unit.sql_queries);
    let query = &unit.sql_queries[0];
    assert!(!query.has_endselect);
    assert!(!query.is_distinct);
    assert!(query.projection_clause.is_some());
    assert!(query.from_clause.is_some());
    assert!(query.into_clause.is_some());
    assert!(query.where_clause.is_some());
    assert!(query.order_by_clause.is_some());

    assert_eq!(unit.sql_sources.len(), 1, "{:?}", unit.sql_sources);
    let source = &unit.sql_sources[0];
    assert_eq!(source.source_kind, SqlSourceKind::From);
    assert_eq!(source.name.as_ref(), "sflight");
    assert!(source.alias.is_none());

    assert_eq!(unit.sql_projections.len(), 1, "{:?}", unit.sql_projections);
    let projection = &unit.sql_projections[0];
    assert_eq!(projection.kind, SqlProjectionKind::Column);
    assert_eq!(projection.name.as_deref(), Some("carrid"));

    assert_eq!(unit.sql_targets.len(), 1, "{:?}", unit.sql_targets);
    let target = &unit.sql_targets[0];
    assert_eq!(target.kind, SqlTargetKind::Into);
    assert!(target.is_table);
    assert!(target.is_inline);
    assert_eq!(target.target_name.as_deref(), Some("lt_flights"));

    assert!(unit.sql_name_refs.iter().any(|reference| {
        reference.kind == SqlNameRefKind::Column && reference.name.as_ref() == "carrid"
    }));
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "lv_carrid"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
}

#[test]
fn collects_sql_semantics_for_insert_dbtab_from_table() {
    let src = r#"
DATA lt_sequen_buff TYPE STANDARD TABLE OF string WITH EMPTY KEY.

INSERT zattp_sequen_bf FROM TABLE lt_sequen_buff ACCEPTING DUPLICATE KEYS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///insert_dbtab_from_table.abap", src, &parsed);

    assert_eq!(unit.sql_queries.len(), 1, "{:?}", unit.sql_queries);
    assert_eq!(unit.sql_sources.len(), 1, "{:?}", unit.sql_sources);
    assert_eq!(unit.sql_sources[0].name.as_ref(), "zattp_sequen_bf");
    assert!(unit.sql_name_refs.iter().any(|reference| {
        reference.kind == SqlNameRefKind::Source && reference.name.as_ref() == "zattp_sequen_bf"
    }));

    let table_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value && reference.name.as_ref() == "lt_sequen_buff"
        })
        .collect();
    assert_eq!(table_refs.len(), 1, "{:?}", unit.references);
    assert!(matches!(
        table_refs[0].resolution,
        Some(Resolution::Symbol(_))
    ));
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("zattp_sequen_bf")
    }));
    assert!(unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnverifiedOpenSqlSource
            && diag.message.contains("zattp_sequen_bf")
    }));
}

#[test]
fn open_sql_where_bare_ident_resolves_to_method_parameter_not_sql_column() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_code_char TYPE string.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    SELECT * FROM demo INTO TABLE @DATA(lt_rows) WHERE mandt = iv_code_char.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///opensql_where_param.abap", src, &parsed);

    assert!(
        !unit.sql_name_refs.iter().any(|reference| {
            reference.kind == SqlNameRefKind::Column && reference.name.as_ref() == "iv_code_char"
        }),
        "host variable should not be recorded as Open SQL column: {:?}",
        unit.sql_name_refs
    );
    assert!(
        unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Value
                && reference.name.as_ref() == "iv_code_char"
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected iv_code_char reference in WHERE, got {:?}",
        unit.references
    );
}

#[test]
fn open_sql_where_selector_host_expr_resolves_to_component_not_sql_column() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    TYPES: BEGIN OF ty_mat,
             matid TYPE string,
           END OF ty_mat.
    DATA ls_mat TYPE ty_mat.
    SELECT * FROM demo INTO TABLE @DATA(lt_rows) WHERE mandt = ls_mat-matid.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///opensql_where_selector.abap", src, &parsed);

    assert!(
        !unit.sql_name_refs.iter().any(|reference| {
            reference.kind == SqlNameRefKind::Column && reference.name.as_ref() == "matid"
        }),
        "selector host expression should not be recorded as Open SQL column: {:?}",
        unit.sql_name_refs
    );
    assert!(
        unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Value
                && reference.name.as_ref() == "ls_mat"
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected ls_mat reference in WHERE, got {:?}",
        unit.references
    );
    assert!(unit.field_accesses.iter().any(|access| {
        access.base_namespace == Namespace::Value
            && access.base_name.as_ref() == "ls_mat"
            && access.field_path.len() == 1
            && access.field_path[0].name.as_ref() == "matid"
    }));
}

#[test]
fn collects_sql_semantics_for_join_dynamic_where_and_for_all_entries() {
    let src = r#"
DATA lt_keys TYPE string.
DATA lt_cond TYPE string.

SELECT DISTINCT a~bupid, b~*
  FROM /sttp/bup AS a
  JOIN /sttp/bupmap AS b ON b~bupid = a~bupid
  FOR ALL ENTRIES IN lt_keys
  INTO TABLE @DATA(lt_rows)
  WHERE (lt_cond).

WRITE lt_rows.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///sql_dynamic_where.abap", src, &parsed);

    assert_eq!(unit.sql_queries.len(), 1, "{:?}", unit.sql_queries);
    let query = &unit.sql_queries[0];
    assert!(query.is_distinct);
    assert!(query.has_dynamic_where);
    assert!(query.for_all_entries_clause.is_some());

    assert_eq!(unit.sql_sources.len(), 2, "{:?}", unit.sql_sources);
    assert!(unit.sql_sources.iter().any(|source| {
        source.source_kind == SqlSourceKind::From
            && source.name.as_ref() == "/sttp/bup"
            && source.alias.as_deref() == Some("a")
    }));
    assert!(unit.sql_sources.iter().any(|source| {
        source.source_kind == SqlSourceKind::Join
            && source.name.as_ref() == "/sttp/bupmap"
            && source.alias.as_deref() == Some("b")
    }));

    assert!(
        unit.sql_predicates
            .iter()
            .any(|predicate| { predicate.kind == SqlPredicateKind::JoinOn })
    );
    assert!(
        unit.sql_predicates
            .iter()
            .any(|predicate| { predicate.kind == SqlPredicateKind::DynamicWhere })
    );
    assert!(
        unit.sql_predicates
            .iter()
            .any(|predicate| { predicate.kind == SqlPredicateKind::ForAllEntries })
    );

    assert!(unit.sql_name_refs.iter().any(|reference| {
        reference.kind == SqlNameRefKind::QualifiedColumn
            && reference.qualifier.as_deref() == Some("a")
            && reference.name.as_ref() == "bupid"
    }));
    assert!(unit.sql_name_refs.iter().any(|reference| {
        reference.kind == SqlNameRefKind::QualifiedStar
            && reference.qualifier.as_deref() == Some("b")
    }));

    assert_eq!(unit.sql_targets.len(), 1, "{:?}", unit.sql_targets);
    let target = &unit.sql_targets[0];
    assert_eq!(target.kind, SqlTargetKind::Into);
    assert_eq!(target.target_name.as_deref(), Some("lt_rows"));
    assert!(target.is_inline);
    assert!(target.is_table);

    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "lt_keys"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "lt_rows"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
}

#[test]
fn infers_inline_select_table_shape_from_explicit_projection_even_when_source_is_unknown() {
    let src = r#"
DATA lv_bj2_max TYPE i.
DATA lc_sts_rep TYPE string.
DATA lr_sr_rule TYPE string.

SELECT rep_evtid,
       evtid,
       rule_type,
       status_rep_evt,
       msguid_out,
       ext_ref_id,
       CASE
         WHEN ext_ref_id = 'PRIORITY1' THEN 'X'
         WHEN ext_ref_id = 'EXCLUDED' THEN 'X'
         WHEN ext_ref_id = 'PRIORITY2' THEN 'Y'
         ELSE ' '
       END AS priority,
       creation_time
  FROM /sttp/rep_evt
  INTO TABLE @DATA(lt_rep_evt)
  UP TO @lv_bj2_max ROWS
  WHERE status_rep_evt = @lc_sts_rep
  AND rule_type IN @lr_sr_rule.

LOOP AT lt_rep_evt INTO DATA(ls_rep_evt).
  WRITE ls_rep_evt-priority.
ENDLOOP.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///sql_inline_shape_unknown.abap", src, &parsed);

    let lt_rep_evt = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "lt_rep_evt")
        .expect("inline SQL target");
    let structure = unit.structure(lt_rep_evt.structure.expect("inline SQL target structure"));
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "rep_evtid")
    );
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "priority")
    );
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "creation_time")
    );

    let ls_rep_evt = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "ls_rep_evt")
        .expect("LOOP inline row");
    let line_structure = unit.structure(ls_rep_evt.structure.expect("LOOP line structure"));
    assert!(
        line_structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "priority")
    );

    assert!(unit.field_accesses.iter().any(|access| {
        access.base_name.as_ref() == "ls_rep_evt"
            && access.field_path.len() == 1
            && access.field_path[0].name.as_ref() == "priority"
    }));
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("lt_rep_evt")
                || diag.message.contains("ls_rep_evt")
                || diag.message.contains("priority"))
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
    assert!(
        unit.diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnverifiedOpenSqlSource),
        "expected unresolved DDIC warning for unknown source, got {:?}",
        unit.diagnostics
    );
    assert!(
        !unit.sql_name_refs.iter().any(|reference| {
            matches!(
                reference.name.as_ref(),
                "case"
                    | "when"
                    | "then"
                    | "else"
                    | "end"
                    | "'priority1'"
                    | "'excluded'"
                    | "'x'"
                    | "'y'"
                    | "' '"
            )
        }),
        "unexpected SQL keyword refs: {:?}",
        unit.sql_name_refs
    );
}

#[test]
fn resolves_fields_after_inline_select_case_projection_for_sort_and_delete_where() {
    let src = r#"
DATA lv_bj2_max TYPE i.
DATA lc_sts_rep TYPE string.
DATA lr_sr_rule TYPE string.

SELECT rep_evtid,
       evtid,
       rule_type,
       status_rep_evt,
       msguid_out,
       ext_ref_id,
       CASE
         WHEN ext_ref_id = 'PRIORITY1' THEN 'X'
         WHEN ext_ref_id = 'EXCLUDED' THEN 'X'
         WHEN ext_ref_id = 'PRIORITY2' THEN 'Y'
         ELSE ' '
       END AS priority,
       creation_time
  FROM /sttp/rep_evt
  INTO TABLE @DATA(lt_rep_evt)
  UP TO @lv_bj2_max ROWS
  WHERE status_rep_evt = @lc_sts_rep
  AND rule_type IN @lr_sr_rule.

SORT lt_rep_evt BY creation_time ASCENDING.
DELETE lt_rep_evt WHERE ext_ref_id IS INITIAL.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///sql_inline_sort_delete.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnknownField | DiagnosticKind::UnresolvedReference
            ) && (diag.message.contains("creation_time") || diag.message.contains("ext_ref_id"))
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn infers_commented_select_projection_columns_for_inline_target_shape() {
    let src = r#"
DATA lv_bj2_max TYPE i.
DATA lc_sts_rep TYPE string.
DATA lr_sr_rule TYPE string.

SELECT rep_evtid,                                                   "Reporting Event id
       evtid,                                                       "Event id
       rule_type,                                                   "Rule type
       status_rep_evt,                                              "Reporting Event Status
       msguid_out,                                                  "AIF Message ID
       ext_ref_id,                                                  "External Reference ID
       CASE
         WHEN ext_ref_id = 'PRIORITY1' THEN 'X'
         WHEN ext_ref_id = 'EXCLUDED' THEN 'X'
         WHEN ext_ref_id = 'PRIORITY2' THEN 'Y'
         ELSE ' '
       END AS priority,
       creation_time                                                "Creation date time
  FROM /sttp/rep_evt
  INTO TABLE @DATA(lt_rep_evt)
  UP TO @lv_bj2_max ROWS                                            "comment
  WHERE status_rep_evt = @lc_sts_rep
  AND rule_type IN @lr_sr_rule.

SORT lt_rep_evt BY creation_time ASCENDING.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///sql_inline_commented_projection.abap", src, &parsed);

    let lt_rep_evt = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "lt_rep_evt")
        .expect("inline SQL target");
    let structure = unit.structure(lt_rep_evt.structure.expect("inline SQL target structure"));
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "creation_time"),
        "expected creation_time in inferred row shape, structure={structure:?}"
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnknownField | DiagnosticKind::UnresolvedReference
            ) && diag.message.contains("creation_time")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
    assert!(
        !unit.sql_name_refs.iter().any(|reference| {
            reference.name.as_ref().contains("reporting")
                || reference.name.as_ref().contains("creation date time")
        }),
        "unexpected comment refs: {:?}",
        unit.sql_name_refs
    );
}

#[test]
fn infers_inline_assignment_target_table_shape_from_source_table() {
    let src = r#"
DATA lv_bj2_max TYPE i.
DATA lc_sts_rep TYPE string.
DATA lr_sr_rule TYPE string.
DATA ls_priority1 TYPE string.

SELECT rep_evtid,
       ext_ref_id,
       creation_time
  FROM /sttp/rep_evt
  INTO TABLE @DATA(lt_obj_rel)
  UP TO @lv_bj2_max ROWS
  WHERE status_rep_evt = @lc_sts_rep
  AND rule_type IN @lr_sr_rule.

DATA(lt_obj) = lt_obj_rel.
DELETE lt_obj WHERE rep_evtid NE ls_priority1.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///inline_assign_table_shape.abap", src, &parsed);

    let lt_obj = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "lt_obj")
        .expect("inline assignment target");
    let structure = unit.structure(
        lt_obj
            .structure
            .expect("inline assignment target structure"),
    );
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "rep_evtid"),
        "expected rep_evtid in inferred assignment shape, structure={structure:?}"
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnknownField | DiagnosticKind::UnresolvedReference
            ) && diag.message.contains("rep_evtid")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn reports_unverified_open_sql_sources_without_workspace_type() {
    let src = r#"
SELECT * FROM /sttp/unknown_tab INTO TABLE DATA(lt).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///opensql_unverified.abap", src, &parsed);
    assert!(
        unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnverifiedOpenSqlSource
                && diag.message.contains("/sttp/unknown_tab")
        }),
        "expected UnverifiedOpenSqlSource, got {:?}",
        unit.diagnostics
    );
}

#[test]
fn suppresses_unverified_open_sql_when_workspace_type_matches_from_name() {
    let src = r#"
TYPES ty_row TYPE i.
DATA lt TYPE STANDARD TABLE OF ty_row.
SELECT * FROM ty_row INTO TABLE lt.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///opensql_local_type.abap", src, &parsed);
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnverifiedOpenSqlSource),
        "unexpected UnverifiedOpenSqlSource: {:?}",
        unit.diagnostics
    );
}

#[test]
fn reports_invalid_into_table_when_target_is_not_internal_table() {
    let src = r#"
TYPES ty_row TYPE i.
DATA wa TYPE ty_row.
SELECT * FROM ty_row INTO TABLE wa.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///into_table_bad.abap", src, &parsed);
    assert!(
        unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::InvalidOpenSqlIntoTarget && diag.message.contains("wa")
        }),
        "expected InvalidOpenSqlIntoTarget for wa, got {:?}",
        unit.diagnostics
    );
}

#[test]
fn accepts_into_table_when_target_uses_local_table_type_alias() {
    let src = r#"
TYPES: BEGIN OF ty_row,
         id TYPE i,
       END OF ty_row.
TYPES ty_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_rows TYPE ty_rows.
SELECT * FROM ty_row INTO TABLE lt_rows.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///into_table_alias_ok.abap", src, &parsed);
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::InvalidOpenSqlIntoTarget),
        "unexpected InvalidOpenSqlIntoTarget for table alias: {:?}",
        unit.diagnostics
    );
}

#[test]
fn accepts_into_table_when_target_uses_external_ddic_table_type() {
    let main_src = r#"
DATA et_sdr TYPE /sttp/tt_evt_sdr.
SELECT * FROM /sttp/dm_evt_sdr INTO CORRESPONDING FIELDS OF TABLE et_sdr.
"#;
    let row_src = r#"
TYPES: BEGIN OF /sttp/dm_evt_sdr,
         evtid TYPE i,
       END OF /sttp/dm_evt_sdr.
"#;
    let table_src = r#"
TYPES /sttp/tt_evt_sdr TYPE STANDARD TABLE OF /sttp/dm_evt_sdr WITH EMPTY KEY.
"#;
    let main_parse = parse(main_src);
    let row_parse = parse(row_src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///main.abap",
            source: main_src,
            parse: &main_parse,
        },
        ProjectInput {
            uri: "file:///ddic_row.abap",
            source: row_src,
            parse: &row_parse,
        },
        ProjectInput {
            uri: "file:///ddic_table.abap",
            source: table_src,
            parse: &table_parse,
        },
    ]);
    let unit = project.unit_by_uri("file:///main.abap").expect("main unit");
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::InvalidOpenSqlIntoTarget),
        "unexpected InvalidOpenSqlIntoTarget for DDIC table type: {:?}",
        unit.diagnostics
    );
}

#[test]
fn modern_select_from_fields_recognizes_namespaced_source_name() {
    let main_src = r#"
DATA mv_evtid TYPE i.
SELECT SINGLE
  FROM /sttp/rep_evt
  FIELDS rep_evtid,
         rule_type,
         msguid_out
  WHERE evtid = @mv_evtid
  AND rule_type IN (
    @zattp_cl_rs_rule_proc=>gcs_rule_type-shipping,
    @zattp_cl_rs_rule_proc=>gcs_rule_type-transloading )
  AND status_rep_evt = 1
  AND recall_status = 3
  INTO @DATA(ls_rep_evt).
"#;
    let ddic_src = r#"
TYPES: BEGIN OF /sttp/rep_evt,
         evtid TYPE i,
         rep_evtid TYPE i,
         rule_type TYPE i,
         msguid_out TYPE string,
         status_rep_evt TYPE i,
         recall_status TYPE i,
       END OF /sttp/rep_evt.
"#;
    let class_src = r#"
CLASS zattp_cl_rs_rule_proc DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gcs_rule_type,
        shipping TYPE i VALUE 1,
        transloading TYPE i VALUE 2,
      END OF gcs_rule_type.
ENDCLASS.
"#;
    let main_parse = parse(main_src);
    let ddic_parse = parse(ddic_src);
    let class_parse = parse(class_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///main.abap",
            source: main_src,
            parse: &main_parse,
        },
        ProjectInput {
            uri: "file:///ddic_row.abap",
            source: ddic_src,
            parse: &ddic_parse,
        },
        ProjectInput {
            uri: "file:///class.abap",
            source: class_src,
            parse: &class_parse,
        },
    ]);
    let unit = project.unit_by_uri("file:///main.abap").expect("main unit");

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnverifiedOpenSqlSource),
        "unexpected UnverifiedOpenSqlSource: {:?}",
        unit.diagnostics
    );
    assert!(
        unit.sql_name_refs.iter().any(|sql_ref| {
            sql_ref.kind == SqlNameRefKind::Source && sql_ref.name.as_ref() == "/sttp/rep_evt"
        }),
        "expected /sttp/rep_evt source ref, refs={:?}",
        unit.sql_name_refs
    );
}

#[test]
fn resolves_select_into_flat_target_reference() {
    let src = r#"
DATA lt TYPE string.
SELECT * FROM demo INTO lt.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///into_flat.abap", src, &parsed);
    let into_pos = src.find("INTO").expect("INTO");
    assert!(
        unit.references.iter().any(|reference| {
            reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "lt"
                && reference.range.start >= into_pos
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected INTO target lt to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
}

#[test]
fn reports_invalid_into_corresponding_when_target_is_not_structure_like() {
    let src = r#"
TYPES ty_scalar TYPE i.
DATA lv TYPE ty_scalar.
SELECT * FROM ty_scalar INTO CORRESPONDING FIELDS OF lv.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///into_corr.abap", src, &parsed);
    assert!(
        unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::InvalidOpenSqlIntoTarget
                && diag.message.contains("CORRESPONDING")
                && diag.message.contains("lv")
        }),
        "expected InvalidOpenSqlIntoTarget for CORRESPONDING, got {:?}",
        unit.diagnostics
    );
}

#[test]
fn into_corresponding_fields_of_table_does_not_require_structure_metadata_on_symbol() {
    let src = r#"
TYPES ty_row TYPE i.
DATA lt_gs1_gcp TYPE STANDARD TABLE OF ty_row.
SELECT * FROM demo INTO CORRESPONDING FIELDS OF TABLE lt_gs1_gcp.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///into_corr_table.abap", src, &parsed);
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::InvalidOpenSqlIntoTarget
                && diag.message.contains("lt_gs1_gcp")
        }),
        "unexpected InvalidOpenSqlIntoTarget for CORRESPONDING FIELDS OF TABLE: {:?}",
        unit.diagnostics
    );
}

#[test]
fn sort_by_component_does_not_report_unknown_symbol_when_row_structure_unresolved() {
    let src = r#"
FORM f.
  DATA lt_tab TYPE STANDARD TABLE OF /sttp/gs1_gcp.
  SORT lt_tab BY gs1_gcp.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///sort_by_unresolved_row.abap", src, &parsed);
    assert!(
        !unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Value
                && reference.name.as_ref() == "gs1_gcp"
                && reference.resolution.is_none()
        }),
        "BY component must not be collected as an unresolved value reference: refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("unknown symbol 'gs1_gcp'")),
        "unexpected unknown-symbol diagnostic for SORT BY key: {:?}",
        unit.diagnostics
    );
    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_namespace == Namespace::Value
                && access.base_name.as_ref() == "lt_tab"
                && access.field_path.len() == 1
                && access.field_path[0].name.as_ref() == "gs1_gcp"
        }),
        "expected SORT BY to record field access on the internal table: {:?}",
        unit.field_accesses
    );
}

#[test]
fn sort_by_unknown_component_reports_unknown_field_when_row_structure_known() {
    let src = r#"
TYPES: BEGIN OF ty_row,
         gs1_gcp TYPE string,
       END OF ty_row.
FORM f.
  DATA lt_tab TYPE STANDARD TABLE OF ty_row.
  SORT lt_tab BY no_such_field.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///sort_by_bad_field.abap", src, &parsed);
    assert!(
        unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("no_such_field")
        }),
        "expected UnknownField for invalid SORT BY key: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_concatenate_operands_and_selector_sources() {
    let src = r#"
CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    METHODS to_string RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
  METHOD to_string.
  ENDMETHOD.
ENDCLASS.

DATA lo_prog TYPE REF TO zcl_program.
DATA mv_odlv TYPE string.
DATA lv_delivery_msg TYPE string.

CONCATENATE lo_prog->to_string( ) mv_odlv INTO lv_delivery_msg SEPARATED BY ': '.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///concatenate_stmt.abap", src, &parsed);

    for name in ["lo_prog", "mv_odlv", "lv_delivery_msg"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved CONCATENATE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_namespace == Namespace::Value
                && access.base_name.as_ref() == "lo_prog"
                && access
                    .field_path
                    .iter()
                    .any(|segment| segment.name.as_ref() == "to_string")
        }),
        "expected CONCATENATE selector metadata, accesses={:?}",
        unit.field_accesses
    );

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("lo_prog")
                    || diag.message.contains("mv_odlv")
                    || diag.message.contains("lv_delivery_msg"))
        }),
        "unexpected CONCATENATE diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_split_source_separator_and_into_targets() {
    let src = r#"
DATA iv_sgtin TYPE string.
DATA lv_part_1 TYPE string.
DATA lv_part_2 TYPE string.
DATA lv_part_3 TYPE string.
DATA lv_part_4 TYPE string.
DATA lv_part_5 TYPE string.
DATA lv_part_6 TYPE string.

SPLIT iv_sgtin
AT    ':'
INTO  lv_part_1
      lv_part_2
      lv_part_3
      lv_part_4
      lv_part_5
      lv_part_6
IN CHARACTER MODE.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///split_stmt.abap", src, &parsed);

    for name in [
        "iv_sgtin",
        "lv_part_1",
        "lv_part_2",
        "lv_part_3",
        "lv_part_4",
        "lv_part_5",
        "lv_part_6",
    ] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved SPLIT reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("iv_sgtin") || diag.message.contains("lv_part_"))
        }),
        "unexpected SPLIT diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_assign_to_inline_field_symbol() {
    let src = r#"
DATA lv_value TYPE string.

ASSIGN lv_value TO FIELD-SYMBOL(<lv_value>).
<lv_value> = <lv_value>.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///assign_inline_fs.abap", src, &parsed);

    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == abap_symbols::SymbolKind::FieldSymbol && symbol.name.as_ref() == "<lv_value>"
    }));

    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "lv_value"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));

    let fs_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "<lv_value>"
        })
        .collect();
    assert_eq!(
        fs_refs.len(),
        2,
        "expected body references, got {fs_refs:?}"
    );
    assert!(
        fs_refs
            .iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected ASSIGN target references to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("lv_value") || diag.message.contains("<lv_value>"))
        }),
        "unexpected ASSIGN diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_assign_component_to_inline_field_symbol() {
    let src = r#"
FIELD-SYMBOLS <ls_outbound> TYPE any.

ASSIGN COMPONENT 'EVENT_LIST'
  OF STRUCTURE <ls_outbound>
  TO FIELD-SYMBOL(<ls_event>).
<ls_event> = <ls_event>.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///assign_component_inline_fs.abap", src, &parsed);

    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == abap_symbols::SymbolKind::FieldSymbol && symbol.name.as_ref() == "<ls_event>"
    }));

    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "<ls_outbound>"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));

    let fs_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "<ls_event>"
        })
        .collect();
    assert_eq!(
        fs_refs.len(),
        2,
        "expected body references, got {fs_refs:?}"
    );
    assert!(
        fs_refs
            .iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected ASSIGN COMPONENT target references to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("<ls_outbound>") || diag.message.contains("<ls_event>"))
        }),
        "unexpected ASSIGN COMPONENT diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_assert_is_assigned_field_symbol_condition() {
    let src = r#"
FIELD-SYMBOLS <ls_outbound> TYPE any.

ASSERT <ls_outbound> IS ASSIGNED.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///assert_is_assigned.abap", src, &parsed);

    let refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "<ls_outbound>"
        })
        .collect();
    assert_eq!(
        refs.len(),
        1,
        "expected ASSERT field-symbol reference, got {refs:?}"
    );
    assert!(
        refs.iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected ASSERT field-symbol reference to resolve, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("<ls_outbound>")
        }),
        "unexpected ASSERT diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_data_ref_dereference_selector_field_access() {
    let src = r#"
TYPES: BEGIN OF ty_row,
         name TYPE string,
       END OF ty_row.
DATA lr_row TYPE REF TO ty_row.
DATA lv_name TYPE string.

lv_name = lr_row->*-name.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///deref_selector.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "lr_row"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_name.as_ref() == "lr_row"
                && access.field_path.len() == 2
                && access.field_path[0].is_deref()
                && access.field_path[1].name.as_ref() == "name"
        }),
        "expected dereference selector path, accesses={:?}",
        unit.field_accesses
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("lr_row") || diag.message.contains("name"))
        }),
        "unexpected dereference selector diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn infers_assign_inline_field_symbol_from_dereferenced_ref() {
    let src = r#"
TYPES: BEGIN OF ty_row,
         name TYPE string,
       END OF ty_row.
DATA lr_row TYPE REF TO ty_row.

ASSIGN lr_row->* TO FIELD-SYMBOL(<ls_row>).
<ls_row>-name = 'demo'.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///assign_deref_inline_fs.abap", src, &parsed);

    let fs_symbol = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "<ls_row>")
        .expect("inline dereferenced field-symbol target");
    assert_eq!(fs_symbol.kind, abap_symbols::SymbolKind::FieldSymbol);
    assert!(
        fs_symbol.structure.is_some(),
        "expected inferred structure for dereferenced target, symbol={fs_symbol:?}"
    );

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_name.as_ref() == "<ls_row>"
                && access.field_path.len() == 1
                && access.field_path[0].name.as_ref() == "name"
        }),
        "expected field-symbol field access, accesses={:?}",
        unit.field_accesses
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("<ls_row>") || diag.message.contains("name"))
        }),
        "unexpected dereferenced ASSIGN diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn infers_loop_inline_target_ref_type_from_source_table() {
    let src = r#"
CLASS zcl_stmt DEFINITION.
  PUBLIC SECTION.
    METHODS to_string RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_stmt IMPLEMENTATION.
  METHOD to_string.
    rv_text = 'stmt'.
  ENDMETHOD.
ENDCLASS.

TYPES ty_stmt_tab TYPE STANDARD TABLE OF REF TO zcl_stmt WITH DEFAULT KEY.
DATA lt_statements TYPE ty_stmt_tab.
DATA lv_text TYPE string.

LOOP AT lt_statements INTO DATA(lo_stmt).
  lv_text = lo_stmt->to_string( ).
ENDLOOP.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///loop_ref_rows.abap", src, &parsed);

    let lo_stmt = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "lo_stmt")
        .expect("inline LOOP ref target");
    let declared_type = lo_stmt
        .declared_type
        .as_ref()
        .expect("loop target declared type");
    assert!(declared_type.is_ref);
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "zcl_stmt");
    assert!(declared_type.field_path.is_empty());

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_name.as_ref() == "lo_stmt"
                && access
                    .field_path
                    .iter()
                    .any(|segment| segment.name.as_ref() == "to_string")
        }),
        "expected loop target method access, accesses={:?}",
        unit.field_accesses
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("lo_stmt") || diag.message.contains("to_string"))
        }),
        "unexpected LOOP ref diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn infers_loop_inline_target_structure_from_source_table() {
    let src = r#"
TYPES: BEGIN OF ty_row,
         name TYPE string,
       END OF ty_row.
TYPES ty_row_tab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA lt_rows TYPE ty_row_tab.

LOOP AT lt_rows INTO DATA(ls_row).
  ls_row-name = 'demo'.
ENDLOOP.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///loop_struct_rows.abap", src, &parsed);

    let ls_row = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "ls_row")
        .expect("inline LOOP structured target");
    let structure_id = ls_row.structure.expect("loop target structure");
    let structure = unit.structure(structure_id);
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "name")
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("ls_row") || diag.message.contains("name"))
        }),
        "unexpected LOOP structure diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn reports_duplicate_declarations() {
    let src = "DATA lv_value TYPE i. DATA lv_value TYPE i.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///dupe.abap", src, &parsed);

    assert!(
        unit.diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::DuplicateDeclaration)
    );
}

#[test]
fn class_definition_and_implementation_are_not_duplicate_class_declarations() {
    let src = r#"
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    METHODS exec.
  PRIVATE SECTION.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
  METHOD exec.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///class_impl.abap", src, &parsed);

    let class_decls = unit
        .symbols
        .iter()
        .filter(|s| s.kind == abap_symbols::SymbolKind::Class && s.name.as_ref() == "some_class")
        .count();
    assert_eq!(class_decls, 1);
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::DuplicateDeclaration),
        "CLASS ... IMPLEMENTATION is not a second declaration of the class"
    );

    let class = unit
        .symbols
        .iter()
        .find(|s| s.kind == abap_symbols::SymbolKind::Class && s.name.as_ref() == "some_class")
        .expect("class symbol");
    let impl_header_ref = unit
        .references
        .iter()
        .find(|r| {
            r.name.as_ref() == "some_class"
                && r.kind == ReferenceKind::TypeRef
                && r.namespace == Namespace::Type
        })
        .expect("implementation header class name should be a type reference");
    assert_eq!(
        impl_header_ref.resolution,
        Some(Resolution::Symbol(SymbolHandle {
            unit: unit.unit_id,
            symbol: class.id,
        }))
    );
}

#[test]
fn collects_public_static_method_metadata_from_class_definition() {
    let src = r#"
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec
      IMPORTING
        iv_value TYPE i.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///class_methods.abap", src, &parsed);

    let class_symbol = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Class && symbol.name.as_ref() == "some_class"
        })
        .expect("class symbol");
    let member = unit
        .class_member(class_symbol.id, "exec")
        .expect("class method metadata");
    assert_eq!(member.kind, abap_symbols::ClassMemberKind::Method);
    assert_eq!(member.visibility, abap_symbols::Visibility::Public);
    assert!(member.is_static);
    assert!(member.signature.contains("CLASS-METHODS exec"));
    assert!(member.signature.contains("iv_value TYPE i"));
}

#[test]
fn resolves_public_class_data_static_members() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA gv_value TYPE i.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lv_value TYPE i.
  lv_value = zcl_demo=>gv_value.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///class_data_static_member.abap", src, &parsed);

    let class_symbol = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Class && symbol.name.as_ref() == "zcl_demo"
        })
        .expect("class symbol");
    let member = unit
        .class_member(class_symbol.id, "gv_value")
        .expect("class attribute metadata");
    assert_eq!(member.kind, abap_symbols::ClassMemberKind::Attribute);
    assert_eq!(member.visibility, abap_symbols::Visibility::Public);
    assert!(member.is_static);
    assert!(member.signature.contains("CLASS-DATA gv_value TYPE i"));

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_namespace == Namespace::Type
                && access.base_name.as_ref() == "zcl_demo"
                && access
                    .field_path
                    .iter()
                    .any(|segment| segment.name.as_ref() == "gv_value")
        }),
        "expected static class-data selector metadata, accesses={:?}",
        unit.field_accesses
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("gv_value")
        }),
        "unexpected class-data diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_grouped_class_constants_static_access() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_s_tab,
        p0 TYPE i VALUE 1,
        p1 TYPE i VALUE 2,
      END OF gc_s_tab .
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lv TYPE i.
  lv = zcl_demo=>gc_s_tab-p0.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///grouped_class_constants.abap", src, &parsed);

    let class_symbol = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Class && symbol.name.as_ref() == "zcl_demo"
        })
        .expect("class symbol");
    let member = unit
        .class_member(class_symbol.id, "gc_s_tab")
        .expect("grouped constants structure should be a class attribute member");
    assert_eq!(member.kind, abap_symbols::ClassMemberKind::Attribute);
    assert!(member.is_static);

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_namespace == Namespace::Type
                && access.base_name.as_ref() == "zcl_demo"
                && access
                    .field_path
                    .iter()
                    .any(|s| s.name.as_ref() == "gc_s_tab")
                && access.field_path.iter().any(|s| s.name.as_ref() == "p0")
        }),
        "expected static grouped-constants selector metadata, accesses={:?}",
        unit.field_accesses
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("p0")
        }),
        "unexpected unknown-field diagnostic for grouped constant component: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_grouped_class_constants_via_me_instance_selector() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS get_value RETURNING VALUE(rv_value) TYPE i.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF gcs_struct_field,
        p0 TYPE i VALUE 1,
        p1 TYPE i VALUE 2,
      END OF gcs_struct_field.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD get_value.
    rv_value = me->gcs_struct_field-p0.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///grouped_class_constants_me.abap", src, &parsed);

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_namespace == Namespace::Value
                && access.base_name.as_ref() == "me"
                && access
                    .field_path
                    .iter()
                    .any(|s| s.name.as_ref() == "gcs_struct_field")
                && access.field_path.iter().any(|s| s.name.as_ref() == "p0")
        }),
        "expected instance selector metadata for grouped constants, accesses={:?}",
        unit.field_accesses
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField
                && (diag.message.contains("gcs_struct_field") || diag.message.contains("p0"))
        }),
        "unexpected unknown-field diagnostic for grouped constant component via me->: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_class_definition_members_inside_implementation_methods() {
    let src = r#"
CLASS zcl_ast_node DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS to_string ABSTRACT
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_ast_node IMPLEMENTATION.
ENDCLASS.

CLASS zcl_number_literal DEFINITION INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_value TYPE string.
    METHODS to_string REDEFINITION.
  PRIVATE SECTION.
    DATA mv_value TYPE string.
ENDCLASS.

CLASS zcl_number_literal IMPLEMENTATION.
  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD to_string.
    rv_text = mv_value.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///class_member_resolution.abap", src, &parsed);

    for name in ["iv_value", "mv_value", "rv_text"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected `{name}` references to resolve, got refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected unresolved diagnostic for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_inherited_instance_methods_for_ref_typed_variables() {
    let src = r#"
CLASS zcl_ast_node DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS to_string ABSTRACT
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_ast_node IMPLEMENTATION.
ENDCLASS.

CLASS zcl_expr DEFINITION ABSTRACT INHERITING FROM zcl_ast_node.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.

DATA lo_expr TYPE REF TO zcl_expr.
DATA lv_text TYPE string.
lv_text = lo_expr->to_string( ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///inherited_methods.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("to_string")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_me_as_current_instance_in_instance_methods() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS send_notification
      EXPORTING ev_response_string TYPE string.
    METHODS exec.
  PRIVATE SECTION.
    DATA mv_state TYPE string.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD send_notification.
    ev_response_string = mv_state.
  ENDMETHOD.

  METHOD exec.
    CALL METHOD me->send_notification
      IMPORTING
        ev_response_string = DATA(lv_response).
    mv_state = lv_response.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///instance_me.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "me"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.field_accesses.iter().any(|access| {
        access.base_namespace == Namespace::Value
            && access.base_name.as_ref() == "me"
            && access
                .field_path
                .iter()
                .any(|segment| segment.name.as_ref() == "send_notification")
    }));

    let lv_response = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable
                && symbol.name.as_ref() == "lv_response"
        })
        .expect("inline IMPORTING target");
    let declared_type = lv_response
        .declared_type
        .as_ref()
        .expect("inline target declared type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "string");
    assert!(declared_type.field_path.is_empty());

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("me")
                || diag.message.contains("send_notification")
                || diag.message.contains("lv_response"))
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_implicit_legacy_call_method_named_arguments() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS send_notification
      EXPORTING ev_response_string TYPE string.
    METHODS exec.
  PRIVATE SECTION.
    DATA mv_state TYPE string.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD send_notification.
    ev_response_string = mv_state.
  ENDMETHOD.

  METHOD exec.
    CALL METHOD send_notification
      IMPORTING
        ev_response_string = DATA(lv_response).
    mv_state = lv_response.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///implicit_legacy_call_method.abap", src, &parsed);

    let lv_response = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable
                && symbol.name.as_ref() == "lv_response"
        })
        .expect("implicit CALL METHOD inline target");
    let declared_type = lv_response
        .declared_type
        .as_ref()
        .expect("implicit CALL METHOD target declared type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "string");
    assert!(declared_type.field_path.is_empty());

    assert!(unit.named_arguments.iter().any(|argument| {
        argument.name.as_ref() == "ev_response_string"
            && argument.section == Some(abap_symbols::NamedArgumentSection::Importing)
    }));

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            )
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn does_not_resolve_me_in_static_methods() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD exec.
    me->exec( ).
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///static_me.abap", src, &parsed);

    assert!(unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("me")
    }));
}

#[test]
fn ignores_trailing_method_modifier_in_returning_type_recovery() {
    let src = r#"
CLASS zcl_ast_node DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string
      ABSTRACT.
ENDCLASS.

CLASS zcl_ast_node IMPLEMENTATION.
  METHOD to_string.
    rv_text = ``.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    assert!(
        parsed.errors.iter().any(|err| {
            err.message
                .contains("method modifier ABSTRACT must appear before parameter declarations")
        }),
        "{:?}",
        parsed.errors
    );

    let unit = analyze_unit("file:///method_modifier_recovery.abap", src, &parsed);
    let rv_text = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Parameter && symbol.name.as_ref() == "rv_text"
        })
        .expect("rv_text parameter");
    let declared_type = rv_text
        .declared_type
        .as_ref()
        .expect("rv_text declared type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "string");
    assert!(declared_type.field_path.is_empty());
}

#[test]
fn reports_unknown_static_class_member_access() {
    let src = r#"
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec.
ENDCLASS.

some_class=>exe( ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///unknown_static_method.abap", src, &parsed);

    assert!(unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnknownField
            && diag.message.contains("unknown static member 'exe'")
    }));
}

#[test]
fn reports_wrong_namespace_for_type_references() {
    let src = "DATA foo TYPE i. DATA bar TYPE foo.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///namespace.abap", src, &parsed);

    assert!(
        unit.diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::WrongNamespace)
    );
}

#[test]
fn resolves_like_line_of_internal_table_variable_for_field_symbols() {
    let src = "\
DATA lt_tab TYPE STANDARD TABLE OF string.\n\
FIELD-SYMBOLS <ls> LIKE LINE OF lt_tab.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///like_line_of_fs.abap", src, &parsed);

    let lt_ref = unit.references.iter().find(|reference| {
        reference.kind == ReferenceKind::TypeRef
            && reference.name.as_ref() == "lt_tab"
            && reference.namespace == Namespace::Value
    });
    assert!(
        lt_ref.is_some_and(|r| matches!(r.resolution, Some(Resolution::Symbol(_)))),
        "expected lt_tab LIKE reference to resolve as a data object, got {:?}",
        lt_ref.map(|r| &r.resolution)
    );
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::WrongNamespace && diag.message.contains("lt_tab")
    }));
}

#[test]
fn recovers_after_syntax_errors_and_keeps_later_resolution() {
    let src = "DATA broken TYPE string\nDATA ok TYPE i.\nok = 1.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///recover.abap", src, &parsed);

    assert!(!parsed.errors.is_empty());
    assert!(
        unit.symbols
            .iter()
            .any(|symbol| symbol.name.as_ref() == "ok")
    );
    assert!(unit.references.iter().any(|reference| {
        reference.name.as_ref() == "ok"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
}

#[test]
fn resolves_builtin_abap_boolean_constants_and_type() {
    let src = "DATA lv_flag TYPE abap_bool. lv_flag = abap_true. IF lv_flag = abap_false. ENDIF.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///builtins.abap", src, &parsed);

    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == abap_symbols::SymbolKind::BuiltinType && symbol.name.as_ref() == "abap_bool"
    }));
    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == abap_symbols::SymbolKind::BuiltinConstant
            && symbol.name.as_ref() == "abap_true"
    }));
    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == abap_symbols::SymbolKind::BuiltinConstant
            && symbol.name.as_ref() == "abap_false"
    }));
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Type
            && reference.name.as_ref() == "abap_bool"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "abap_true"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "abap_false"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.message.contains("abap_true")
            || diag.message.contains("abap_false")
            || diag.message.contains("abap_bool")
    }));
}

#[test]
fn resolves_any_as_builtin_type() {
    let src = "DATA lr_any TYPE any.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///builtin_any.abap", src, &parsed);

    let any_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.kind == ReferenceKind::TypeRef
                && reference.namespace == Namespace::Type
                && reference.name.as_ref() == "any"
        })
        .expect("any type reference");
    assert_eq!(any_ref.resolution, Some(Resolution::BuiltinType));
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("any"))
    );
}

#[test]
fn resolves_builtin_sy_and_common_ddic_aliases() {
    let src = "\
DATA lv_tabix TYPE sy-tabix.\n\
DATA lv_guid TYPE guid.\n\
DATA lv_flag TYPE xfeld.\n\
DATA lv_table TYPE tabname.\n\
DATA lv_objcl TYPE cdobjectcl.\n\
DATA lv_fm TYPE rs38l_fnam.\n\
DATA lv_mem TYPE memoryid.\n\
IF sy-subrc = 0.\n\
  lv_tabix = sy-tabix.\n\
ENDIF.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///legacy_builtins.abap", src, &parsed);

    for (name, kind) in [
        ("sy", abap_symbols::SymbolKind::BuiltinVariable),
        ("syst", abap_symbols::SymbolKind::BuiltinType),
        ("guid", abap_symbols::SymbolKind::BuiltinType),
        ("xfeld", abap_symbols::SymbolKind::BuiltinType),
        ("tabname", abap_symbols::SymbolKind::BuiltinType),
        ("cdobjectcl", abap_symbols::SymbolKind::BuiltinType),
        ("rs38l_fnam", abap_symbols::SymbolKind::BuiltinType),
        ("memoryid", abap_symbols::SymbolKind::BuiltinType),
    ] {
        assert!(
            unit.symbols
                .iter()
                .any(|symbol| symbol.kind == kind && symbol.name.as_ref() == name)
        );
    }

    for type_name in [
        "guid",
        "xfeld",
        "tabname",
        "cdobjectcl",
        "rs38l_fnam",
        "memoryid",
    ] {
        assert!(unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Type
                && reference.name.as_ref() == type_name
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }));
    }

    let sy_refs = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.name.as_ref() == "sy"
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        })
        .count();
    assert!(sy_refs >= 2);
    let sy_symbol = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "sy")
        .expect("builtin sy symbol");
    let sy_structure = unit.structure(sy_symbol.structure.expect("sy structure metadata"));
    assert!(
        sy_structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "subrc")
    );
    assert!(
        sy_structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "msgv1")
    );
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("sy")
                || diag.message.contains("guid")
                || diag.message.contains("memoryid"))
    );
}

#[test]
fn resolves_more_legacy_builtin_aliases() {
    let src = "\
DATA lv_msg TYPE symsgv.\n\
DATA lv_date TYPE sydatum.\n\
DATA lv_ts TYPE timestamp.\n\
DATA lv_cursor TYPE cursor.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///more_builtins.abap", src, &parsed);

    for name in ["symsgv", "sydatum", "timestamp", "cursor"] {
        assert!(unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Type
                && reference.name.as_ref() == name
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }));
    }
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.message.contains("symsgv")
            || diag.message.contains("sydatum")
            || diag.message.contains("timestamp")
            || diag.message.contains("cursor")
    }));
}

#[test]
fn rejects_unknown_sy_field_access() {
    let src = "IF sy-nope = 0. ENDIF. DATA lv_bad TYPE sy-nope.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///bad_sy.abap", src, &parsed);

    assert!(
        unit.diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnknownField && diag.message.contains("nope"))
    );
}

#[test]
fn collects_user_defined_begin_of_type_structure() {
    let src = "\
TYPES: BEGIN OF ty_pair,\n\
         a TYPE i,\n\
         b TYPE string,\n\
       END OF ty_pair.\n\
DATA ls_pair TYPE ty_pair.\n\
ls_pair-a = 1.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///ty_pair.abap", src, &parsed);

    let ty_pair = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::TypeDef && symbol.name.as_ref() == "ty_pair"
        })
        .expect("structured type symbol");
    let structure = unit.structure(ty_pair.structure.expect("type structure metadata"));
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "a")
    );
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "b")
    );

    let ls_pair = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == "ls_pair"
        })
        .expect("typed variable");
    assert_eq!(ls_pair.structure, ty_pair.structure);
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnknownField
                || diag.message.contains("ty_pair"))
    );
}

#[test]
fn validates_user_defined_begin_of_data_components() {
    let src = "\
DATA: BEGIN OF ls_date,\n\
        yyyy(4),\n\
        mm(2),\n\
      END OF ls_date.\n\
ls_date-yyyy = '2026'.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///ls_date.abap", src, &parsed);

    let ls_date = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == "ls_date"
        })
        .expect("structured data symbol");
    let structure = unit.structure(ls_date.structure.expect("data structure metadata"));
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "yyyy")
    );
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "mm")
    );
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnknownField
                || diag.message.contains("ls_date"))
    );
}

#[test]
fn resolves_type_component_access_for_user_defined_structures() {
    let src = "\
TYPES: BEGIN OF ty_pair,\n\
         a TYPE i,\n\
       END OF ty_pair.\n\
DATA lv_value TYPE ty_pair-a.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///type_component.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "ty_pair"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference
                || diag.kind == DiagnosticKind::UnknownField)
    );
}

#[test]
fn rejects_unknown_user_defined_structure_fields() {
    let src = "\
TYPES: BEGIN OF ty_pair,\n\
         a TYPE i,\n\
       END OF ty_pair.\n\
DATA ls_pair TYPE ty_pair.\n\
ls_pair-missing = 1.\n\
DATA lv_value TYPE ty_pair-missing.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///missing_field.abap", src, &parsed);

    let unknown_field_diags: Vec<_> = unit
        .diagnostics
        .iter()
        .filter(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("missing")
        })
        .collect();
    assert_eq!(unknown_field_diags.len(), 2);
}

#[test]
fn carries_nested_structure_metadata_on_fields() {
    let src = "\
TYPES: BEGIN OF ty_outer,\n\
         BEGIN OF inner,\n\
           a TYPE i,\n\
         END OF inner,\n\
       END OF ty_outer.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///nested_type.abap", src, &parsed);

    let ty_outer = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::TypeDef && symbol.name.as_ref() == "ty_outer"
        })
        .expect("outer type");
    let outer_structure = unit.structure(ty_outer.structure.expect("outer structure"));
    let inner_field = outer_structure
        .fields
        .iter()
        .find(|field| field.name.as_ref() == "inner")
        .expect("inner field");
    let inner_structure = unit.structure(inner_field.structure.expect("inner structure"));
    assert!(
        inner_structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "a")
    );
}

#[test]
fn validates_nested_selector_chains_for_user_defined_structures() {
    let src = "\
TYPES: BEGIN OF ty_outer,\n\
         BEGIN OF inner,\n\
           a TYPE i,\n\
         END OF inner,\n\
       END OF ty_outer.\n\
DATA ls_outer TYPE ty_outer.\n\
ls_outer-inner-a = 1.\n\
DATA lv_value TYPE ty_outer-inner-a.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///nested_chain.abap", src, &parsed);

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnknownField
                || diag.kind == DiagnosticKind::UnresolvedReference)
    );
}

#[test]
fn rejects_unknown_nested_structure_fields() {
    let src = "\
TYPES: BEGIN OF ty_outer,\n\
         BEGIN OF inner,\n\
           a TYPE i,\n\
         END OF inner,\n\
       END OF ty_outer.\n\
DATA ls_outer TYPE ty_outer.\n\
ls_outer-inner-missing = 1.\n\
DATA lv_value TYPE ty_outer-inner-missing.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///nested_missing.abap", src, &parsed);

    let unknown_field_diags: Vec<_> = unit
        .diagnostics
        .iter()
        .filter(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("missing")
        })
        .collect();
    assert_eq!(unknown_field_diags.len(), 2);
}

#[test]
fn exposes_declared_field_type_metadata_for_scalar_fields() {
    let src = "\
TYPES: BEGIN OF ty_pair,\n\
         a TYPE i,\n\
       END OF ty_pair.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///field_type_scalar.abap", src, &parsed);

    let ty_pair = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::TypeDef && symbol.name.as_ref() == "ty_pair"
        })
        .expect("pair type");
    let pair_structure = unit.structure(ty_pair.structure.expect("pair structure"));
    let field = pair_structure
        .fields
        .iter()
        .find(|field| field.name.as_ref() == "a")
        .expect("scalar field");
    let type_ref = field.type_ref.as_ref().expect("field type ref");
    assert_eq!(type_ref.namespace, Namespace::Type);
    assert_eq!(type_ref.base_name.as_ref(), "i");
    assert!(type_ref.field_path.is_empty());
    assert!(field.structure.is_none());
}

#[test]
fn resolves_structured_fields_declared_via_type_reference() {
    let src = "\
TYPES: BEGIN OF ty_inner,\n\
         a TYPE i,\n\
       END OF ty_inner.\n\
TYPES: BEGIN OF ty_outer,\n\
         inner TYPE ty_inner,\n\
       END OF ty_outer.\n\
DATA ls_outer TYPE ty_outer.\n\
ls_outer-inner-a = 1.\n\
DATA lv_value TYPE ty_outer-inner-a.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///field_type_struct.abap", src, &parsed);

    let ty_outer = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::TypeDef && symbol.name.as_ref() == "ty_outer"
        })
        .expect("outer type");
    let outer_structure = unit.structure(ty_outer.structure.expect("outer structure"));
    let inner_field = outer_structure
        .fields
        .iter()
        .find(|field| field.name.as_ref() == "inner")
        .expect("inner field");
    let type_ref = inner_field.type_ref.as_ref().expect("inner type ref");
    assert_eq!(type_ref.namespace, Namespace::Type);
    assert_eq!(type_ref.base_name.as_ref(), "ty_inner");
    let inner_structure = unit.structure(inner_field.structure.expect("resolved inner structure"));
    assert!(
        inner_structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "a")
    );
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnknownField
                || diag.kind == DiagnosticKind::UnresolvedReference)
    );
}

#[test]
fn flattens_include_type_members_in_block_structured_types() {
    let src = "\
TYPES: BEGIN OF ty_inner,\n\
         a TYPE i,\n\
       END OF ty_inner.\n\
TYPES: BEGIN OF ty_outer.\n\
INCLUDE TYPE ty_inner AS inner.\n\
TYPES: b TYPE string,\n\
END OF ty_outer.\n\
DATA ls_outer TYPE ty_outer.\n\
ls_outer-a = 1.\n\
DATA lv_value TYPE ty_outer-a.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///include_type_block.abap", src, &parsed);

    let ty_outer = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::TypeDef && symbol.name.as_ref() == "ty_outer"
        })
        .expect("outer type");
    let outer_structure = unit.structure(ty_outer.structure.expect("outer structure"));
    assert!(
        outer_structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "a"),
        "expected included field, fields={:?}",
        outer_structure.fields
    );
    assert!(
        outer_structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "b"),
        "expected local field, fields={:?}",
        outer_structure.fields
    );
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnknownField
                || diag.kind == DiagnosticKind::UnresolvedReference),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn collects_structured_types_with_keyword_named_fields_and_table_aliases() {
    let src = "\
TYPES:\n\
  BEGIN OF ts_cust_info,\n\
    type     TYPE char1,\n\
    root     TYPE string,\n\
    tag_path TYPE string,\n\
    intkey   TYPE /sttp/e_intkey,\n\
    attr_int TYPE /sttp/e_attr_int,\n\
  END OF ts_cust_info.\n\
TYPES:\n\
  tt_cust_info TYPE SORTED TABLE OF ts_cust_info WITH NON-UNIQUE KEY primary_key COMPONENTS root.\n\
DATA ls_cust_info TYPE ts_cust_info.\n\
DATA lt_cust_info TYPE tt_cust_info.\n\
ls_cust_info-type = 'A'.\n\
ls_cust_info-root = 'node'.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///structured_types_keyword_fields.abap", src, &parsed);

    let ts_cust_info = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::TypeDef
                && symbol.name.as_ref() == "ts_cust_info"
        })
        .expect("structured type symbol");
    let structure = unit.structure(ts_cust_info.structure.expect("structure metadata"));
    let type_field = structure
        .fields
        .iter()
        .find(|field| field.name.as_ref() == "type")
        .expect("field named type");
    assert_eq!(
        type_field
            .type_ref
            .as_ref()
            .expect("field declared type")
            .base_name
            .as_ref(),
        "char1"
    );
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "root"),
        "expected root field, fields={:?}",
        structure.fields
    );

    let tt_cust_info = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::TypeDef
                && symbol.name.as_ref() == "tt_cust_info"
        })
        .expect("table alias symbol");
    let declared_type = tt_cust_info
        .declared_type
        .as_ref()
        .expect("table alias declared type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "ts_cust_info");

    let ls_cust_info = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable
                && symbol.name.as_ref() == "ls_cust_info"
        })
        .expect("structured variable");
    assert_eq!(ls_cust_info.structure, ts_cust_info.structure);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField
                || (diag.kind == DiagnosticKind::UnresolvedReference
                    && (diag.message.contains("ts_cust_info")
                        || diag.message.contains("tt_cust_info")
                        || diag.message.contains("ls_cust_info")
                        || diag.message.contains("lt_cust_info")))
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn exposes_structure_field_query_info() {
    let src = "\
TYPES: BEGIN OF ty_inner,\n\
         a TYPE i,\n\
       END OF ty_inner.\n\
TYPES: BEGIN OF ty_outer,\n\
         inner TYPE ty_inner,\n\
         label TYPE string,\n\
       END OF ty_outer.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///field_query.abap", src, &parsed);

    let ty_outer = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::TypeDef && symbol.name.as_ref() == "ty_outer"
        })
        .expect("outer type");
    let outer_structure_id = ty_outer.structure.expect("outer structure");

    let fields = unit.structure_field_infos(outer_structure_id);
    assert_eq!(fields.len(), 2);

    let inner = unit
        .structure_field_info(outer_structure_id, "inner")
        .expect("inner field info");
    assert_eq!(
        inner.type_ref.expect("inner type ref").base_name.as_ref(),
        "ty_inner"
    );
    assert!(matches!(
        inner.shape,
        StructureFieldShape::Structured { .. }
    ));

    let label = unit
        .resolve_structure_field_path(outer_structure_id, &["label"])
        .expect("label field info");
    assert!(matches!(label.shape, StructureFieldShape::Scalar));

    let nested = unit
        .resolve_structure_field_path(outer_structure_id, &["inner", "a"])
        .expect("nested field info");
    assert_eq!(nested.name.as_ref(), "a");
    assert!(matches!(nested.shape, StructureFieldShape::Scalar));
}

#[test]
fn resolves_new_constructor_type_reference() {
    let src = r#"
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec.
  PRIVATE SECTION.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
  METHOD exec.
    DATA(lo_instance) = NEW some_class( ).
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///new_ctor.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("some_class")
        }),
        "unexpected unresolved diagnostic: {:?}",
        unit.diagnostics
    );

    let class = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Class && symbol.name.as_ref() == "some_class"
        })
        .expect("class symbol");

    let ctor_ref = unit
        .references
        .iter()
        .rfind(|reference| {
            reference.kind == ReferenceKind::TypeRef
                && reference.namespace == Namespace::Type
                && reference.name.as_ref() == "some_class"
        })
        .expect("constructor type reference");

    assert_eq!(
        ctor_ref.resolution,
        Some(Resolution::Symbol(SymbolHandle {
            unit: unit.unit_id,
            symbol: class.id,
        }))
    );
}

#[test]
fn resolves_create_object_target_with_ref_to_type() {
    let src = r#"
CLASS some_class DEFINITION.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
ENDCLASS.

DATA lo_instance TYPE REF TO some_class.
CREATE OBJECT lo_instance.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///create_object.abap", src, &parsed);

    let lo_instance = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable
                && symbol.name.as_ref() == "lo_instance"
        })
        .expect("lo_instance variable");
    let declared_type = lo_instance
        .declared_type
        .as_ref()
        .expect("declared ref type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert!(declared_type.is_ref);
    assert_eq!(declared_type.base_name.as_ref(), "some_class");
    assert!(declared_type.field_path.is_empty());

    let create_object_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.kind == ReferenceKind::Identifier
                && reference.namespace == Namespace::Value
                && reference.name.as_ref() == "lo_instance"
        })
        .expect("create object target reference");
    assert_eq!(
        create_object_ref.resolution,
        Some(Resolution::Symbol(SymbolHandle {
            unit: unit.unit_id,
            symbol: lo_instance.id,
        }))
    );

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("lo_instance") || diag.message.contains("some_class"))
        }),
        "unexpected unresolved diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_superclass_reference_and_signature_parameters_in_class_definition() {
    let src = r#"
CLASS some_base DEFINITION.
ENDCLASS.

CLASS some_base IMPLEMENTATION.
ENDCLASS.

CLASS some_sub DEFINITION INHERITING FROM some_base.
  PUBLIC SECTION.
    METHODS exec
      IMPORTING iv_input TYPE i
      RETURNING VALUE(rv_output) TYPE string.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///class_signature.abap", src, &parsed);

    let base = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Class && symbol.name.as_ref() == "some_base"
        })
        .expect("base class symbol");

    let superclass_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.kind == ReferenceKind::TypeRef
                && reference.namespace == Namespace::Type
                && reference.name.as_ref() == "some_base"
                && src[reference.range.clone()].eq_ignore_ascii_case("some_base")
        })
        .expect("superclass reference");
    assert_eq!(
        superclass_ref.resolution,
        Some(Resolution::Symbol(SymbolHandle {
            unit: unit.unit_id,
            symbol: base.id,
        }))
    );

    for (name, type_name) in [("iv_input", "i"), ("rv_output", "string")] {
        let param = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == abap_symbols::SymbolKind::Parameter && symbol.name.as_ref() == name
            })
            .expect("signature parameter symbol");
        let declared_type = param
            .declared_type
            .as_ref()
            .expect("parameter declared type");
        assert_eq!(declared_type.namespace, Namespace::Type);
        assert_eq!(declared_type.base_name.as_ref(), type_name);
    }
}

#[test]
fn collects_public_method_metadata_from_interface_definition() {
    let src = r#"
INTERFACE zif_demo.
  METHODS exec
    IMPORTING iv_value TYPE i
    RETURNING VALUE(rv_text) TYPE string.
ENDINTERFACE.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///interface_method_metadata.abap", src, &parsed);

    let interface_symbol = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Interface && symbol.name.as_ref() == "zif_demo"
        })
        .expect("interface symbol");
    let member = unit
        .class_member(interface_symbol.id, "exec")
        .expect("interface method metadata");
    assert_eq!(member.kind, abap_symbols::ClassMemberKind::Method);
    assert_eq!(member.visibility, abap_symbols::Visibility::Public);
    assert!(!member.is_static);
    assert!(member.signature.contains("METHODS exec"));
    assert_eq!(member.parameters.len(), 2);
    assert_eq!(member.parameters[0].name.as_ref(), "iv_value");
    assert_eq!(member.parameters[1].name.as_ref(), "rv_text");
}

#[test]
fn collects_public_attribute_metadata_from_interface_definition() {
    let src = r#"
INTERFACE zif_demo.
  DATA gv_value TYPE i.
ENDINTERFACE.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///interface_attr_metadata.abap", src, &parsed);

    let interface_symbol = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Interface && symbol.name.as_ref() == "zif_demo"
        })
        .expect("interface symbol");
    let member = unit
        .class_member(interface_symbol.id, "gv_value")
        .expect("interface attribute metadata");
    assert_eq!(member.kind, abap_symbols::ClassMemberKind::Attribute);
    assert_eq!(member.visibility, abap_symbols::Visibility::Public);
    assert!(!member.is_static);
    assert!(member.signature.contains("DATA gv_value TYPE i"));
}

#[test]
fn create_data_stmt_resolves_target_and_dynamic_type_operand() {
    let src = r#"
TYPES: BEGIN OF ty_finf,
         ddicstructure TYPE string,
       END OF ty_finf.

DATA lr_sap_data TYPE REF TO data.
DATA ls_finf TYPE ty_finf.

CREATE DATA lr_sap_data TYPE (ls_finf-ddicstructure).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///create_data.abap", src, &parsed);

    let lr_sap_data = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable
                && symbol.name.as_ref() == "lr_sap_data"
        })
        .expect("lr_sap_data variable");

    let create_data_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.kind == ReferenceKind::Identifier
                && reference.namespace == Namespace::Value
                && reference.name.as_ref() == "lr_sap_data"
        })
        .expect("create data target reference");
    assert_eq!(
        create_data_ref.resolution,
        Some(Resolution::Symbol(SymbolHandle {
            unit: unit.unit_id,
            symbol: lr_sap_data.id,
        }))
    );

    assert!(unit.references.iter().any(|reference| {
        reference.kind == ReferenceKind::Identifier
            && reference.namespace == Namespace::Value
            && reference.name.as_ref() == "ls_finf"
    }));

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("lr_sap_data") || diag.message.contains("ls_finf"))
        }),
        "unexpected unresolved diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn create_data_stmt_resolves_like_operand_as_value_reference() {
    let src = r#"
DATA mo_outbound TYPE REF TO data.
DATA iv_data TYPE string.

CREATE DATA mo_outbound LIKE iv_data.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///create_data_like.abap", src, &parsed);

    let mo_outbound = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable
                && symbol.name.as_ref() == "mo_outbound"
        })
        .expect("mo_outbound variable");
    let iv_data = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == "iv_data"
        })
        .expect("iv_data variable");

    let outbound_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.kind == ReferenceKind::Identifier
                && reference.namespace == Namespace::Value
                && reference.name.as_ref() == "mo_outbound"
        })
        .expect("create data target reference");
    assert_eq!(
        outbound_ref.resolution,
        Some(Resolution::Symbol(SymbolHandle {
            unit: unit.unit_id,
            symbol: mo_outbound.id,
        }))
    );

    let like_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.kind == ReferenceKind::Identifier
                && reference.namespace == Namespace::Value
                && reference.name.as_ref() == "iv_data"
        })
        .expect("create data like reference");
    assert_eq!(
        like_ref.resolution,
        Some(Resolution::Symbol(SymbolHandle {
            unit: unit.unit_id,
            symbol: iv_data.id,
        }))
    );

    assert!(!unit.diagnostics.iter().any(|diag| {
        (diag.kind == DiagnosticKind::UnresolvedReference
            || diag.kind == DiagnosticKind::WrongNamespace)
            && (diag.message.contains("mo_outbound") || diag.message.contains("iv_data"))
    }));
}

#[test]
fn resolves_constructor_signature_parameter_type_references() {
    let src = r#"
CLASS zcl_expr DEFINITION ABSTRACT.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.

CLASS zcl_binary_expr DEFINITION INHERITING FROM zcl_expr.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_left  TYPE REF TO zcl_expr
        iv_op    TYPE string
        io_right TYPE REF TO zcl_expr.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///constructor_signature.abap", src, &parsed);

    let ctor_start = src
        .find("METHODS constructor")
        .expect("constructor signature start");

    let class_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.kind == ReferenceKind::TypeRef
                && reference.namespace == Namespace::Type
                && reference.name.as_ref() == "zcl_expr"
                && reference.range.start > ctor_start
        })
        .collect();
    assert_eq!(
        class_refs.len(),
        2,
        "expected constructor type refs, refs={class_refs:?}"
    );
    assert!(
        class_refs
            .iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected resolved class type refs, refs={class_refs:?}"
    );

    let string_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.kind == ReferenceKind::TypeRef
                && reference.namespace == Namespace::Type
                && reference.name.as_ref() == "string"
                && reference.range.start > ctor_start
        })
        .expect("constructor string type reference");
    assert_eq!(string_ref.resolution, Some(Resolution::BuiltinType));
}

#[test]
fn resolves_inherited_class_type_refs_across_project_units() {
    let base_src = r#"
CLASS /cdbasis/cl_messages DEFINITION.
  PUBLIC SECTION.
    TYPES te_loglevel TYPE numc1.
ENDCLASS.
"#;
    let sub_src = r#"
CLASS /sttp/cl_messages DEFINITION INHERITING FROM /cdbasis/cl_messages.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gcs_log_level,
        very_high TYPE te_loglevel VALUE 1,
        high      TYPE te_loglevel VALUE 2,
      END OF gcs_log_level.
ENDCLASS.
"#;
    let base_parse = parse(base_src);
    let sub_parse = parse(sub_src);
    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///base.abap",
            source: base_src,
            parse: &base_parse,
        },
        ProjectInput {
            uri: "file:///sub.abap",
            source: sub_src,
            parse: &sub_parse,
        },
    ]);
    let unit = project.unit_by_uri("file:///sub.abap").expect("sub unit");

    let refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.kind == ReferenceKind::TypeRef
                && reference.namespace == Namespace::Type
                && reference.name.as_ref() == "te_loglevel"
        })
        .collect();
    assert_eq!(refs.len(), 2, "expected inherited type refs, refs={refs:?}");
    assert!(
        refs.iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_)))),
        "expected inherited type refs to resolve, refs={refs:?}"
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("te_loglevel")
        }),
        "unexpected unresolved inherited type diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn accepts_inherited_class_type_static_selector_refs_across_project_units() {
    let base_src = r#"
CLASS /cdbasis/cl_messages DEFINITION.
  PUBLIC SECTION.
    TYPES te_loglevel TYPE numc1.
    TYPES te_typelevel TYPE char1.
ENDCLASS.
"#;
    let sub_src = r#"
CLASS /sttp/cl_messages DEFINITION INHERITING FROM /cdbasis/cl_messages.
  PUBLIC SECTION.
    CLASS-METHODS create_new_handler_att
      IMPORTING
        iv_loglevel TYPE /sttp/cl_messages=>te_loglevel
        iv_typelevel TYPE /sttp/cl_messages=>te_typelevel.
ENDCLASS.
"#;
    let base_parse = parse(base_src);
    let sub_parse = parse(sub_src);
    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///base_static_type.abap",
            source: base_src,
            parse: &base_parse,
        },
        ProjectInput {
            uri: "file:///sub_static_type.abap",
            source: sub_src,
            parse: &sub_parse,
        },
    ]);
    let unit = project
        .unit_by_uri("file:///sub_static_type.abap")
        .expect("sub unit");

    let class_type_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Type
                && reference.name.as_ref() == "/sttp/cl_messages"
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        })
        .collect();
    assert_eq!(
        class_type_refs.len(),
        2,
        "expected resolved static type selectors, refs={:?}",
        unit.references
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField
                && diag.message.contains("unknown static member")
                && (diag.message.contains("te_loglevel") || diag.message.contains("te_typelevel"))
        }),
        "unexpected inherited static type selector diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_method_raising_exception_type_refs() {
    let src = r#"
CLASS zcx_resume DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run RAISING resumable(zcx_resume).
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///method_raising.abap", src, &parsed);

    let refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.kind == ReferenceKind::TypeRef
                && reference.namespace == Namespace::Type
                && reference.name.as_ref() == "zcx_resume"
        })
        .collect();
    assert_eq!(
        refs.len(),
        1,
        "expected one raising type ref, refs={refs:?}"
    );
    assert!(
        matches!(refs[0].resolution, Some(Resolution::Symbol(_))),
        "expected resolved raising type ref, refs={refs:?}"
    );
}

#[test]
fn resolves_constructor_arguments_and_token_only_statement_references() {
    let src = r#"
CLASS zcl_ast_node DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS to_string ABSTRACT
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_ast_node IMPLEMENTATION.
ENDCLASS.

CLASS zcl_expr DEFINITION ABSTRACT INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.

CLASS zcl_stmt DEFINITION ABSTRACT INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_stmt IMPLEMENTATION.
ENDCLASS.

CLASS zcl_assign_stmt DEFINITION INHERITING FROM zcl_stmt.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_name TYPE string
        io_expr TYPE REF TO zcl_expr.
ENDCLASS.

CLASS zcl_assign_stmt IMPLEMENTATION.
ENDCLASS.

CLASS zcl_print_stmt DEFINITION INHERITING FROM zcl_stmt.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_expr TYPE REF TO zcl_expr.
ENDCLASS.

CLASS zcl_print_stmt IMPLEMENTATION.
ENDCLASS.

CLASS zcl_program DEFINITION INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
    METHODS add_statement
      IMPORTING io_stmt TYPE REF TO zcl_stmt.
    METHODS to_string REDEFINITION.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
  METHOD add_statement.
  ENDMETHOD.

  METHOD to_string.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_expr1 TYPE REF TO zcl_expr.
  DATA lo_assign TYPE REF TO zcl_assign_stmt.
  DATA lo_print TYPE REF TO zcl_print_stmt.
  DATA lo_prog TYPE REF TO zcl_program.

  lo_assign = NEW zcl_assign_stmt(
    iv_name = 'x'
    io_expr = lo_expr1
  ).
  lo_prog->add_statement( lo_assign ).
  lo_prog->add_statement( lo_print ).
  WRITE / lo_prog->to_string( ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///simple_stmt_refs.abap", src, &parsed);

    for name in ["lo_expr1", "lo_assign", "lo_print", "lo_prog"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.kind == ReferenceKind::Identifier
                    && reference.namespace == Namespace::Value
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved value reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    let ctor_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.kind == ReferenceKind::TypeRef
                && reference.namespace == Namespace::Type
                && reference.name.as_ref() == "zcl_assign_stmt"
        })
        .expect("constructor type reference");
    assert!(matches!(ctor_ref.resolution, Some(Resolution::Symbol(_))));

    for member_name in ["add_statement", "to_string"] {
        assert!(
            unit.field_accesses.iter().any(|access| {
                access.base_name.as_ref() == "lo_prog"
                    && access
                        .field_path
                        .iter()
                        .any(|segment| segment.name.as_ref() == member_name)
            }),
            "expected selector metadata for `{member_name}`"
        );
    }

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference),
        "unexpected unresolved diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_cond_constructor_clause_references() {
    let src = r#"
CLASS lcl_element DEFINITION.
  PUBLIC SECTION.
    DATA prefix TYPE string.
    DATA name TYPE string.
    METHODS get_value
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS lcl_element IMPLEMENTATION.
  METHOD get_value.
    rv_text = name.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_element TYPE REF TO lcl_element.
  DATA lv_text TYPE string.

  lv_text = COND string(
    WHEN lo_element->prefix = '' THEN lo_element->get_value( )
    ELSE lo_element->name
  ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///cond_constructor.abap", src, &parsed);

    assert!(
        unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "lo_element"
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected resolved COND value references, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );

    for member_name in ["prefix", "get_value", "name"] {
        assert!(
            unit.field_accesses.iter().any(|access| {
                access.base_name.as_ref() == "lo_element"
                    && access
                        .field_path
                        .iter()
                        .any(|segment| segment.name.as_ref() == member_name)
            }),
            "expected selector metadata for `{member_name}` in COND expression"
        );
    }

    for keyword in ["when", "then", "else"] {
        assert!(
            !unit
                .references
                .iter()
                .any(|reference| reference.name.as_ref() == keyword),
            "unexpected keyword reference `{keyword}`, refs={:?}",
            unit.references
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(keyword)
            }),
            "unexpected unresolved diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference),
        "unexpected unresolved diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_value_table_expression_optional_without_value_keyword_reference() {
    let src = r#"
TYPES: BEGIN OF ty_item,
         objid TYPE string,
       END OF ty_item.

DATA it_obj_itm TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.
DATA is_obj_ids TYPE ty_item.
DATA(ls_obj_itm) = VALUE #( it_obj_itm[ objid = is_obj_ids-objid ] OPTIONAL ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_optional.abap", src, &parsed);

    for name in ["it_obj_itm", "is_obj_ids"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved VALUE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    assert!(
        !unit
            .references
            .iter()
            .any(|reference| reference.name.as_ref() == "value"),
        "unexpected VALUE keyword reference, refs={:?}",
        unit.references
    );

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference),
        "unexpected unresolved diagnostics: {:?}",
        unit.diagnostics
    );

    let ls_obj_itm = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "ls_obj_itm")
        .expect("inline value target");
    let structure = unit.structure(ls_obj_itm.structure.expect("inferred row structure"));
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "objid"),
        "expected objid in inferred VALUE row shape, structure={structure:?}"
    );
}

#[test]
fn resolves_value_for_binding_and_base_expression_references() {
    let src = r#"
TYPES: BEGIN OF ty_row,
         objid TYPE string,
       END OF ty_row.

DATA lt_sequen_buff TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA mt_obj_ids_native TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

lt_sequen_buff = VALUE #( BASE lt_sequen_buff
                          FOR ls_obj IN mt_obj_ids_native
                          ( objid = ls_obj-objid ) ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_for.abap", src, &parsed);

    for name in ["lt_sequen_buff", "mt_obj_ids_native", "ls_obj"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved VALUE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    for keyword in ["base", "for", "in"] {
        assert!(
            !unit
                .references
                .iter()
                .any(|reference| reference.name.as_ref() == keyword),
            "unexpected VALUE keyword reference `{keyword}`, refs={:?}",
            unit.references
        );
    }

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference),
        "unexpected unresolved diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_value_for_where_clause_bindings() {
    let src = r#"
TYPES: BEGIN OF ty_row,
         objid TYPE string,
       END OF ty_row.

DATA lv_parent TYPE string.
DATA mt_obj_ids_native TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

DATA(lt_filtered) = VALUE #(
  FOR ls_obj IN mt_obj_ids_native
  WHERE ( objid <> lv_parent )
  ( ls_obj-objid ) ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_for_where.abap", src, &parsed);

    for name in ["mt_obj_ids_native", "objid", "lv_parent", "ls_obj"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved VALUE WHERE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("objid") || diag.message.contains("lv_parent"))
        }),
        "unexpected VALUE WHERE diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_implicit_method_named_arguments_inside_value_for_where_body() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS decode_objid_gs1_rs
      IMPORTING iv_objid TYPE string
      RETURNING VALUE(rv_gs1) TYPE string.
    METHODS exec.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD decode_objid_gs1_rs.
    rv_gs1 = iv_objid.
  ENDMETHOD.

  METHOD exec.
    TYPES: BEGIN OF ty_row,
             objid TYPE string,
           END OF ty_row.
    DATA lv_parent TYPE string.
    DATA mt_obj_ids_native TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

    DATA(lt_filtered) = VALUE #(
      FOR ls_obj IN mt_obj_ids_native
      WHERE ( objid <> lv_parent )
      ( decode_objid_gs1_rs( iv_objid = ls_obj-objid ) ) ).
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_for_where_implicit_method.abap", src, &parsed);

    assert!(
        unit.named_arguments.iter().any(|access| {
            access.name.as_ref() == "iv_objid"
                && matches!(
                    access.target,
                    abap_symbols::NamedArgumentTarget::ImplicitMethod { ref method_name }
                        if method_name.as_ref() == "decode_objid_gs1_rs"
                )
        }),
        "expected iv_objid named argument access, named_args={:?} diagnostics={:?}",
        unit.named_arguments,
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("iv_objid") || diag.message.contains("objid"))
        }),
        "unexpected VALUE WHERE implicit-method diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn infers_inline_assignment_target_table_shape_from_value_base_constructor() {
    let src = r#"
TYPES: BEGIN OF ty_row,
         objid TYPE string,
       END OF ty_row.

DATA lt_sequen_buff TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA mt_obj_ids_native TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

DATA(lt_new) = VALUE #( BASE lt_sequen_buff
                        FOR ls_obj IN mt_obj_ids_native
                        ( objid = ls_obj-objid ) ).
DELETE lt_new WHERE objid = ''.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_base_infer.abap", src, &parsed);

    let lt_new = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "lt_new")
        .expect("inline value table target");
    let structure = unit.structure(lt_new.structure.expect("inferred table row structure"));
    assert!(
        structure
            .fields
            .iter()
            .any(|field| field.name.as_ref() == "objid"),
        "expected objid in inferred VALUE BASE row shape, structure={structure:?}"
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnknownField | DiagnosticKind::UnresolvedReference
            ) && diag.message.contains("objid")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn infers_explicit_value_constructor_as_non_ref_type() {
    let src = r#"
TYPES: BEGIN OF ty_item,
         objid TYPE string,
       END OF ty_item.

DATA(ls_obj_itm) = VALUE ty_item( objid = 'X' ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_explicit_type.abap", src, &parsed);

    let ls_obj_itm = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "ls_obj_itm")
        .expect("inline explicit value target");
    let declared_type = ls_obj_itm
        .declared_type
        .as_ref()
        .expect("declared type inferred from VALUE");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert!(!declared_type.is_ref);
    assert_eq!(declared_type.base_name.as_ref(), "ty_item");
    assert!(declared_type.field_path.is_empty());
}

#[test]
fn resolves_value_lines_of_references_inside_cond_constructor() {
    let src = r#"
TYPES ty_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY.

DATA lt_source TYPE ty_tab.
DATA lv_from TYPE i VALUE 1.
DATA lv_to TYPE i VALUE 2.
DATA lv_ok TYPE abap_bool VALUE abap_true.
DATA lv_extra TYPE string VALUE `x`.

DATA(lt_result) = COND ty_tab(
  WHEN lv_ok = abap_true THEN VALUE #(
    ( LINES OF lt_source FROM lv_from TO lv_to USING KEY primary_key )
    ( lv_extra )
  )
  ELSE VALUE #( ) ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_lines_of_cond.abap", src, &parsed);

    for name in ["lt_source", "lv_from", "lv_to", "lv_ok", "lv_extra"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved VALUE/COND reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    for keyword in ["lines", "of", "from", "to", "using", "key", "primary_key"] {
        assert!(
            !unit
                .references
                .iter()
                .any(|reference| reference.name.as_ref() == keyword),
            "unexpected LINES OF keyword reference `{keyword}`, refs={:?}",
            unit.references
        );
    }

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference),
        "unexpected unresolved diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_value_for_let_bindings_from_project_shape() {
    let src = r#"
TYPES: BEGIN OF ty_obj,
         objid TYPE string,
       END OF ty_obj,
       ty_obj_tab TYPE STANDARD TABLE OF ty_obj WITH EMPTY KEY,
       BEGIN OF ty_selopt,
         sign TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
         low TYPE string,
       END OF ty_selopt,
       rseloption TYPE STANDARD TABLE OF ty_selopt WITH EMPTY KEY.

DATA it_objids TYPE ty_obj_tab.

DATA(lr_obj_ids) = VALUE rseloption(
  FOR ls_ids IN it_objids
    LET s = 'I'
        o = 'EQ'
    IN sign = s
       option = o
       ( low = ls_ids-objid ) ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_for_let.abap", src, &parsed);

    for name in ["it_objids", "ls_ids", "s", "o"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved VALUE FOR LET reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    for keyword in ["for", "let", "in"] {
        assert!(
            !unit
                .diagnostics
                .iter()
                .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference
                    && diag.message.contains(keyword)),
            "unexpected unresolved diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_cond_let_bindings_and_field_symbols() {
    let src = r#"
TYPES:
  BEGIN OF date,
    year  TYPE c LENGTH 4,
    month TYPE c LENGTH 2,
    day   TYPE c LENGTH 2,
  END OF date,
  dates TYPE TABLE OF date WITH EMPTY KEY.

DATA dates TYPE dates.
DATA(isodate) = COND string(
  WHEN sy-index > 0 THEN LET <date> = dates[ sy-index ]
                             sep = '-'
                         IN <date>-year && sep && <date>-month && sep && <date>-day
  ELSE `` ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///cond_let_field_symbol.abap", src, &parsed);

    for name in ["dates", "sy", "<date>", "sep"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved COND LET reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    assert!(
        unit.symbols.iter().any(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::FieldSymbol && symbol.name.as_ref() == "<date>"
        }),
        "expected LET field symbol declaration, symbols={:?}",
        unit.symbols
    );
}

#[test]
fn resolves_cond_leading_let_bindings() {
    let src = r#"
DATA(lv_text) = COND string(
  LET noon = '120000'
  IN
  WHEN sy-timlo < noon THEN |AM|
  ELSE |PM| ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///cond_leading_let.abap", src, &parsed);

    for name in ["sy", "noon"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved COND leading LET reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    assert!(
        unit.symbols
            .iter()
            .any(|symbol| symbol.name.as_ref() == "noon"),
        "expected LET symbol declaration, symbols={:?}",
        unit.symbols
    );

    for keyword in ["let", "in", "when", "then", "else"] {
        assert!(
            !unit
                .references
                .iter()
                .any(|reference| reference.name.as_ref() == keyword),
            "unexpected keyword reference `{keyword}`, refs={:?}",
            unit.references
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(keyword)
            }),
            "unexpected unresolved diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference),
        "unexpected unresolved diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_bare_method_calls_inside_cond_and_value_let_expressions() {
    let src = r#"
TYPES stringtab TYPE STANDARD TABLE OF string WITH EMPTY KEY.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
  PRIVATE SECTION.
    METHODS get_objid
      IMPORTING iv_raw TYPE string
      RETURNING VALUE(rv_text) TYPE string.
    METHODS get_object_hry
      IMPORTING it_objid TYPE STANDARD TABLE OF string
      RETURNING VALUE(rt_objid) TYPE STANDARD TABLE OF string.
    METHODS get_unavailable_obj_pda
      IMPORTING it_child_obj TYPE STANDARD TABLE OF string
      RETURNING VALUE(rt_objid) TYPE STANDARD TABLE OF string.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA(iv_raw) = `A`.
    DATA(result) = LET lt_extpda2 =
      get_unavailable_obj_pda(
        it_child_obj = get_object_hry(
          it_objid = VALUE stringtab( ( get_objid( iv_raw = iv_raw ) ) ) ) )
      IN
      COND stringtab(
        WHEN lt_extpda2 IS INITIAL
        THEN VALUE #( ( get_objid( iv_raw = iv_raw ) ) )
        ELSE VALUE #( ) ).
  ENDMETHOD.

  METHOD get_objid.
    rv_text = iv_raw.
  ENDMETHOD.

  METHOD get_object_hry.
    rt_objid = it_objid.
  ENDMETHOD.

  METHOD get_unavailable_obj_pda.
    rt_objid = it_child_obj.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///cond_value_method_calls.abap", src, &parsed);

    for name in ["get_unavailable_obj_pda", "get_object_hry", "get_objid"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Routine
                    && reference.kind == ReferenceKind::RoutineCall
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved routine call for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    for name in ["iv_raw", "lt_extpda2"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved value reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    for name in ["get_unavailable_obj_pda", "get_object_hry", "get_objid"] {
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected unresolved diagnostic for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn suppresses_dependent_field_reference_when_base_type_is_unknown() {
    let src = r#"
DATA is_response TYPE /sttp/unknown_response.
DATA(lt_codes) = VALUE #( is_response-data[ 1 ]-kodovi OPTIONAL ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///unknown_base_type_field.abap", src, &parsed);

    assert!(
        unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("/sttp/unknown_response")
        }),
        "expected unknown type diagnostic, diagnostics={:?}",
        unit.diagnostics
    );

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("kodovi")),
        "unexpected dependent diagnostic for `kodovi`, diagnostics={:?}",
        unit.diagnostics
    );
}

#[test]
fn treats_text_pool_selector_base_as_builtin_value_symbol() {
    let src = r#"
DATA(lv_status) = COND string(
  WHEN abap_true = abap_true THEN TEXT-005
  ELSE TEXT-006 ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///text_pool_cond.abap", src, &parsed);

    assert!(
        unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "text"
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected resolved builtin TEXT base reference, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("unknown symbol 'text'")
        }),
        "unexpected TEXT base unresolved diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn suppresses_cond_clause_keywords_inside_value_named_argument() {
    let src = r#"
DATA(is_response) = VALUE stringtab( ).
DATA(ls_item) = VALUE string(
  LET lv_text = `fallback` IN
  COND #( WHEN abap_true = abap_true THEN lv_text ELSE TEXT-001 ) ).

DATA(ls_row) = VALUE stringtab(
  ( COND #( WHEN abap_true = abap_true THEN `ok` ELSE TEXT-001 ) ) ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_named_arg_cond_keywords.abap", src, &parsed);

    for keyword in ["when", "then", "else"] {
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(keyword)
            }),
            "unexpected unresolved diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_value_let_with_string_templates_without_literal_diagnostics() {
    let src = r#"
TYPES: stringtab TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA(lt_text) = VALUE stringtab(
  LET it = `be`
  IN ( |To { it } is to do| )
     ( |To do is to { it }| ) ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_let_templates.abap", src, &parsed);

    assert!(
        unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "it"
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected resolved LET template binding, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("is to do")
                    || diag.message.contains("to do is to")
                    || diag.message.contains("unknown symbol ' '"))
        }),
        "unexpected template-literal unresolved diagnostics: {:?}",
        unit.diagnostics
    );

    let it_symbol = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "it")
        .expect("LET variable");
    let declared_type = it_symbol.declared_type.as_ref().expect("LET inferred type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "string");
    assert!(!declared_type.is_ref);
    assert!(declared_type.field_path.is_empty());
}

#[test]
fn resolves_value_conditional_for_iteration_bindings() {
    let src = r#"
DATA lv_limit TYPE i VALUE 3.
DATA(text) = VALUE stringtab(
  FOR n = 1 UNTIL n > lv_limit
  ( |{ n }| ) ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///value_conditional_for.abap", src, &parsed);

    for name in ["lv_limit", "n"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved conditional FOR reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    for keyword in ["for", "until"] {
        assert!(
            !unit
                .diagnostics
                .iter()
                .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference
                    && diag.message.contains(keyword)),
            "unexpected unresolved diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }

    let n_symbol = unit
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "n")
        .expect("conditional FOR variable");
    let declared_type = n_symbol
        .declared_type
        .as_ref()
        .expect("conditional FOR variable type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "i");
    assert!(!declared_type.is_ref);
    assert!(declared_type.field_path.is_empty());
}

#[test]
fn resolves_reduce_for_iteration_and_accumulator_bindings() {
    let src = r#"
TYPES ty_inttab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

DATA lt_rep TYPE ty_inttab.
DATA(lv_rep) = REDUCE i(
  INIT x = 0
  FOR wa IN lt_rep
  NEXT x = x + wa ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///reduce_for.abap", src, &parsed);

    for name in ["lt_rep", "wa", "x"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved REDUCE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    for keyword in ["init", "for", "in", "next"] {
        assert!(
            !unit
                .diagnostics
                .iter()
                .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference
                    && diag.message.contains(keyword)),
            "unexpected unresolved diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_reduce_for_where_clause_bindings() {
    let src = r#"
TYPES: BEGIN OF ty_rep,
         status_rep_evt TYPE i,
         objid TYPE i,
       END OF ty_rep.
TYPES ty_rep_tab TYPE STANDARD TABLE OF ty_rep WITH EMPTY KEY.
TYPES ty_objid_tab TYPE STANDARD TABLE OF ty_rep WITH EMPTY KEY.

DATA lt_rep TYPE ty_rep_tab.
DATA ls_obj_ids TYPE ty_rep.
DATA(lv_rep) = REDUCE i(
  INIT x = 0
  FOR wa IN lt_rep
      WHERE ( status_rep_evt <> 1
              AND objid = ls_obj_ids-objid )
  NEXT x = x + 1 ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///reduce_for_where.abap", src, &parsed);

    for name in ["lt_rep", "status_rep_evt", "objid", "ls_obj_ids", "x"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved REDUCE WHERE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    for keyword in ["where", "and", "next", "status_rep_evt", "objid"] {
        assert!(
            !unit
                .diagnostics
                .iter()
                .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference
                    && diag.message.contains(keyword)),
            "unexpected unresolved diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn wait_up_to_seconds_stmt_resolves_duration_operand() {
    let src = r#"
FORM run.
  DATA lv_time TYPE i.
  WAIT UP TO lv_time SECONDS.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///wait_stmt.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.kind == ReferenceKind::Identifier
            && reference.name.as_ref() == "lv_time"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    for keyword in ["up", "to", "seconds"] {
        assert!(
            !unit
                .diagnostics
                .iter()
                .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference
                    && diag.message.contains(keyword)),
            "unexpected unresolved diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn delete_table_from_stmt_resolves_table_and_source_operands() {
    let src = r#"
TYPES ty_objid_tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

FORM run.
  DATA ct_objids TYPE ty_objid_tab.
  DATA is_obj_ids TYPE i.
  DELETE TABLE ct_objids FROM is_obj_ids.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///delete_table_from.abap", src, &parsed);

    for name in ["ct_objids", "is_obj_ids"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved DELETE TABLE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }
    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("table")),
        "unexpected unresolved diagnostic for `table`: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_replace_statement_operands_and_targets() {
    let src = r#"
CLASS zattp_cl_rep_constants DEFINITION.
  PUBLIC SECTION.
    CONSTANTS gv_url_locat_replace_from TYPE string VALUE 'from'.
    CONSTANTS gv_url_locat_replace_to TYPE string VALUE 'to'.
ENDCLASS.

CLASS zattp_cl_rep_constants IMPLEMENTATION.
ENDCLASS.

TYPES: BEGIN OF ty_destination,
         content TYPE string,
       END OF ty_destination.

FORM run USING iv_id TYPE string.
  DATA ev_timestamp_iso TYPE string.
  DATA ls_destination TYPE ty_destination.
  FIELD-SYMBOLS <fs_destination> TYPE ty_destination.

  ASSIGN ls_destination TO <fs_destination>.
  REPLACE ',' IN ev_timestamp_iso WITH '.'.
  REPLACE FIRST OCCURRENCE OF zattp_cl_rep_constants=>gv_url_locat_replace_from IN <fs_destination>-content WITH zattp_cl_rep_constants=>gv_url_locat_replace_to.
  REPLACE ALL OCCURRENCES OF '%22' IN iv_id WITH '"' IN CHARACTER MODE.
  REPLACE ALL OCCURRENCES OF REGEX '%2F|%2f' IN iv_id WITH '/' IN CHARACTER MODE.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///replace_stmt.abap", src, &parsed);

    for name in ["ev_timestamp_iso", "iv_id", "<fs_destination>"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved REPLACE reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected REPLACE diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }

    let static_target_refs = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Type
                && reference.kind == ReferenceKind::StaticTarget
                && reference.name.as_ref() == "zattp_cl_rep_constants"
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        })
        .count();
    assert!(
        static_target_refs >= 2,
        "expected resolved static REPLACE targets, refs={:?} diagnostics={:?}",
        unit.references,
        unit.diagnostics
    );

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_namespace == Namespace::Value
                && access.base_name.as_ref() == "<fs_destination>"
                && access
                    .field_path
                    .iter()
                    .any(|segment| segment.name.as_ref() == "content")
        }),
        "expected REPLACE target selector metadata, accesses={:?}",
        unit.field_accesses
    );
    for member_name in ["gv_url_locat_replace_from", "gv_url_locat_replace_to"] {
        assert!(
            unit.field_accesses.iter().any(|access| {
                access.base_namespace == Namespace::Type
                    && access.base_name.as_ref() == "zattp_cl_rep_constants"
                    && access
                        .field_path
                        .iter()
                        .any(|segment| segment.name.as_ref() == member_name)
            }),
            "expected REPLACE static selector metadata for `{member_name}`, accesses={:?}",
            unit.field_accesses
        );
    }

    for literal in ["%22", "\"", "%2f", "/"] {
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(literal)
            }),
            "unexpected unresolved literal diagnostic for `{literal}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_find_first_occurrence_match_offset_statement() {
    let src = r#"
FORM run USING iv_tag_path TYPE string.
  DATA lv_first_sep TYPE int4.

  FIND FIRST OCCURRENCE OF | | IN iv_tag_path MATCH OFFSET lv_first_sep.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///find_stmt.abap", src, &parsed);

    for name in ["iv_tag_path", "lv_first_sep"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.kind == ReferenceKind::Identifier
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved FIND reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected FIND diagnostics for `{name}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn infers_inline_new_ref_type_and_collects_named_argument_accesses() {
    let src = r#"
CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    METHODS add_statement
      IMPORTING io_stmt TYPE string.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_prog) = NEW zcl_program( ).
  lo_prog->add_statement( io_stmt = 'x' ).
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///inline_named_args.abap", src, &parsed);

    let lo_prog = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == "lo_prog"
        })
        .expect("inline variable");
    let declared_type = lo_prog
        .declared_type
        .as_ref()
        .expect("inferred declared type");
    assert!(declared_type.is_ref);
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "zcl_program");

    assert!(unit.named_arguments.iter().any(|access| {
        access.name.as_ref() == "io_stmt"
            && matches!(
                &access.target,
                abap_symbols::NamedArgumentTarget::Method { method_name, .. }
                    if method_name.as_ref() == "add_statement"
            )
    }));
}

#[test]
fn resolves_static_method_call_sections_and_inline_importing_targets() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_event_data
      IMPORTING iv_evtid TYPE i
      EXPORTING es_evt TYPE string.
    CLASS-METHODS get_hash
      IMPORTING iv_text TYPE string
      RETURNING VALUE(rv_hash) TYPE string.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA mv_evtid TYPE i.
  DATA mv_text TYPE string.
  zcl_demo=>get_event_data(
    EXPORTING
      iv_evtid = mv_evtid
    IMPORTING
      es_evt = DATA(ls_evt) ).
  zcl_demo=>get_hash(
    EXPORTING
      iv_text = mv_text
    RECEIVING
      rv_hash = DATA(lv_hash) ).
  ls_evt = `done`.
  lv_hash = mv_text.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///static_method_call_sections.abap", src, &parsed);

    for name in ["ls_evt", "lv_hash"] {
        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == name
            })
            .unwrap_or_else(|| panic!("expected inline variable symbol for `{name}`"));
        let declared_type = symbol
            .declared_type
            .as_ref()
            .unwrap_or_else(|| panic!("expected declared type for `{name}`"));
        assert_eq!(declared_type.namespace, Namespace::Type);
        assert_eq!(declared_type.base_name.as_ref(), "string");
        assert!(declared_type.field_path.is_empty());
    }

    assert!(unit.named_arguments.iter().any(|access| {
        access.name.as_ref() == "es_evt"
            && access.section == Some(abap_symbols::NamedArgumentSection::Importing)
    }));
    assert!(unit.named_arguments.iter().any(|access| {
        access.name.as_ref() == "rv_hash"
            && access.section == Some(abap_symbols::NamedArgumentSection::Receiving)
    }));

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            )
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_legacy_call_method_sections_and_inline_importing_targets() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_event_data
      IMPORTING iv_evtid TYPE i
      EXPORTING es_evt TYPE string.
    CLASS-METHODS get_hash
      IMPORTING iv_text TYPE string
      RETURNING VALUE(rv_hash) TYPE string.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA mv_evtid TYPE i.
  DATA mv_text TYPE string.
  CALL METHOD zcl_demo=>get_event_data
    EXPORTING
      iv_evtid = mv_evtid
    IMPORTING
      es_evt = DATA(ls_evt).
  CALL METHOD zcl_demo=>get_hash
    EXPORTING
      iv_text = mv_text
    RECEIVING
      rv_hash = DATA(lv_hash).
  ls_evt = `done`.
  lv_hash = mv_text.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///legacy_call_method_sections.abap", src, &parsed);

    for name in ["ls_evt", "lv_hash"] {
        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == name
            })
            .unwrap_or_else(|| panic!("expected inline variable symbol for `{name}`"));
        let declared_type = symbol
            .declared_type
            .as_ref()
            .unwrap_or_else(|| panic!("expected declared type for `{name}`"));
        assert_eq!(declared_type.namespace, Namespace::Type);
        assert_eq!(declared_type.base_name.as_ref(), "string");
        assert!(declared_type.field_path.is_empty());
    }

    assert!(unit.named_arguments.iter().any(|access| {
        access.name.as_ref() == "es_evt"
            && access.section == Some(abap_symbols::NamedArgumentSection::Importing)
    }));
    assert!(unit.named_arguments.iter().any(|access| {
        access.name.as_ref() == "rv_hash"
            && access.section == Some(abap_symbols::NamedArgumentSection::Receiving)
    }));

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            )
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_unqualified_method_call_inline_importing_targets() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS check_sequencing_rs
      IMPORTING iv_rule_type TYPE string
      EXPORTING ev_sequencing_error TYPE abap_bool
                ev_sequencing_error_msg TYPE string
                et_objids TYPE stringtab.
    METHODS exec.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD check_sequencing_rs.
  ENDMETHOD.

  METHOD exec.
    DATA lv_rule_type TYPE string.
    check_sequencing_rs(
      EXPORTING
        iv_rule_type = lv_rule_type
      IMPORTING
        ev_sequencing_error = DATA(lv_seq_err)
        ev_sequencing_error_msg = DATA(lv_seq_err_msg)
        et_objids = DATA(lt_seq_objids)
    ).

    lv_seq_err = abap_true.
    lv_seq_err_msg = `done`.
    APPEND `x` TO lt_seq_objids.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit(
        "file:///implicit_method_call_inline_importing.abap",
        src,
        &parsed,
    );

    for (name, type_name) in [
        ("lv_seq_err", "abap_bool"),
        ("lv_seq_err_msg", "string"),
        ("lt_seq_objids", "stringtab"),
    ] {
        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == name
            })
            .unwrap_or_else(|| panic!("expected inline variable symbol for `{name}`"));
        let declared_type = symbol
            .declared_type
            .as_ref()
            .unwrap_or_else(|| panic!("expected declared type for `{name}`"));
        assert_eq!(declared_type.namespace, Namespace::Type);
        assert_eq!(declared_type.base_name.as_ref(), type_name);
    }

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("data")
                    || diag.message.contains("lv_seq_err")
                    || diag.message.contains("lv_seq_err_msg")
                    || diag.message.contains("lt_seq_objids"))
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_unqualified_method_call_inline_importing_targets_with_trailing_comments() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS check_sequencing_rs
      IMPORTING iv_rule_type TYPE string
      EXPORTING ev_sequencing_error TYPE abap_bool
                ev_sequencing_error_msg TYPE string
                et_objids TYPE stringtab.
    METHODS exec.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD check_sequencing_rs.
  ENDMETHOD.

  METHOD exec.
    DATA lv_rule_type TYPE string.
    check_sequencing_rs(
      EXPORTING
        iv_rule_type            = lv_rule_type                 " Type of Rule
      IMPORTING
        ev_sequencing_error     = DATA(lv_seq_err)             " Sequence error
        ev_sequencing_error_msg = DATA(lv_seq_err_msg)         " Sequence error message
        et_objids               = DATA(lt_seq_objids)          " Object Identifiers
    ).

    lv_seq_err = abap_true.
    lv_seq_err_msg = `done`.
    APPEND `x` TO lt_seq_objids.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit(
        "file:///implicit_method_call_inline_importing_comments.abap",
        src,
        &parsed,
    );

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("data")
                    || diag.message.contains("lv_seq_err")
                    || diag.message.contains("lv_seq_err_msg")
                    || diag.message.contains("lt_seq_objids"))
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_legacy_call_method_inline_importing_targets_with_trailing_comments() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS send_notification_acc
      IMPORTING it_acc_obj TYPE string
      EXPORTING ev_rep_status TYPE string
                ev_http_code TYPE string.
    METHODS exec.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD exec.
    DATA lt_obj_comm TYPE string.
    CALL METHOD me->send_notification_acc
      EXPORTING
        it_acc_obj = lt_obj_comm
      IMPORTING
        ev_rep_status = DATA(lv_rep_status) " Reporting Event Status
        ev_http_code = DATA(lv_http_code). " Character Field Length = 10
    lv_rep_status = lv_http_code.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///legacy_call_method_comments.abap", src, &parsed);

    for name in ["lv_rep_status", "lv_http_code"] {
        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == name
            })
            .unwrap_or_else(|| panic!("expected inline variable symbol for `{name}`"));
        let declared_type = symbol
            .declared_type
            .as_ref()
            .unwrap_or_else(|| panic!("expected declared type for `{name}`"));
        assert_eq!(declared_type.namespace, Namespace::Type);
        assert_eq!(declared_type.base_name.as_ref(), "string");
        assert!(declared_type.field_path.is_empty());
    }

    assert!(!unit.diagnostics.iter().any(|diag| {
        matches!(
            diag.kind,
            DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
        )
    }));
}

#[test]
fn legacy_call_method_inline_importing_targets_remain_visible_after_endtry() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS send_notification_acc
      EXPORTING ev_rep_status TYPE string
                ev_http_code TYPE string.
    METHODS exec.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD exec.
    TRY.
        CALL METHOD me->send_notification_acc
          IMPORTING
            ev_rep_status = DATA(lv_rep_status)
            ev_http_code = DATA(lv_http_code).
      CATCH cx_root.
    ENDTRY.

    lv_rep_status = lv_http_code.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit(
        "file:///legacy_call_method_try_visibility.abap",
        src,
        &parsed,
    );

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("lv_rep_status") || diag.message.contains("lv_http_code"))
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn catch_inline_target_remains_visible_after_endtry() {
    let src = r#"
CLASS cx_demo DEFINITION INHERITING FROM cx_root.
ENDCLASS.

CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS exec.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD exec.
    TRY.
        WRITE 'x'.
      CATCH cx_demo INTO DATA(lx_error_handling_general).
        lx_error_handling_general->get_text( ).
    ENDTRY.

    lx_error_handling_general->get_text( ).
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///catch_inline_visibility.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag
                    .message
                    .contains("unknown symbol 'lx_error_handling_general'")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn collects_call_function_sections_without_keyword_diagnostics() {
    let src = r#"
START-OF-SELECTION.
  DATA iv_message TYPE string.
  DATA lt_strings TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  CALL FUNCTION 'SWA_STRING_SPLIT'
    EXPORTING
      input_string           = iv_message
      max_component_length   = 50
    TABLES
      string_components      = lt_strings
    EXCEPTIONS
      OTHERS                 = 1.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///call_function_sections.abap", src, &parsed);

    assert!(unit.named_arguments.iter().any(|access| {
        access.name.as_ref() == "input_string"
            && access.section == Some(abap_symbols::NamedArgumentSection::Exporting)
            && matches!(
                &access.target,
                abap_symbols::NamedArgumentTarget::Function { function_name }
                    if function_name.as_ref() == "swa_string_split"
            )
    }));
    assert!(unit.named_arguments.iter().any(|access| {
        access.name.as_ref() == "string_components"
            && access.section == Some(abap_symbols::NamedArgumentSection::Tables)
    }));

    for keyword in ["FUNCTION", "EXPORTING", "TABLES", "EXCEPTIONS"] {
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(keyword)
            }),
            "unexpected unresolved keyword diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn collects_call_transformation_operands_without_keyword_diagnostics() {
    let src = r#"
START-OF-SELECTION.
  DATA lv_json TYPE string.
  DATA lo_writer TYPE REF TO cl_sxml_string_writer.
  DATA lv_json_hex TYPE xstring.
  DATA ev_data TYPE string.

  CALL TRANSFORMATION /sttp/json_xml_to_upper
    SOURCE XML lv_json
    RESULT XML lo_writer.

  CALL TRANSFORMATION id
    SOURCE XML lv_json_hex
    RESULT result = ev_data.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///call_transformation.abap", src, &parsed);

    for name in ["lv_json", "lo_writer", "lv_json_hex", "ev_data"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected `{name}` reference to resolve, got refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    for keyword in ["TRANSFORMATION", "SOURCE", "RESULT", "XML"] {
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(keyword)
            }),
            "unexpected unresolved keyword diagnostic for `{keyword}`: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn resolves_template_interpolation_references_and_method_accesses() {
    let src = r#"
CLASS zcl_expr DEFINITION.
  PUBLIC SECTION.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
  METHOD to_string.
    rv_text = 'expr'.
  ENDMETHOD.
ENDCLASS.

DATA mo_left TYPE REF TO zcl_expr.
DATA mo_right TYPE REF TO zcl_expr.
DATA mv_op TYPE string.
DATA rv_text TYPE string.

rv_text = |({ mo_left->to_string( ) } { mv_op } { mo_right->to_string( ) })|.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///template_refs.abap", src, &parsed);

    for name in ["mo_left", "mv_op", "mo_right"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.kind == ReferenceKind::Identifier
                    && reference.namespace == Namespace::Value
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved template reference for `{name}`, refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }

    let to_string_accesses = unit
        .field_accesses
        .iter()
        .filter(|access| {
            access
                .field_path
                .iter()
                .any(|segment| segment.name.as_ref() == "to_string")
        })
        .count();
    assert_eq!(
        to_string_accesses, 2,
        "expected two template method accesses"
    );

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            )
        }),
        "unexpected template diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_table_expression_selector_accesses_with_keyword_named_fields() {
    let src = "\
TYPES: BEGIN OF ty_rep,
         type TYPE string,
       END OF ty_rep.
TYPES ty_rep_tab TYPE STANDARD TABLE OF ty_rep WITH EMPTY KEY.
DATA lt_rep TYPE ty_rep_tab.
DATA lv_type TYPE string.

lv_type = lt_rep[ 1 ]-type.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///table_expr_selector.abap", src, &parsed);

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_name.as_ref() == "lt_rep"
                && access
                    .field_path
                    .iter()
                    .map(|segment| segment.name.as_ref())
                    .collect::<Vec<_>>()
                    == vec!["type"]
        }),
        "expected table-expression selector access, accesses={:?}",
        unit.field_accesses
    );
    assert!(
        unit.references.iter().any(|reference| {
            reference.name.as_ref() == "lt_rep"
                && reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected resolved table reference, refs={:?}",
        unit.references
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            )
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_legacy_table_body_operator_in_loop_and_predicate_contexts() {
    let src = "\
DATA lt_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY.
FIELD-SYMBOLS <lv_row> TYPE string.

IF lt_tab[] IS NOT INITIAL.
  LOOP AT lt_tab[] ASSIGNING <lv_row>.
  ENDLOOP.
ENDIF.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///legacy_table_body_operator.abap", src, &parsed);

    let lt_tab_refs = unit
        .references
        .iter()
        .filter(|reference| {
            reference.name.as_ref() == "lt_tab"
                && reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
        })
        .collect::<Vec<_>>();
    assert_eq!(
        lt_tab_refs.len(),
        2,
        "expected both lt_tab[] uses to resolve"
    );
    assert!(
        lt_tab_refs
            .iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::Symbol(_))))
    );
    assert!(
        unit.field_accesses.is_empty(),
        "legacy [] should not produce selector field accesses: {:?}",
        unit.field_accesses
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            )
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_legacy_table_body_operator_in_assignment_describe_and_for_all_entries() {
    let src = "\
DATA it_obj_ids TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA it_src TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA it_dst TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_dummy TYPE string.
DATA lv_lines TYPE i.

it_dst[] = it_src[].
DESCRIBE TABLE it_dst[] LINES lv_lines.

IF it_obj_ids[] IS NOT INITIAL.
  SELECT mandt UP TO 1 ROWS
    FROM t000
    INTO lv_dummy
    FOR ALL ENTRIES IN it_obj_ids[]
    WHERE mandt = it_obj_ids.
  ENDSELECT.
ENDIF.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///legacy_table_body_misc.abap", src, &parsed);

    for name in ["it_src", "it_dst", "lv_lines", "it_obj_ids", "lv_dummy"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.name.as_ref() == name
                    && reference.namespace == Namespace::Value
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "expected resolved reference for `{name}`, refs={:?}",
            unit.references
        );
    }
    assert!(
        unit.field_accesses.is_empty(),
        "legacy [] should not produce selector field accesses: {:?}",
        unit.field_accesses
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            )
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_host_expression_in_for_all_entries_clause() {
    let src = "\
DATA lt_rep_evt TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lt_obj_rel TYPE STANDARD TABLE OF string WITH EMPTY KEY.

SELECT rep_evtid,
       objid
  FROM /sttp/rep_obj_rl
  INTO TABLE @lt_obj_rel
  FOR ALL ENTRIES IN @lt_rep_evt
  WHERE rep_evtid = @lt_rep_evt.
";
    let parsed = parse(src);
    let unit = analyze_unit("file:///host_expr_for_all_entries.abap", src, &parsed);
    let semantic = unit.semantic();
    let for_all_entries_offset = src.find("@lt_rep_evt").expect("for all entries host expr") + 1;

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("unknown symbol '@'")
        }),
        "unexpected unresolved host expression diagnostic: diagnostics={:?}, refs={:?}",
        unit.diagnostics,
        unit.references
    );
    assert!(
        unit.references.iter().any(|reference| {
            reference.name.as_ref() == "lt_rep_evt"
                && reference.namespace == Namespace::Value
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected resolved FOR ALL ENTRIES host reference, refs={:?}",
        unit.references
    );
    let reference = semantic
        .refs()
        .reference_at_offset(for_all_entries_offset)
        .expect("reference at FOR ALL ENTRIES offset");
    assert_eq!(reference.name.as_ref(), "lt_rep_evt");
    assert!(matches!(reference.resolution, Some(Resolution::Symbol(_))));
}

#[test]
fn resolves_selector_chain_ending_with_legacy_table_body_operator() {
    let src = "\
FIELD-SYMBOLS: <fs_choice> TYPE any,
               <fs_destination> TYPE any.

LOOP AT <fs_choice>-object_event-extension-destination_list-destination[] ASSIGNING <fs_destination>.
ENDLOOP.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///selector_legacy_table_body.abap", src, &parsed);

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_name.as_ref() == "<fs_choice>"
                && access
                    .field_path
                    .iter()
                    .map(|segment| segment.name.as_ref())
                    .collect::<Vec<_>>()
                    == vec![
                        "object_event",
                        "extension",
                        "destination_list",
                        "destination",
                    ]
        }),
        "expected selector access ending in legacy [], accesses={:?}",
        unit.field_accesses
    );
    assert!(
        unit.references.iter().any(|reference| {
            reference.name.as_ref() == "<fs_destination>"
                && reference.namespace == Namespace::Value
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected assigning target to resolve, refs={:?}",
        unit.references
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            )
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn suppresses_unknown_symbol_for_bare_loop_where_field_name() {
    let src = "\
TYPES: BEGIN OF ty_dest,
  type TYPE string,
  content TYPE string,
END OF ty_dest.
TYPES ty_dest_tab TYPE STANDARD TABLE OF ty_dest WITH EMPTY KEY.
DATA lt_dest TYPE ty_dest_tab.
FIELD-SYMBOLS <fs_destination> TYPE ty_dest.

LOOP AT lt_dest[] ASSIGNING <fs_destination>
WHERE type IS NOT INITIAL.
ENDLOOP.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///loop_where_bare_field.abap", src, &parsed);

    assert!(
        unit.references.iter().any(|reference| {
            reference.name.as_ref() == "type"
                && reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
        }),
        "expected bare field reference in LOOP WHERE, refs={:?}",
        unit.references
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("'type'")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn suppresses_unknown_symbol_for_bare_loop_where_field_name_with_cs_on_nested_selector_source() {
    let src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_dest,
             type TYPE string,
             content TYPE string,
           END OF ty_dest.
    TYPES ty_dest_tab TYPE STANDARD TABLE OF ty_dest WITH EMPTY KEY.
    TYPES: BEGIN OF ty_destination_list,
             destination TYPE ty_dest_tab,
           END OF ty_destination_list.
    TYPES: BEGIN OF ty_extension,
             destination_list TYPE ty_destination_list,
           END OF ty_extension.
    TYPES: BEGIN OF ty_object_event,
             extension TYPE ty_extension,
           END OF ty_object_event.
    TYPES: BEGIN OF ty_choice,
             object_event TYPE ty_object_event,
           END OF ty_choice.
    METHODS run CHANGING cs_choice TYPE ty_choice.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    FIELD-SYMBOLS <fs_destination> TYPE ty_dest.
    LOOP AT cs_choice-object_event-extension-destination_list-destination[] ASSIGNING <fs_destination>
    WHERE type CS 'location'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///loop_where_nested_selector_cs.abap", src, &parsed);

    assert!(
        unit.references.iter().any(|reference| {
            reference.name.as_ref() == "type"
                && reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
        }),
        "expected bare field reference in LOOP WHERE, refs={:?}",
        unit.references
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("'type'")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn get_reference_stmt_resolves_source_and_target_operands() {
    let src = r#"
TYPES: BEGIN OF ty_xmlparse,
         xi_data TYPE REF TO data,
       END OF ty_xmlparse.

DATA es_request_aif_struct TYPE string.
DATA ls_xmlparse TYPE ty_xmlparse.

GET REFERENCE OF es_request_aif_struct INTO ls_xmlparse-xi_data.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///get_reference_struct.abap", src, &parsed);

    let source_symbol = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Variable
                && symbol.name.as_ref() == "es_request_aif_struct"
        })
        .expect("source variable");

    let source_ref = unit
        .references
        .iter()
        .find(|reference| {
            reference.kind == ReferenceKind::Identifier
                && reference.namespace == Namespace::Value
                && reference.name.as_ref() == "es_request_aif_struct"
        })
        .expect("get reference source");
    assert_eq!(
        source_ref.resolution,
        Some(Resolution::Symbol(SymbolHandle {
            unit: unit.unit_id,
            symbol: source_symbol.id,
        }))
    );

    assert!(unit.references.iter().any(|reference| {
        reference.kind == ReferenceKind::Identifier
            && reference.namespace == Namespace::Value
            && reference.name.as_ref() == "ls_xmlparse"
    }));

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("es_request_aif_struct")
                    || diag.message.contains("ls_xmlparse"))
        }),
        "unexpected unresolved diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn get_reference_stmt_resolves_plain_target_reference() {
    let src = r#"
DATA ls_xmlparse TYPE string.
DATA lo_xmlparse TYPE REF TO data.

GET REFERENCE OF ls_xmlparse INTO lo_xmlparse.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///get_reference_plain.abap", src, &parsed);

    for name in ["ls_xmlparse", "lo_xmlparse"] {
        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == abap_symbols::SymbolKind::Variable && symbol.name.as_ref() == name
            })
            .expect("variable symbol");

        let reference = unit
            .references
            .iter()
            .find(|reference| {
                reference.kind == ReferenceKind::Identifier
                    && reference.namespace == Namespace::Value
                    && reference.name.as_ref() == name
            })
            .expect("reference");

        assert_eq!(
            reference.resolution,
            Some(Resolution::Symbol(SymbolHandle {
                unit: unit.unit_id,
                symbol: symbol.id,
            }))
        );
    }

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::UnresolvedReference),
        "unexpected unresolved diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn suppresses_unknown_symbol_for_bare_delete_where_field_name_on_external_table_type() {
    let main_src = r#"
DATA lt_trans_del TYPE /sttp/tt_evt_sdr.

DELETE lt_trans_del WHERE evtid IS NOT INITIAL.
"#;
    let row_src = r#"
TYPES: BEGIN OF /sttp/dm_evt_sdr,
         evtid TYPE i,
       END OF /sttp/dm_evt_sdr.
"#;
    let table_src = r#"
TYPES /sttp/tt_evt_sdr TYPE STANDARD TABLE OF /sttp/dm_evt_sdr WITH EMPTY KEY.
"#;
    let main_parse = parse(main_src);
    let row_parse = parse(row_src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///main.abap",
            source: main_src,
            parse: &main_parse,
        },
        ProjectInput {
            uri: "file:///ddic_row.abap",
            source: row_src,
            parse: &row_parse,
        },
        ProjectInput {
            uri: "file:///ddic_table.abap",
            source: table_src,
            parse: &table_parse,
        },
    ]);
    let unit = project.unit_by_uri("file:///main.abap").expect("main unit");

    assert!(
        unit.references.iter().any(|reference| {
            reference.name.as_ref() == "evtid"
                && reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
        }),
        "expected bare field reference in DELETE WHERE, refs={:?}",
        unit.references
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("'evtid'")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn suppresses_unknown_symbol_for_bare_delete_where_field_name_on_inline_copy_of_external_table_type()
 {
    let main_src = r#"
DATA mt_obj_itm TYPE /sttp/t_dm_obj_itm.

DATA(lt_obj_itm) = mt_obj_itm.
DELETE lt_obj_itm WHERE uom NE 'PK'.
"#;
    let row_src = r#"
TYPES: BEGIN OF /sttp/dm_obj_itm,
         uom TYPE string,
         objid TYPE string,
       END OF /sttp/dm_obj_itm.
"#;
    let table_src = r#"
TYPES /sttp/t_dm_obj_itm TYPE STANDARD TABLE OF /sttp/dm_obj_itm WITH EMPTY KEY.
"#;
    let main_parse = parse(main_src);
    let row_parse = parse(row_src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///main.abap",
            source: main_src,
            parse: &main_parse,
        },
        ProjectInput {
            uri: "file:///ddic_row.abap",
            source: row_src,
            parse: &row_parse,
        },
        ProjectInput {
            uri: "file:///ddic_table.abap",
            source: table_src,
            parse: &table_parse,
        },
    ]);
    let unit = project.unit_by_uri("file:///main.abap").expect("main unit");

    assert!(
        unit.references.iter().any(|reference| {
            reference.name.as_ref() == "uom"
                && reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
        }),
        "expected bare field reference in DELETE WHERE, refs={:?}",
        unit.references
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("'uom'")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn suppresses_unknown_symbol_for_bare_delete_where_field_name_on_inline_copy_of_external_table_attribute_in_method()
 {
    let main_src = r#"
CLASS zcl_rule DEFINITION.
  PRIVATE SECTION.
    DATA mt_obj_itm TYPE /sttp/t_dm_obj_itm.
    METHODS run.
ENDCLASS.

CLASS zcl_rule IMPLEMENTATION.
  METHOD run.
    DATA(lt_obj_itm) = mt_obj_itm.
    DELETE lt_obj_itm WHERE uom NE 'PK'.
  ENDMETHOD.
ENDCLASS.
"#;
    let row_src = r#"
TYPES: BEGIN OF /sttp/dm_obj_itm,
         uom TYPE string,
         objid TYPE string,
       END OF /sttp/dm_obj_itm.
"#;
    let table_src = r#"
TYPES /sttp/t_dm_obj_itm TYPE STANDARD TABLE OF /sttp/dm_obj_itm WITH EMPTY KEY.
"#;
    let main_parse = parse(main_src);
    let row_parse = parse(row_src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///main.abap",
            source: main_src,
            parse: &main_parse,
        },
        ProjectInput {
            uri: "file:///ddic_row.abap",
            source: row_src,
            parse: &row_parse,
        },
        ProjectInput {
            uri: "file:///ddic_table.abap",
            source: table_src,
            parse: &table_parse,
        },
    ]);
    let unit = project.unit_by_uri("file:///main.abap").expect("main unit");

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("'uom'")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn suppresses_unknown_symbol_for_bare_delete_where_field_name_from_ddic_proxy_include_structure() {
    let main_src = r#"
DATA mt_obj_itm TYPE /sttp/t_dm_obj_itm.

DATA(lt_obj_itm) = mt_obj_itm.
DELETE lt_obj_itm WHERE uom NE 'PK'.
"#;
    let include_src = r#"
TYPES: BEGIN OF /sttp/s_dm_obj_itm,
         uom TYPE string,
       END OF /sttp/s_dm_obj_itm.
"#;
    let row_src = r#"
TYPES: BEGIN OF /sttp/dm_obj_itm,
         dm_obj_itm TYPE /sttp/s_dm_obj_itm,
       END OF /sttp/dm_obj_itm.
"#;
    let table_src = r#"
TYPES /sttp/t_dm_obj_itm TYPE STANDARD TABLE OF /sttp/dm_obj_itm WITH EMPTY KEY.
"#;
    let main_parse = parse(main_src);
    let include_parse = parse(include_src);
    let row_parse = parse(row_src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///main.abap",
            source: main_src,
            parse: &main_parse,
        },
        ProjectInput {
            uri: "file:///ddic_include.abap",
            source: include_src,
            parse: &include_parse,
        },
        ProjectInput {
            uri: "file:///ddic_row.abap",
            source: row_src,
            parse: &row_parse,
        },
        ProjectInput {
            uri: "file:///ddic_table.abap",
            source: table_src,
            parse: &table_parse,
        },
    ]);
    let unit = project.unit_by_uri("file:///main.abap").expect("main unit");

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("'uom'")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn suppresses_unknown_symbol_for_bare_delete_where_field_name_when_ddic_proxy_include_metadata_is_incomplete()
 {
    let main_src = r#"
DATA mt_obj_itm TYPE /sttp/t_dm_obj_itm.

DATA(lt_obj_itm) = mt_obj_itm.
DELETE lt_obj_itm WHERE uom NE 'PK'.
"#;
    let include_src = r#"
TYPES: BEGIN OF /sttp/s_dm_obj_itm,
         serno TYPE string,
       END OF /sttp/s_dm_obj_itm.
"#;
    let row_src = r#"
TYPES: BEGIN OF /sttp/dm_obj_itm,
         dm_obj_itm TYPE /sttp/s_dm_obj_itm,
       END OF /sttp/dm_obj_itm.
"#;
    let table_src = r#"
TYPES /sttp/t_dm_obj_itm TYPE STANDARD TABLE OF /sttp/dm_obj_itm WITH EMPTY KEY.
"#;
    let main_parse = parse(main_src);
    let include_parse = parse(include_src);
    let row_parse = parse(row_src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///main.abap",
            source: main_src,
            parse: &main_parse,
        },
        ProjectInput {
            uri: "file:///ddic_include.abap",
            source: include_src,
            parse: &include_parse,
        },
        ProjectInput {
            uri: "file:///ddic_row.abap",
            source: row_src,
            parse: &row_parse,
        },
        ProjectInput {
            uri: "file:///ddic_table.abap",
            source: table_src,
            parse: &table_parse,
        },
    ]);
    let unit = project.unit_by_uri("file:///main.abap").expect("main unit");

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("'uom'")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn validates_unknown_template_interpolation_members() {
    let src = r#"
CLASS zcl_expr DEFINITION.
  PUBLIC SECTION.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
  METHOD to_string.
    rv_text = 'expr'.
  ENDMETHOD.
ENDCLASS.

DATA mo_left TYPE REF TO zcl_expr.
DATA rv_text TYPE string.

rv_text = |{ mo_left->missing( ) }|.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///template_unknown_member.abap", src, &parsed);

    assert!(unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnknownField
            && diag.message.contains("missing")
            && diag.message.contains("zcl_expr")
    }));
}

#[test]
fn reports_builtin_routine_named_argument_passing_as_invalid() {
    let src = "DATA text TYPE string. DATA len TYPE i. len = strlen( val = text ).";
    let parsed = parse(src);
    let unit = analyze_unit("file:///routine_named_args.abap", src, &parsed);

    assert!(unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::InvalidBuiltinNamedArgument
            && diag.message.contains("strlen")
            && diag.message.contains("named parameter passing")
    }));
}

#[test]
fn resolves_to_lower_as_builtin_routine() {
    let src = "DATA text TYPE string. DATA lower TYPE string. lower = to_lower( text ).";
    let parsed = parse(src);
    let unit = analyze_unit("file:///builtin_to_lower.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.kind == ReferenceKind::RoutineCall
            && reference.namespace == Namespace::Routine
            && reference.name.as_ref() == "to_lower"
            && matches!(reference.resolution, Some(Resolution::BuiltinRoutine))
    }));
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("to_lower")
    }));
}

#[test]
fn resolves_substring_builtin_with_named_arguments() {
    let src = "\
DATA iv_string TYPE string.\n\
DATA gc_0 TYPE i VALUE 0.\n\
DATA gc_50 TYPE i VALUE 50.\n\
DATA gc_150 TYPE i VALUE 150.\n\
DATA ev1 TYPE string.\n\
DATA ev2 TYPE string.\n\
DATA ev3 TYPE string.\n\
ev1 = substring( val = iv_string off = gc_0 len = gc_50 ).\n\
ev2 = substring( val = iv_string off = gc_150 ).\n\
ev3 = substring( val = iv_string len = 10 ).\n\
";
    let parsed = parse(src);
    let unit = analyze_unit("file:///builtin_substring.abap", src, &parsed);

    let substring_calls: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.kind == ReferenceKind::RoutineCall
                && reference.namespace == Namespace::Routine
                && reference.name.as_ref() == "substring"
        })
        .collect();
    assert_eq!(substring_calls.len(), 3, "{:?}", substring_calls);
    assert!(
        substring_calls
            .iter()
            .all(|reference| { matches!(reference.resolution, Some(Resolution::BuiltinRoutine)) })
    );
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::InvalidBuiltinNamedArgument
            && diag.message.contains("substring")
    }));
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("substring")
    }));
}

#[test]
fn resolves_condense_builtin_with_named_arguments() {
    let src = "\
DATA ev_characters TYPE string.\n\
DATA sv_null_char TYPE string.\n\
ev_characters = condense( val = ev_characters del = sv_null_char ).\n\
";
    let parsed = parse(src);
    let unit = analyze_unit("file:///builtin_condense.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.kind == ReferenceKind::RoutineCall
            && reference.namespace == Namespace::Routine
            && reference.name.as_ref() == "condense"
            && matches!(reference.resolution, Some(Resolution::BuiltinRoutine))
    }));
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::InvalidBuiltinNamedArgument
            && diag.message.contains("condense")
    }));
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("condense")
    }));
}

#[test]
fn resolves_round_builtin_with_named_arguments() {
    let src = "\
DATA lv_value TYPE decfloat34 VALUE '1.25'.\n\
DATA lv_dec TYPE i VALUE 1.\n\
DATA lv_mode TYPE i VALUE 0.\n\
DATA lv_out TYPE decfloat34.\n\
lv_out = round( val = lv_value dec = lv_dec mode = lv_mode ).\n\
";
    let parsed = parse(src);
    let unit = analyze_unit("file:///builtin_round.abap", src, &parsed);

    assert!(
        unit.references.iter().any(|reference| {
            reference.kind == ReferenceKind::RoutineCall
                && reference.namespace == Namespace::Routine
                && reference.name.as_ref() == "round"
                && matches!(reference.resolution, Some(Resolution::BuiltinRoutine))
        }),
        "{:#?}",
        unit.references
    );
    for name in ["lv_value", "lv_dec", "lv_mode"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "missing reference for `{name}`: refs={:?} diagnostics={:?}",
            unit.references,
            unit.diagnostics
        );
    }
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::InvalidBuiltinNamedArgument && diag.message.contains("round")
    }));
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("round")
    }));
}

#[test]
fn resolves_line_exists_with_table_expression_as_builtin_routine() {
    let src = "\
DATA lt_rep_evt TYPE STANDARD TABLE OF string WITH EMPTY KEY.\n\
IF line_exists( lt_rep_evt[ table_line = 'X' ] ).\n\
ENDIF.\n\
";
    let parsed = parse(src);
    let unit = analyze_unit("file:///builtin_line_exists.abap", src, &parsed);

    assert!(
        unit.references.iter().any(|reference| {
            reference.kind == ReferenceKind::RoutineCall
                && reference.namespace == Namespace::Routine
                && reference.name.as_ref() == "line_exists"
                && matches!(reference.resolution, Some(Resolution::BuiltinRoutine))
        }),
        "{:#?}",
        unit.references
    );
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("line_exists")
    }));
}

#[test]
fn resolves_line_exists_in_not_and_condition_as_builtin_routine() {
    let src = "\
DATA lt_rep_evt TYPE STANDARD TABLE OF string WITH EMPTY KEY.\n\
DATA lt_obj_comm TYPE STANDARD TABLE OF string WITH EMPTY KEY.\n\
CONSTANTS lc_rs_comm TYPE string VALUE 'COMM'.\n\
IF NOT line_exists( lt_rep_evt[ rule_type = lc_rs_comm ] ) AND lt_obj_comm IS NOT INITIAL.\n\
ENDIF.\n\
";
    let parsed = parse(src);
    let unit = analyze_unit("file:///builtin_line_exists_not_and.abap", src, &parsed);

    let line_exists_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.kind == ReferenceKind::RoutineCall
                && reference.namespace == Namespace::Routine
                && reference.name.as_ref() == "line_exists"
        })
        .collect();
    assert_eq!(line_exists_refs.len(), 1, "{:#?}", unit.references);
    assert!(
        line_exists_refs
            .iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::BuiltinRoutine)))
    );
}

#[test]
fn resolves_line_exists_in_or_condition_as_builtin_routine() {
    let src = "\
TYPES: BEGIN OF ty_child,\n\
         trkid TYPE string,\n\
         serial TYPE string,\n\
       END OF ty_child.\n\
DATA lt_resp TYPE STANDARD TABLE OF ty_child WITH EMPTY KEY.\n\
DATA ls_child TYPE ty_child.\n\
IF line_exists( lt_resp[ trkid = ls_child-trkid ] ) OR\n\
   line_exists( lt_resp[ serial = ls_child-serial ] ).\n\
ENDIF.\n\
";
    let parsed = parse(src);
    let unit = analyze_unit("file:///builtin_line_exists_or.abap", src, &parsed);

    let line_exists_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.kind == ReferenceKind::RoutineCall
                && reference.namespace == Namespace::Routine
                && reference.name.as_ref() == "line_exists"
        })
        .collect();
    assert_eq!(line_exists_refs.len(), 2, "{:#?}", unit.references);
    assert!(
        line_exists_refs
            .iter()
            .all(|reference| matches!(reference.resolution, Some(Resolution::BuiltinRoutine)))
    );
}

#[test]
fn data_and_class_data_value_decls_do_not_raise_unresolved_symbols() {
    let src = r##"
CLASS /cdbasis/cl_messages DEFINITION.
  PROTECTED SECTION.
    DATA mv_loglevel TYPE i VALUE 0. "#EC NOTEXT           " .
    CLASS-DATA sv_loglevel TYPE i VALUE 0. "#EC NOTEXT           " .
ENDCLASS.
"##;
    let parsed = parse(src);
    let unit = analyze_unit("file:///class_data_value_decl.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("mv_loglevel") || diag.message.contains("data"))
        }),
        "unexpected unresolved diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_interface_calls_and_aliases_transitively() {
    let src = r#"
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.

INTERFACE i2.
  INTERFACES i1.
  ALIASES m1 FOR i1~meth.
  METHODS meth.
ENDINTERFACE.

INTERFACE i3.
  INTERFACES i2.
  ALIASES: m1 FOR i2~m1,
           m2 FOR i2~meth.
  METHODS meth.
ENDINTERFACE.

CLASS c1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES i3.
    ALIASES: m1 FOR i3~m1,
             m2 FOR i3~m2,
             m3 FOR i3~meth.
ENDCLASS.

CLASS c1 IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
  METHOD i2~meth.
  ENDMETHOD.
  METHOD i3~meth.
  ENDMETHOD.
ENDCLASS.

DATA lo_obj TYPE REF TO c1.

START-OF-SELECTION.
  CREATE OBJECT lo_obj.
  lo_obj->i1~meth( ).
  lo_obj->i2~meth( ).
  lo_obj->i3~meth( ).
  lo_obj->m1( ).
  lo_obj->m2( ).
  lo_obj->m3( ).
"#;
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///interfaces_aliases.abap", src, &parsed);

    assert!(
        unit.implemented_interfaces.iter().any(|item| {
            item.owner_symbol
                == unit
                    .symbols
                    .iter()
                    .find(|symbol| {
                        symbol.kind == abap_symbols::SymbolKind::Class
                            && symbol.name.as_ref() == "c1"
                    })
                    .expect("class c1")
                    .id
                && item.interface_name.as_ref() == "i3"
        }),
        "{:#?}",
        unit.implemented_interfaces
    );

    for alias in ["m1", "m2", "m3"] {
        assert!(
            unit.class_members.iter().any(|member| {
                member.class_symbol
                    == unit
                        .symbols
                        .iter()
                        .find(|symbol| {
                            symbol.kind == abap_symbols::SymbolKind::Class
                                && symbol.name.as_ref() == "c1"
                        })
                        .expect("class c1")
                        .id
                    && member.name.as_ref() == alias
            }),
            "missing alias member {alias}: {:#?}",
            unit.class_members
        );
    }

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("i1")
                || diag.message.contains("i2")
                || diag.message.contains("i3")
                || diag.message.contains("m1")
                || diag.message.contains("m2")
                || diag.message.contains("m3"))
        }),
        "{:#?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_interface_qualified_calls_via_inherited_interface_implementation() {
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

START-OF-SELECTION.
  CREATE OBJECT lo_obj.
  lo_obj->i1~meth( ).
"#;
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///inherited_interface_impl.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("i1") || diag.message.contains("meth"))
        }),
        "{:#?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_me_and_interface_parameters_in_qualified_method_implementation() {
    let src = r#"
INTERFACE i1.
  METHODS meth
    IMPORTING iv_value TYPE i
    RETURNING VALUE(rv_value) TYPE i.
ENDINTERFACE.

CLASS c1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES i1.
  PRIVATE SECTION.
    DATA mv_value TYPE i.
ENDCLASS.

CLASS c1 IMPLEMENTATION.
  METHOD i1~meth.
    me->mv_value = iv_value.
    rv_value = me->mv_value.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///qualified_method_impl_scope.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "me"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "iv_value"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "rv_value"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Type
            && reference.name.as_ref() == "i1"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.field_accesses.iter().any(|access| {
        access.base_namespace == Namespace::Type
            && access.base_name.as_ref() == "i1"
            && access.field_path.len() == 1
            && access.field_path[0].name.as_ref() == "meth"
    }));

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("me")
                || diag.message.contains("iv_value")
                || diag.message.contains("rv_value")
                || diag.message.contains("mv_value")
                || diag.message.contains("i1")
                || diag.message.contains("meth"))
        }),
        "{:#?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_me_in_method_implemented_from_second_interface_statement() {
    let src = r#"
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.

INTERFACE i2.
  METHODS met2.
ENDINTERFACE.

CLASS c1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES i1.
    INTERFACES i2.
  PRIVATE SECTION.
    METHODS meth1.
ENDCLASS.

CLASS c1 IMPLEMENTATION.
  METHOD i2~met2.
    me->meth1( ).
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///second_interface_impl_scope.abap", src, &parsed);

    assert!(
        unit.implemented_interfaces
            .iter()
            .any(|item| item.interface_name.as_ref() == "i1"),
        "{:#?}",
        unit.implemented_interfaces
    );
    assert!(
        unit.implemented_interfaces
            .iter()
            .any(|item| item.interface_name.as_ref() == "i2"),
        "{:#?}",
        unit.implemented_interfaces
    );
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "me"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("me")
                || diag.message.contains("meth1")
                || diag.message.contains("i2")
                || diag.message.contains("met2"))
        }),
        "{:#?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_namespaced_interface_method_implementation_scope() {
    let src = r#"
INTERFACE /sttp/if_badi_rule_processing.
  METHODS execute
    IMPORTING
      !iv_evtid TYPE string
      !is_rule_keys TYPE string OPTIONAL
    CHANGING
      !co_messages TYPE string OPTIONAL.
ENDINTERFACE.

CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_badi_interface.
    INTERFACES /sttp/if_badi_rule_processing.
    METHODS prepare_data
      IMPORTING
        VALUE(is_rule_keys) TYPE string.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD /sttp/if_badi_rule_processing~execute.
    CALL METHOD me->prepare_data
      EXPORTING
        is_rule_keys = is_rule_keys.
    co_messages = iv_evtid.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///namespaced_interface_impl.abap", src, &parsed);

    for name in ["me", "iv_evtid", "is_rule_keys", "co_messages"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.namespace == Namespace::Value
                    && reference.name.as_ref() == name
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "missing resolved reference for {name}: {:#?}",
            unit.references
        );
    }
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Type
            && reference.name.as_ref() == "/sttp/if_badi_rule_processing"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.field_accesses.iter().any(|access| {
        access.base_namespace == Namespace::Type
            && access.base_name.as_ref() == "/sttp/if_badi_rule_processing"
            && access.field_path.len() == 1
            && access.field_path[0].name.as_ref() == "execute"
    }));
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("me")
                || diag.message.contains("iv_evtid")
                || diag.message.contains("is_rule_keys")
                || diag.message.contains("co_messages")
                || diag.message.contains("/sttp/if_badi_rule_processing")
                || diag.message.contains("execute"))
        }),
        "{:#?}",
        unit.diagnostics
    );
}

#[test]
fn resolves_qualified_method_scope_when_first_interface_is_unresolved() {
    let src = r#"
INTERFACE /sttp/if_badi_rule_processing.
  METHODS execute
    IMPORTING !iv_evtid TYPE string.
ENDINTERFACE.

CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_badi_interface.
    INTERFACES /sttp/if_badi_rule_processing.
    METHODS helper.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD /sttp/if_badi_rule_processing~execute.
    me->helper( ).
    DATA(lv_evtid) = iv_evtid.
  ENDMETHOD.

  METHOD helper.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///unresolved_first_interface.abap", src, &parsed);

    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "me"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.name.as_ref() == "iv_evtid"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("me")
                || diag.message.contains("helper")
                || diag.message.contains("iv_evtid")
                || diag.message.contains("/sttp/if_badi_rule_processing"))
        }),
        "{:#?}",
        unit.diagnostics
    );
}

#[test]
fn interface_load_statement_uses_type_namespace_for_referenced_interface() {
    let src = r#"
INTERFACE if_inner.
  METHODS run.
ENDINTERFACE.

INTERFACE if_outer.
  INTERFACE if_inner LOAD.
ENDINTERFACE.
"#;
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///interface_load.abap", src, &parsed);

    assert!(
        unit.implemented_interfaces.iter().any(|item| {
            item.interface_name.as_ref() == "if_inner"
                && unit
                    .symbols
                    .iter()
                    .find(|symbol| {
                        symbol.kind == abap_symbols::SymbolKind::Interface
                            && symbol.name.as_ref() == "if_outer"
                    })
                    .is_some_and(|owner| item.owner_symbol == owner.id)
        }),
        "{:#?}",
        unit.implemented_interfaces
    );

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::WrongNamespace | DiagnosticKind::UnresolvedReference
            ) && diag.message.contains("if_inner")
        }),
        "{:#?}",
        unit.diagnostics
    );
}

#[test]
fn substring_access_uses_value_namespace() {
    let src = "\
DATA ls_time TYPE string.\n\
DATA lv_evt TYPE string.\n\
DATA lv_long TYPE string.\n\
lv_evt = ls_time+2(8).\n\
lv_long = ls_time(14).";
    let parsed = parse(src);
    let unit = analyze_unit("file:///substring_expr.abap", src, &parsed);

    let ls_time_refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| reference.name.as_ref() == "ls_time")
        .collect();
    assert_eq!(ls_time_refs.len(), 2, "{:?}", ls_time_refs);
    assert!(ls_time_refs.iter().all(|reference| {
        reference.namespace == Namespace::Value
            && reference.kind == ReferenceKind::Identifier
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::WrongNamespace && diag.message.contains("ls_time")
    }));
}

#[test]
fn substring_access_on_table_expression_selector_resolves_field() {
    let src = "\
TYPES: BEGIN OF ty_encode_decode,\n\
         code_char TYPE string,\n\
       END OF ty_encode_decode.\n\
TYPES ty_encode_decode_tab TYPE STANDARD TABLE OF ty_encode_decode WITH EMPTY KEY.\n\
DATA lt_encode_decode TYPE ty_encode_decode_tab.\n\
DATA rv_gs1 TYPE string.\n\
rv_gs1 = lt_encode_decode[ 1 ]-code_char+2.";
    let parsed = parse(src);
    let unit = analyze_unit("file:///substring_table_expr_selector.abap", src, &parsed);

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_name.as_ref() == "lt_encode_decode"
                && access
                    .field_path
                    .iter()
                    .map(|segment| segment.name.as_ref())
                    .collect::<Vec<_>>()
                    == vec!["code_char"]
        }),
        "expected table-expression selector field access, accesses={:?}",
        unit.field_accesses
    );
    assert!(
        unit.references.iter().any(|reference| {
            reference.name.as_ref() == "lt_encode_decode"
                && reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }),
        "expected resolved lt_encode_decode reference, refs={:?}",
        unit.references
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && diag.message.contains("code_char")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn value_optional_with_substring_on_table_expression_selector_resolves_field() {
    let src = "\
TYPES: BEGIN OF ty_encode_decode,\n\
         code_char TYPE string,\n\
       END OF ty_encode_decode.\n\
TYPES ty_encode_decode_tab TYPE STANDARD TABLE OF ty_encode_decode WITH EMPTY KEY.\n\
DATA lt_encode_decode TYPE ty_encode_decode_tab.\n\
DATA rv_gs1 TYPE string.\n\
rv_gs1 = VALUE #( lt_encode_decode[ 1 ]-code_char+2 OPTIONAL ).";
    let parsed = parse(src);
    let unit = analyze_unit(
        "file:///value_optional_substring_table_expr.abap",
        src,
        &parsed,
    );

    assert!(
        unit.field_accesses.iter().any(|access| {
            access.base_name.as_ref() == "lt_encode_decode"
                && access
                    .field_path
                    .iter()
                    .map(|segment| segment.name.as_ref())
                    .collect::<Vec<_>>()
                    == vec!["code_char"]
        }),
        "expected field access, accesses={:?}",
        unit.field_accesses
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && diag.message.contains("code_char")
        }),
        "unexpected diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn reports_missing_super_constructor_call_in_subclass_constructor() {
    let src = r#"
CLASS zcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_name TYPE string.
ENDCLASS.

CLASS zcl_parent IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_name TYPE string.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///missing_super_ctor.abap", src, &parsed);

    assert!(unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::MissingSuperConstructorCall
            && diag.message.contains("must call super->constructor( )")
    }));
}

#[test]
fn reports_missing_parent_constructor_arguments_in_super_call() {
    let src = r#"
CLASS zcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_name TYPE string
        iv_kind TYPE string.
ENDCLASS.

CLASS zcl_parent IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_name TYPE string
        iv_kind TYPE string.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_name = iv_name ).
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///missing_super_ctor_args.abap", src, &parsed);

    assert!(unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::MissingSuperConstructorCall && diag.message.contains("iv_kind")
    }));
}

#[test]
fn accepts_valid_super_constructor_call_without_unresolved_super() {
    let src = r#"
CLASS zcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_name TYPE string
        iv_kind TYPE string.
ENDCLASS.

CLASS zcl_parent IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_name TYPE string
        iv_kind TYPE string.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      iv_name = iv_name
      iv_kind = iv_kind
    ).
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///valid_super_ctor.abap", src, &parsed);

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::MissingSuperConstructorCall),
        "unexpected constructor diagnostics: {:?}",
        unit.diagnostics
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("super")
        }),
        "unexpected unresolved super diagnostic: {:?}",
        unit.diagnostics
    );
}

#[test]
fn accepts_legacy_super_constructor_call_without_missing_call_diagnostic() {
    let src = r#"
CLASS zcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS zcl_parent IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD SUPER->CONSTRUCTOR.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///legacy_super_ctor.abap", src, &parsed);

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::MissingSuperConstructorCall),
        "unexpected constructor diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn accepts_uppercase_legacy_super_constructor_call_with_space_before_period() {
    let src = r#"
CLASS zcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS zcl_parent IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    CALL METHOD SUPER->CONSTRUCTOR .
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///legacy_super_ctor_spaced.abap", src, &parsed);

    assert!(
        !unit
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::MissingSuperConstructorCall),
        "unexpected constructor diagnostics: {:?}",
        unit.diagnostics
    );
}

#[test]
fn message_stmt_resolves_with_into_and_dynamic_text() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS m.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD m.
    DATA:
      lv_lines TYPE i,
      gv_dummy_msg TYPE string,
      iv_logsys TYPE string,
      lv_result TYPE string.
    MESSAGE i043(/sttp/int_msg) WITH lv_lines iv_logsys INTO gv_dummy_msg.
    MESSAGE lv_result TYPE 'E'.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///message_stmt.abap", src, &parsed);

    for name in ["lv_lines", "iv_logsys", "gv_dummy_msg", "lv_result"] {
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected unresolved diagnostic for {name}: {:?}",
            unit.diagnostics
        );
    }
}

#[test]
fn message_stmt_declares_into_data_inline() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS m.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD m.
    MESSAGE w899(/sttp/msg) WITH sy-msgv1 INTO DATA(lv_message).
    lv_message = |x|.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///message_into_data.abap", src, &parsed);

    assert!(
        unit.symbols.iter().any(|s| {
            s.kind == abap_symbols::SymbolKind::Variable && s.name.as_ref() == "lv_message"
        }),
        "expected inline lv_message symbol, got {:?}",
        unit.symbols
    );
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("lv_message")
        }),
        "unexpected unresolved lv_message: {:?}",
        unit.diagnostics
    );
}

#[test]
fn message_stmt_resolves_with_literal_and_following_identifier() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS m IMPORTING iv_logsys TYPE string.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD m.
    DATA:
      lv_lines TYPE i,
      gv_dummy_msg TYPE string.
    MESSAGE i043(/sttp/int_msg) WITH lv_lines 'ORDER_HEADER' iv_logsys INTO gv_dummy_msg.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///message_stmt_literal_arg.abap", src, &parsed);

    for name in ["lv_lines", "iv_logsys", "gv_dummy_msg"] {
        assert!(
            !unit.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected unresolved diagnostic for {name}: {:?}",
            unit.diagnostics
        );
    }
    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("unknown symbol ' '")
        }),
        "unexpected unresolved diagnostic for blank template literal: {:?}",
        unit.diagnostics
    );
}

#[test]
fn message_stmt_ignores_text_pool_ids_and_display_like_appendix() {
    let src = r#"
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS m.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD m.
    DATA lv_name TYPE string.
    MESSAGE s398(00) WITH TEXT-007 lv_name DISPLAY LIKE 'E'.
  ENDMETHOD.
ENDCLASS.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///message_stmt_text_pool.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && (diag.message.contains("unknown symbol 'text'")
                    || diag.message.contains("unknown symbol '007'")
                    || diag.message.contains("unknown symbol 'display'")
                    || diag.message.contains("unknown symbol 'like'"))
        }),
        "unexpected MESSAGE compact/text-pool diagnostics: {:?}",
        unit.diagnostics
    );
    assert!(
        unit.references.iter().any(|reference| {
            reference.kind == ReferenceKind::MessageClass && reference.name.as_ref() == "00"
        }),
        "expected compact MESSAGE class reference: {:?}",
        unit.references
    );
    assert!(
        unit.references.iter().any(|reference| {
            reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref() == "lv_name"
                && reference.resolution.is_some()
        }),
        "expected MESSAGE operand lv_name to resolve: {:?}",
        unit.references
    );
}

#[test]
fn ignores_plain_template_literal_text_in_token_collected_statements() {
    let src = r#"
FORM run USING iv_tag_path TYPE string.
  DATA lv_first_sep TYPE int4.

  FIND FIRST OCCURRENCE OF |abc| IN iv_tag_path MATCH OFFSET lv_first_sep.
ENDFORM.
"#;
    let parsed = parse(src);
    let unit = analyze_unit("file:///find_stmt_literal.abap", src, &parsed);

    assert!(
        !unit.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("unknown symbol 'abc'")
        }),
        "unexpected unresolved diagnostic for template literal text: {:?}",
        unit.diagnostics
    );
}
