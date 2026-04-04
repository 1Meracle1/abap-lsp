use abap_parser::parse;

use abap_symbols::{
    DiagnosticKind, Namespace, ReferenceKind, Resolution, SqlNameRefKind, SqlPredicateKind,
    SqlProjectionKind, SqlSourceKind, SqlTargetKind, StructureFieldShape, SymbolHandle,
    analyze_unit,
};

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
