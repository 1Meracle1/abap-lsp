use abap_parser::parse;

use abap_symbols::{DiagnosticKind, Namespace, Resolution, StructureFieldShape, analyze_unit};

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
