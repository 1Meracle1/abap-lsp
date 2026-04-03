use abap_parser::parse;

use abap_symbols::{
    DiagnosticKind, Namespace, ReferenceKind, Resolution, StructureFieldShape, SymbolHandle,
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
            err.message.contains("method modifier ABSTRACT must appear before parameter declarations")
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
