use abap_parser::parse;

use abap_symbols::{
    ClassMemberKind, DiagnosticKind, Namespace, ProjectInput, ReferenceKind, Resolution,
    SqlNameRefKind, SymbolKind, analyze_project,
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
fn included_units_share_the_including_compilation_context() {
    let root_src = "REPORT zmain. INCLUDE: ztop, zf01.";
    let top_src = "DATA gv_shared TYPE i.";
    let form_src = "FORM run. gv_shared = 1. ENDFORM.";
    let root_parse = parse(root_src);
    let top_parse = parse(top_src);
    let form_parse = parse(form_src);

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
            source: form_src,
            parse: &form_parse,
        },
    ]);

    let form = project.unit_by_uri("zf01.abap").expect("form include");
    assert!(form.references.iter().any(|reference| {
        reference.name.as_ref() == "gv_shared"
            && matches!(reference.resolution, Some(Resolution::Symbol(handle)) if handle.unit == project.unit_by_uri("ztop.abap").expect("top include").unit_id)
    }));
}

#[test]
fn reports_type_reference_to_type_declared_in_later_include() {
    let root_src = "REPORT zmain. INCLUDE: zdata, ztypes.";
    let data_src = "DATA: ls_object_src TYPE ts_obj_ids.";
    let types_src = "\
TYPES:\n\
  BEGIN OF ts_obj_ids,\n\
    owner TYPE char12,\n\
  END OF ts_obj_ids.";
    let root_parse = parse(root_src);
    let data_parse = parse(data_src);
    let types_parse = parse(types_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "zdata.abap",
            source: data_src,
            parse: &data_parse,
        },
        ProjectInput {
            uri: "ztypes.abap",
            source: types_src,
            parse: &types_parse,
        },
    ]);

    let data = project.unit_by_uri("zdata.abap").expect("data include");
    assert!(
        data.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("declared after its use")
        }),
        "expected later include type diagnostic, diagnostics={:?}",
        data.diagnostics
    );
}

#[test]
fn accepts_type_reference_to_type_declared_in_prior_include() {
    let root_src = "REPORT zmain. INCLUDE ztypes. DATA: ls_object_src TYPE ts_obj_ids.";
    let types_src = "\
TYPES:\n\
  BEGIN OF ts_obj_ids,\n\
    owner TYPE char12,\n\
  END OF ts_obj_ids.";
    let root_parse = parse(root_src);
    let types_parse = parse(types_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "ztypes.abap",
            source: types_src,
            parse: &types_parse,
        },
    ]);

    let root = project.unit_by_uri("zmain.abap").expect("root unit");
    assert!(
        !root.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference
                && diag.message.contains("declared after its use")
        }),
        "unexpected declaration-order diagnostic: {:?}",
        root.diagnostics
    );
}

#[test]
fn does_not_resolve_sibling_unit_symbols_without_include_edge() {
    let root_src = "REPORT zmain. gr_demo = 1.";
    let sibling_src = "DATA gr_demo TYPE i.";
    let root_parse = parse(root_src);
    let sibling_parse = parse(sibling_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "zmain_top.abap",
            source: sibling_src,
            parse: &sibling_parse,
        },
    ]);

    let root = project.unit_by_uri("zmain.abap").expect("root unit");
    assert!(root.include_edges.is_empty());
    assert!(root.references.iter().any(|reference| {
        reference.name.as_ref() == "gr_demo" && reference.resolution.is_none()
    }));
}

#[test]
fn resolves_global_class_when_name_matches_unit_stem() {
    let parent_src = "CLASS zcl_parent DEFINITION. ENDCLASS.";
    let consumer_src = "DATA lo_parent TYPE REF TO zcl_parent.";
    let parent_parse = parse(parent_src);
    let consumer_parse = parse(consumer_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///workspace/zcl_parent.abap",
            source: parent_src,
            parse: &parent_parse,
        },
        ProjectInput {
            uri: "file:///workspace/zconsumer.abap",
            source: consumer_src,
            parse: &consumer_parse,
        },
    ]);

    let parent = project
        .unit_by_uri("file:///workspace/zcl_parent.abap")
        .expect("parent unit");
    let consumer = project
        .unit_by_uri("file:///workspace/zconsumer.abap")
        .expect("consumer unit");
    assert!(consumer.references.iter().any(|reference| {
        reference.name.as_ref() == "zcl_parent"
            && matches!(
                reference.resolution,
                Some(Resolution::Symbol(handle)) if handle.unit == parent.unit_id
            )
    }));
    assert!(
        !consumer.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("zcl_parent")
        }),
        "unexpected unresolved global class diagnostic: {:?}",
        consumer.diagnostics
    );
}

#[test]
fn program_local_class_without_prefix_does_not_resolve_across_unrelated_units() {
    let local_src = "CLASS zcl_helper DEFINITION. ENDCLASS.";
    let consumer_src = "DATA lo_helper TYPE REF TO zcl_helper.";
    let local_parse = parse(local_src);
    let consumer_parse = parse(consumer_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///workspace/zprogram_top.abap",
            source: local_src,
            parse: &local_parse,
        },
        ProjectInput {
            uri: "file:///workspace/zconsumer.abap",
            source: consumer_src,
            parse: &consumer_parse,
        },
    ]);

    let consumer = project
        .unit_by_uri("file:///workspace/zconsumer.abap")
        .expect("consumer unit");
    assert!(consumer.references.iter().any(|reference| {
        reference.name.as_ref() == "zcl_helper" && reference.resolution.is_none()
    }));
    assert!(
        consumer.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains("zcl_helper")
        }),
        "expected unresolved local class diagnostic, got {:?}",
        consumer.diagnostics
    );
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
fn resolves_include_from_sibling_includes_folder_before_global_name_fallback() {
    let root_src = "REPORT zrep.\nINCLUDE zrep_top.\nSTART-OF-SELECTION.\n  lv_includes = 1.";
    let global_src = "DATA lv_global TYPE i.";
    let includes_src = "DATA lv_includes TYPE i.";
    let root_parse = parse(root_src);
    let global_parse = parse(global_src);
    let includes_parse = parse(includes_src);

    let includes_uri = "file:///workspace/src/ZREP/Includes/ZREP_TOP.abap";
    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///workspace/src/ZREP/ZREP.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "file:///workspace/src/includes/ZREP_TOP.abap",
            source: global_src,
            parse: &global_parse,
        },
        ProjectInput {
            uri: includes_uri,
            source: includes_src,
            parse: &includes_parse,
        },
    ]);

    let root = project
        .unit_by_uri("file:///workspace/src/ZREP/ZREP.abap")
        .expect("root unit");
    let target_uri = root
        .include_edges
        .iter()
        .find(|edge| edge.name.as_ref() == "zrep_top")
        .and_then(|edge| edge.target)
        .and_then(|target| project.units.get(target.as_usize()))
        .map(|unit| unit.uri.as_ref())
        .expect("include target");
    assert_eq!(target_uri, includes_uri);
    assert!(root.references.iter().any(|reference| {
        reference.name.as_ref() == "lv_includes"
            && matches!(reference.resolution, Some(Resolution::Symbol(_)))
    }));
}

#[test]
fn resolves_same_folder_include_before_sibling_includes_folder() {
    let root_src = "REPORT zrep.\nINCLUDE zrep_top.\nSTART-OF-SELECTION.\n  lv_same_folder = 1.";
    let includes_src = "DATA lv_includes TYPE i.";
    let same_folder_src = "DATA lv_same_folder TYPE i.";
    let root_parse = parse(root_src);
    let includes_parse = parse(includes_src);
    let same_folder_parse = parse(same_folder_src);

    let same_folder_uri = "file:///workspace/src/ZREP/ZREP_TOP.abap";
    let project = analyze_project(&[
        ProjectInput {
            uri: "file:///workspace/src/ZREP/ZREP.abap",
            source: root_src,
            parse: &root_parse,
        },
        ProjectInput {
            uri: "file:///workspace/src/ZREP/Includes/ZREP_TOP.abap",
            source: includes_src,
            parse: &includes_parse,
        },
        ProjectInput {
            uri: same_folder_uri,
            source: same_folder_src,
            parse: &same_folder_parse,
        },
    ]);

    let root = project
        .unit_by_uri("file:///workspace/src/ZREP/ZREP.abap")
        .expect("root unit");
    let target_uri = root
        .include_edges
        .iter()
        .find(|edge| edge.name.as_ref() == "zrep_top")
        .and_then(|edge| edge.target)
        .and_then(|target| project.units.get(target.as_usize()))
        .map(|unit| unit.uri.as_ref())
        .expect("include target");
    assert_eq!(target_uri, same_folder_uri);
    assert!(root.references.iter().any(|reference| {
        reference.name.as_ref() == "lv_same_folder"
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
fn links_local_class_definition_and_implementation_across_ordered_report_includes() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zcls.
START-OF-SELECTION.
  CREATE OBJECT gr_demo.
  CALL METHOD gr_demo->get_data.
"#;
    let top_src = r#"
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS check_existing.
    METHODS get_data.
ENDCLASS.
"#;
    let cls_src = r#"
DATA gr_demo TYPE REF TO lcl_demo.

CLASS lcl_demo IMPLEMENTATION.
  METHOD check_existing.
  ENDMETHOD.

  METHOD get_data.
  ENDMETHOD.
ENDCLASS.
"#;
    let root_parse = parse(root_src);
    let top_parse = parse(top_src);
    let cls_parse = parse(cls_src);

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
            uri: "zcls.abap",
            source: cls_src,
            parse: &cls_parse,
        },
    ]);

    let top = project.unit_by_uri("ztop.abap").expect("top include");
    let cls = project.unit_by_uri("zcls.abap").expect("class include");
    let top_class = top
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "lcl_demo")
        .expect("top class definition");
    assert!(
        top.class_definition(top_class.id).is_some(),
        "top class should be recorded as a definition"
    );

    let get_data = top
        .class_members_for(top_class.id)
        .find(|member| member.kind == ClassMemberKind::Method && member.name.as_ref() == "get_data")
        .expect("get_data declaration");
    let implementation = get_data
        .implementation
        .as_ref()
        .expect("cross-include implementation link");
    assert_eq!(implementation.unit, cls.unit_id);
    assert!(
        cls_src[implementation.range.clone()]
            .to_ascii_lowercase()
            .contains("get_data")
    );

    let implementation_symbol = cls
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == abap_symbols::SymbolKind::Method
                && symbol.name.as_ref() == get_data.name.as_ref()
        })
        .expect("implementation method symbol");
    let (definition_unit, definition_member) = project
        .class_member_definition_for_method_symbol(cls.unit_id, implementation_symbol.id)
        .expect("implementation should resolve to declaration");
    assert_eq!(definition_unit, top.unit_id);
    assert_eq!(definition_member.name, get_data.name);

    assert!(
        top.diagnostics
            .iter()
            .all(|diag| diag.kind != DiagnosticKind::MissingMethodImplementation),
        "ordered sibling include implementation should satisfy method declarations: {:?}",
        top.diagnostics
    );
    let root = project.unit_by_uri("zmain.abap").expect("root unit");
    assert!(
        root.diagnostics.iter().all(|diag| {
            !(diag.kind == DiagnosticKind::UnknownField && diag.message.contains("get_data"))
        }),
        "root call should resolve class members through ordered includes: {:?}",
        root.diagnostics
    );
}

#[test]
fn resolves_class_attributes_from_definition_include_inside_implementation_include_methods() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zcls.
"#;
    let top_src = r#"
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    DATA: lv_jobname  TYPE string,
          lv_jobcount TYPE string.
    METHODS get_data.
ENDCLASS.
"#;
    let cls_src = r#"
CLASS lcl_demo IMPLEMENTATION.
  METHOD get_data.
    lv_jobname = 'demo'.
    IF lv_jobcount IS INITIAL.
      lv_jobcount = lv_jobname.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
"#;
    let root_parse = parse(root_src);
    let top_parse = parse(top_src);
    let cls_parse = parse(cls_src);

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
            uri: "zcls.abap",
            source: cls_src,
            parse: &cls_parse,
        },
    ]);

    let top = project.unit_by_uri("ztop.abap").expect("top include");
    let cls = project.unit_by_uri("zcls.abap").expect("class include");
    let top_class = top
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "lcl_demo")
        .expect("top class definition");
    let jobname_symbol = top
        .symbols
        .iter()
        .find(|symbol| {
            symbol.name.as_ref() == "lv_jobname"
                && top.scope(symbol.scope).owner == Some(top_class.id)
        })
        .expect("lv_jobname symbol");
    let jobcount_symbol = top
        .symbols
        .iter()
        .find(|symbol| {
            symbol.name.as_ref() == "lv_jobcount"
                && top.scope(symbol.scope).owner == Some(top_class.id)
        })
        .expect("lv_jobcount symbol");

    for (name, expected_symbol) in [
        ("lv_jobname", jobname_symbol.id),
        ("lv_jobcount", jobcount_symbol.id),
    ] {
        let refs: Vec<_> = cls
            .references
            .iter()
            .filter(|reference| {
                reference.namespace == Namespace::Value && reference.name.as_ref() == name
            })
            .collect();
        assert!(!refs.is_empty(), "expected references for {name}");
        assert!(
            refs.iter().all(|reference| {
                matches!(
                    reference.resolution,
                    Some(Resolution::Symbol(handle))
                        if handle.unit == top.unit_id && handle.symbol == expected_symbol
                )
            }),
            "expected {name} references to resolve to top include member, got {refs:?}"
        );
        assert!(
            !cls.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected unresolved diagnostic for {name}: {:?}",
            cls.diagnostics
        );
    }
}

#[test]
fn reports_unknown_new_shorthand_constructor_parameter_from_definition_include_attribute() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
"#;
    let top_src = r#"
CLASS zcl_child DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING VALUE(container_name) TYPE string.
ENDCLASS.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS display.
  PRIVATE SECTION.
    DATA mo_cont TYPE REF TO zcl_child.
ENDCLASS.
"#;
    let f01_src = r#"
CLASS lcl_app IMPLEMENTATION.
  METHOD display.
    mo_cont = NEW #( container_name1 = 'CCONTAINER' ).
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

    let f01 = project.unit_by_uri("zf01.abap").expect("f01 include");
    assert!(
        f01.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownNamedParameter
                && diag.message.contains("container_name1")
        }),
        "expected unknown constructor parameter diagnostic, got {:?}",
        f01.diagnostics
    );
}

#[test]
fn resolves_method_parameters_from_definition_include_inside_implementation_include_methods() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
"#;
    let top_src = r#"
CLASS lcl_obj_epcis_repr DEFINITION.
  PUBLIC SECTION.
    METHODS extract_data
      RETURNING VALUE(rv_ok) TYPE c.
  PROTECTED SECTION.
    METHODS status_from_rep_evt_status
      IMPORTING iv_status_rep_evt TYPE i
      RETURNING VALUE(rv_status) TYPE string.
ENDCLASS.
"#;
    let f01_src = r#"
CLASS lcl_obj_epcis_repr IMPLEMENTATION.
  METHOD extract_data.
    rv_ok = 'X'.
  ENDMETHOD.

  METHOD status_from_rep_evt_status.
    CASE iv_status_rep_evt.
      WHEN 0.
        rv_status = 'warning'.
      WHEN 1.
        rv_status = 'success'.
      WHEN OTHERS.
        rv_status = 'error'.
    ENDCASE.
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

    let f01 = project
        .unit_by_uri("zf01.abap")
        .expect("implementation include");
    for name in ["rv_ok", "iv_status_rep_evt", "rv_status"] {
        assert!(
            !f01.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedReference && diag.message.contains(name)
            }),
            "unexpected unresolved diagnostic for {name}: {:?}",
            f01.diagnostics
        );

        let refs: Vec<_> = f01
            .references
            .iter()
            .filter(|reference| {
                reference.namespace == Namespace::Value && reference.name.as_ref() == name
            })
            .collect();
        assert!(!refs.is_empty(), "expected references for {name}");
        assert!(
            refs.iter().all(|reference| {
                matches!(
                    reference.resolution,
                    Some(Resolution::Symbol(handle))
                        if handle.unit == f01.unit_id
                            && f01.symbol(handle.symbol).kind == SymbolKind::Parameter
                )
            }),
            "expected {name} references to resolve to method parameters, got {refs:?}"
        );
    }
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
fn infers_loop_inline_target_type_from_table_declared_in_prior_include() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
"#;
    let top_src = r#"
TYPES: BEGIN OF ty_b2p_outs,
         objid TYPE string,
       END OF ty_b2p_outs,
       tt_b2p_outs TYPE TABLE OF ty_b2p_outs INITIAL SIZE 0.

DATA gt_b2p_outs TYPE tt_b2p_outs.
"#;
    let f01_src = r#"
FORM process_data.
  LOOP AT gt_b2p_outs INTO DATA(ls_b2p_outs).
    DATA(lv_objid) = ls_b2p_outs-objid.
  ENDLOOP.
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

    let f01 = project.unit_by_uri("zf01.abap").expect("form include");
    let top = project.unit_by_uri("ztop.abap").expect("top include");
    assert!(
        f01.references.iter().any(|reference| {
            reference.name.as_ref() == "gt_b2p_outs"
                && matches!(reference.resolution, Some(Resolution::Symbol(handle)) if handle.unit == top.unit_id)
        }),
        "expected LOOP source to resolve to top include symbol, refs={:?}",
        f01.references
    );

    let ls_b2p_outs = f01
        .symbols
        .iter()
        .find(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "ls_b2p_outs")
        .expect("loop inline target");
    let declared_type = ls_b2p_outs
        .declared_type
        .as_ref()
        .expect("loop inline target declared type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "ty_b2p_outs");
    assert!(declared_type.field_path.is_empty());

    assert!(
        !f01.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("gt_b2p_outs")
                || diag.message.contains("ls_b2p_outs")
                || diag.message.contains("objid"))
        }),
        "unexpected include LOOP diagnostics: {:?}",
        f01.diagnostics
    );
}

#[test]
fn infers_loop_inline_target_type_from_class_table_declared_in_prior_include() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
"#;
    let top_src = r#"
CLASS lcl_app DEFINITION.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_object_info,
             evtid TYPE string,
           END OF ty_object_info.
    TYPES tt_object_info TYPE STANDARD TABLE OF ty_object_info.
    DATA mt_object_info TYPE tt_object_info.
    METHODS run.
ENDCLASS.
"#;
    let f01_src = r#"
CLASS lcl_app IMPLEMENTATION.
  METHOD run.
    LOOP AT mt_object_info INTO DATA(ls_obj_info).
      DATA(lv_evtid) = ls_obj_info-evtid.
      DATA(lv_bad) = ls_obj_info-missing.
    ENDLOOP.
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

    let f01 = project.unit_by_uri("zf01.abap").expect("class include");
    let ls_obj_info = f01
        .symbols
        .iter()
        .find(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "ls_obj_info")
        .expect("loop inline target");
    let declared_type = ls_obj_info
        .declared_type
        .as_ref()
        .expect("loop inline target declared type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "ty_object_info");
    assert!(
        f01.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("missing")
        }),
        "expected unknown field diagnostic for class-local loop row type, diagnostics={:?}",
        f01.diagnostics
    );
}

#[test]
fn infers_append_inline_field_symbol_type_from_table_declared_in_prior_include() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
"#;
    let top_src = r#"
TYPES: BEGIN OF ty_fcat,
         fieldname TYPE string,
       END OF ty_fcat.
TYPES tt_fcat TYPE STANDARD TABLE OF ty_fcat WITH EMPTY KEY.
DATA mt_fieldcat TYPE tt_fcat.
"#;
    let f01_src = r#"
FORM display_alv.
  APPEND INITIAL LINE TO mt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
  <fs_fcat>-fieldname = 'DOCNUM'.
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

    let f01 = project.unit_by_uri("zf01.abap").expect("form include");
    let fs_fcat = f01
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == SymbolKind::FieldSymbol && symbol.name.as_ref() == "<fs_fcat>"
        })
        .expect("inline field-symbol");
    let declared_type = fs_fcat
        .declared_type
        .as_ref()
        .expect("inline field-symbol declared type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "ty_fcat");

    assert!(
        f01.value_flow_edges.iter().any(|edge| {
            matches!(
                &edge.target,
                abap_symbols::ValueFlowTargetData::FieldSymbol { name: Some(name), .. }
                    if name.as_ref() == "<fs_fcat>"
            ) && edge
                .target_type
                .declared_type
                .as_ref()
                .is_some_and(|type_ref| type_ref.base_name.as_ref() == "ty_fcat")
        }),
        "expected APPEND field-symbol binding to carry ty_fcat, edges={:?}",
        f01.value_flow_edges
    );
}

#[test]
fn infers_read_table_inline_field_symbol_type_from_class_table_declared_in_prior_include() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
"#;
    let top_src = r#"
CLASS lcl_app DEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_row,
             fieldname TYPE string,
           END OF ty_row.
    TYPES tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
    DATA mt_rows TYPE tt_row.
    METHODS run.
ENDCLASS.
"#;
    let f01_src = r#"
CLASS lcl_app IMPLEMENTATION.
  METHOD run.
    READ TABLE mt_rows WITH KEY fieldname = 'DOCNUM' ASSIGNING FIELD-SYMBOL(<row>).
    <row>-fieldname = 'DOCNUM'.
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

    let f01 = project.unit_by_uri("zf01.abap").expect("class include");
    let row = f01
        .symbols
        .iter()
        .find(|symbol| symbol.kind == SymbolKind::FieldSymbol && symbol.name.as_ref() == "<row>")
        .expect("inline field-symbol");
    let declared_type = row
        .declared_type
        .as_ref()
        .expect("inline field-symbol declared type");
    assert_eq!(declared_type.namespace, Namespace::Type);
    assert_eq!(declared_type.base_name.as_ref(), "ty_row");

    assert!(
        f01.value_flow_edges.iter().any(|edge| {
            matches!(
                &edge.target,
                abap_symbols::ValueFlowTargetData::FieldSymbol { name: Some(name), .. }
                    if name.as_ref() == "<row>"
            ) && edge
                .target_type
                .declared_type
                .as_ref()
                .is_some_and(|type_ref| type_ref.base_name.as_ref() == "ty_row")
        }),
        "expected READ TABLE field-symbol binding to carry ty_row, edges={:?}",
        f01.value_flow_edges
    );
}

#[test]
fn infers_loop_inline_target_type_from_select_options_declared_in_prior_include() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zsel,
         zf01.
"#;
    let top_src = "DATA gv_docnum TYPE string.";
    let sel_src = "SELECT-OPTIONS so_dels FOR gv_docnum.";
    let f01_src = r#"
FORM process_data.
  LOOP AT so_dels INTO DATA(ls_doc).
    DATA(lv_low) = ls_doc-low.
  ENDLOOP.
ENDFORM.
"#;
    let root_parse = parse(root_src);
    let top_parse = parse(top_src);
    let sel_parse = parse(sel_src);
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
            uri: "zsel.abap",
            source: sel_src,
            parse: &sel_parse,
        },
        ProjectInput {
            uri: "zf01.abap",
            source: f01_src,
            parse: &f01_parse,
        },
    ]);

    let f01 = project.unit_by_uri("zf01.abap").expect("form include");
    let sel = project.unit_by_uri("zsel.abap").expect("selection include");
    assert!(
        f01.references.iter().any(|reference| {
            reference.name.as_ref() == "so_dels"
                && matches!(reference.resolution, Some(Resolution::Symbol(handle)) if handle.unit == sel.unit_id)
        }),
        "expected LOOP source to resolve to selection include symbol, refs={:?}",
        f01.references
    );

    let ls_doc = f01
        .symbols
        .iter()
        .find(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "ls_doc")
        .expect("loop inline target");
    let declared_type = ls_doc
        .declared_type
        .as_ref()
        .expect("loop inline target declared type");
    assert_eq!(declared_type.namespace, Namespace::Value);
    assert_eq!(declared_type.base_name.as_ref(), "so_dels");
    assert_eq!(
        ls_doc.type_clause_display.as_deref(),
        Some("LINE OF so_dels")
    );

    assert!(
        !f01.diagnostics.iter().any(|diag| {
            matches!(
                diag.kind,
                DiagnosticKind::UnresolvedReference | DiagnosticKind::UnknownField
            ) && (diag.message.contains("so_dels")
                || diag.message.contains("ls_doc")
                || diag.message.contains("low"))
        }),
        "unexpected include SELECT-OPTIONS LOOP diagnostics: {:?}",
        f01.diagnostics
    );
}

#[test]
fn reports_ddic_table_type_use_without_tables_in_selection_screen_include_closure() {
    let root_src = r#"
REPORT zmain.
INCLUDE zf01.
"#;
    let include_src = "PARAMETERS p_lagp TYPE lagp.";
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
fn accepts_ddic_table_type_use_in_selection_screen_when_tables_is_declared_in_relevant_include() {
    let root_src = r#"
REPORT zmain.
INCLUDE: ztop,
         zf01.
"#;
    let top_src = "TABLES lagp.";
    let include_src = "PARAMETERS p_lagp TYPE lagp.";
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
fn reports_ddic_table_field_type_use_in_parameters_without_tables_work_area() {
    let root_src = r#"
REPORT zmain.
INCLUDE zf01.
"#;
    let include_src = "PARAMETERS p_jobname LIKE tbtco-jobname.";
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
fn ignores_ddic_table_type_use_outside_selection_screen_declarations() {
    let src = r#"
REPORT zmain.
DATA gt_lagp TYPE STANDARD TABLE OF lagp.
"#;
    let table_src = "TYPES lagp TYPE string.";
    let parse_src = parse(src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: src,
            parse: &parse_src,
        },
        ProjectInput {
            uri: "/sap/bc/adt/ddic/tables/lagp",
            source: table_src,
            parse: &table_parse,
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
fn ignores_ddic_table_field_type_use_outside_selection_screen_declarations() {
    let src = r#"
REPORT zmain.
DATA lv_jobname TYPE tbtco-jobname.
"#;
    let table_src = "TYPES: BEGIN OF tbtco, jobname TYPE string, END OF tbtco.";
    let parse_src = parse(src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: src,
            parse: &parse_src,
        },
        ProjectInput {
            uri: "/sap/bc/adt/vit/wb/object_type/tabldt/object_name/TBTCO",
            source: table_src,
            parse: &table_parse,
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
fn resolves_tables_work_area_against_ddic_table_like_dependency() {
    let src = r#"
REPORT zmain.
TABLES sscrfields.
sscrfields-ucomm = 'BUT1'.
"#;
    let table_src = "TYPES: BEGIN OF sscrfields, ucomm TYPE string, END OF sscrfields.";
    let parse_src = parse(src);
    let table_parse = parse(table_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zmain.abap",
            source: src,
            parse: &parse_src,
        },
        ProjectInput {
            uri: "/sap/bc/adt/vit/wb/object_type/tabldt/object_name/SSCRFIELDS",
            source: table_src,
            parse: &table_parse,
        },
    ]);

    let unit = project.unit_by_uri("zmain.abap").expect("report unit");
    let dependency = project
        .unit_by_uri("/sap/bc/adt/vit/wb/object_type/tabldt/object_name/SSCRFIELDS")
        .expect("dependency unit");

    assert!(unit.references.iter().any(|reference| {
        reference.kind == ReferenceKind::TypeRef
            && reference.namespace == Namespace::Type
            && reference.name.as_ref() == "sscrfields"
            && matches!(
                reference.resolution,
                Some(Resolution::Symbol(handle)) if handle.unit == dependency.unit_id
            )
    }));
    assert!(!unit.diagnostics.iter().any(|diag| {
        diag.kind == DiagnosticKind::UnresolvedReference
            || diag.kind == DiagnosticKind::WrongNamespace
            || diag.kind == DiagnosticKind::UnknownField
    }));
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

#[test]
fn infers_inline_method_result_from_inherited_protected_receiver() {
    let message_src = r#"
INTERFACE zif_message_container PUBLIC.
ENDINTERFACE.
"#;
    let context_src = r#"
INTERFACE zif_context PUBLIC.
  METHODS get_message_container
    RETURNING VALUE(ro_message_container) TYPE REF TO zif_message_container.
ENDINTERFACE.
"#;
    let parent_src = r#"
CLASS zcl_parent DEFINITION.
  PROTECTED SECTION.
    DATA mo_context TYPE REF TO zif_context.
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
    DATA(lo_msg) = mo_context->get_message_container( ).
  ENDMETHOD.
ENDCLASS.
"#;
    let message_parse = parse(message_src);
    let context_parse = parse(context_src);
    let parent_parse = parse(parent_src);
    let child_parse = parse(child_src);

    let project = analyze_project(&[
        ProjectInput {
            uri: "zif_message_container.abap",
            source: message_src,
            parse: &message_parse,
        },
        ProjectInput {
            uri: "zif_context.abap",
            source: context_src,
            parse: &context_parse,
        },
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

    let child = project.unit_by_uri("zcl_child.abap").expect("child unit");
    let lo_msg = child
        .symbols
        .iter()
        .find(|symbol| symbol.name.as_ref() == "lo_msg")
        .expect("inline variable");
    let declared_type = lo_msg.declared_type.as_ref().expect("inferred type");

    assert_eq!(declared_type.namespace, Namespace::Type);
    assert!(declared_type.is_ref);
    assert_eq!(declared_type.base_name.as_ref(), "zif_message_container");
}
