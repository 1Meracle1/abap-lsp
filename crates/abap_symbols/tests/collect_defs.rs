use abap_parser::parse;

use abap_symbols::{Namespace, ReferenceKind, Resolution, ScopeKind, SymbolKind, analyze_unit};

#[test]
fn collects_top_level_and_nested_definitions() {
    let src = r#"
FORM run.
  DATA lv_local TYPE i.
ENDFORM.

CLASS lcl_demo IMPLEMENTATION.
  METHOD execute.
    DATA lv_inner TYPE i.
  ENDMETHOD.
ENDCLASS.

DATA gv_value TYPE i.
TYPES ty_name TYPE string.
CONSTANTS gc_limit TYPE i VALUE 1.
FIELD-SYMBOLS <fs_row> TYPE any.
"#;

    let parsed = parse(src);
    let unit = analyze_unit("file:///defs.abap", src, &parsed);

    assert!(
        unit.symbols
            .iter()
            .any(|symbol| { symbol.kind == SymbolKind::Form && symbol.name.as_ref() == "run" })
    );
    assert!(
        unit.symbols.iter().any(|symbol| {
            symbol.kind == SymbolKind::Class && symbol.name.as_ref() == "lcl_demo"
        })
    );
    assert!(
        unit.symbols.iter().any(|symbol| {
            symbol.kind == SymbolKind::Method && symbol.name.as_ref() == "execute"
        })
    );
    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "gv_value"
    }));
    assert!(
        unit.symbols.iter().any(|symbol| {
            symbol.kind == SymbolKind::TypeDef && symbol.name.as_ref() == "ty_name"
        })
    );
    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == SymbolKind::Constant && symbol.name.as_ref() == "gc_limit"
    }));
    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == SymbolKind::FieldSymbol && symbol.name.as_ref() == "<fs_row>"
    }));
    assert!(
        unit.scopes
            .iter()
            .any(|scope| scope.kind == ScopeKind::Form)
    );
    assert!(
        unit.scopes
            .iter()
            .any(|scope| scope.kind == ScopeKind::Method)
    );
}

#[test]
fn collects_tables_work_area_declarations() {
    let src = r#"
TABLES: tbtco, v_op.
tbtco-jobname = v_op-name.
"#;

    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///tables.abap", src, &parsed);

    for name in ["tbtco", "v_op"] {
        let symbol = unit
            .symbols
            .iter()
            .find(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == name)
            .unwrap_or_else(|| panic!("missing TABLES work area symbol {name}"));
        let declared_type = symbol
            .declared_type
            .as_ref()
            .unwrap_or_else(|| panic!("missing synthesized type for {name}"));
        assert_eq!(declared_type.namespace, Namespace::Type);
        assert_eq!(declared_type.base_name.as_ref(), name);
        assert_eq!(symbol.type_clause_display.as_deref(), Some(name));
        assert!(
            unit.table_work_areas
                .iter()
                .any(|work_area| work_area.name.as_ref() == name)
        );

        assert!(unit.references.iter().any(|reference| {
            reference.name.as_ref() == name
                && reference.namespace == Namespace::Type
                && reference.kind == ReferenceKind::TypeRef
        }));
        assert!(unit.references.iter().any(|reference| {
            reference.name.as_ref() == name
                && reference.namespace == Namespace::Value
                && matches!(reference.resolution, Some(Resolution::Symbol(_)))
        }));
    }
}

#[test]
fn common_part_delimiters_do_not_emit_bogus_symbols() {
    let src = r#"
DATA: BEGIN OF COMMON PART fm06lcbe.
DATA: BEGIN OF bet OCCURS 50.
        INCLUDE STRUCTURE ekbe.
DATA: END OF bet.
DATA: END OF COMMON PART.
"#;

    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///common_part.abap", src, &parsed);

    assert!(
        unit.symbols
            .iter()
            .any(|symbol| { symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "bet" })
    );
    for bogus in ["begin", "common", "end"] {
        assert!(
            !unit
                .symbols
                .iter()
                .any(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == bogus),
            "unexpected common-part delimiter symbol {bogus}: {:?}",
            unit.symbols
        );
        assert!(
            !unit.references.iter().any(|reference| {
                reference.kind == ReferenceKind::Identifier && reference.name.as_ref() == bogus
            }),
            "unexpected common-part delimiter reference {bogus}: {:?}",
            unit.references
        );
    }
}

#[test]
fn legacy_occurs_header_line_keeps_declared_type_clean() {
    let src = "DATA int_eket LIKE beket OCCURS 0 WITH HEADER LINE.";
    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///occurs_header.abap", src, &parsed);

    let symbol = unit
        .symbols
        .iter()
        .find(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "int_eket")
        .expect("int_eket symbol");
    let declared_type = symbol.declared_type.as_ref().expect("declared type");
    assert_eq!(declared_type.namespace, Namespace::Value);
    assert_eq!(declared_type.base_name.as_ref(), "beket");
    assert_eq!(symbol.type_clause_display.as_deref(), Some("beket"));
}

#[test]
fn constant_structure_collects_numeric_prefixed_component_names() {
    let src = r#"
CONSTANTS: BEGIN OF gc_bapi_proc_mode,
             aip VALUE 'A',
             46c VALUE 'B',
           END OF gc_bapi_proc_mode.
"#;

    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///constant_components.abap", src, &parsed);

    let symbol = unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.kind == SymbolKind::Constant && symbol.name.as_ref() == "gc_bapi_proc_mode"
        })
        .expect("constant structure symbol");
    let structure_id = symbol.structure.expect("constant structure id");
    let structure = &unit.structures[structure_id.as_usize()];
    let field_names = structure
        .fields
        .iter()
        .map(|field| field.name.as_ref())
        .collect::<Vec<_>>();
    assert_eq!(field_names, vec!["aip", "46c"]);
}

#[test]
fn collects_amdp_method_symbol_and_using_dependencies() {
    let src = r#"
CLASS zcl_attp_ua_dep_rl_amdp DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    CLASS-METHODS get_rel_rep_evt.
ENDCLASS.

CLASS zcl_attp_ua_dep_rl_amdp IMPLEMENTATION.
  METHOD get_rel_rep_evt
         BY DATABASE PROCEDURE
         FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING /sttp/rep_evt zattp_t_mat_prp.
    et_rep_dep = SELECT rep_evtid, evtid
                   FROM "/STTP/REP_EVT" AS rep
                   INNER JOIN :it_ua_dp_rl AS ua
                   ON rep.rule_type = ua.previousruletype;
  ENDMETHOD.
ENDCLASS.
"#;

    let parsed = parse(src);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let unit = analyze_unit("file:///amdp.abap", src, &parsed);

    assert!(unit.symbols.iter().any(|symbol| {
        symbol.kind == SymbolKind::Method && symbol.name.as_ref() == "get_rel_rep_evt"
    }));
    for name in ["/sttp/rep_evt", "zattp_t_mat_prp"] {
        assert!(
            unit.references.iter().any(|reference| {
                reference.name.as_ref() == name
                    && reference.namespace == Namespace::Type
                    && reference.kind == ReferenceKind::TypeRef
            }),
            "missing AMDP USING dependency reference {name}: {:?}",
            unit.references
        );
    }
    assert!(
        unit.references
            .iter()
            .all(|reference| reference.name.as_ref() != "rep_evtid"),
        "SQLScript body identifiers should stay opaque: {:?}",
        unit.references
    );
}
