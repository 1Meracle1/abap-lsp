use abap_parser::parse;

use abap_symbols::{ScopeKind, SymbolKind, analyze_unit};

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
