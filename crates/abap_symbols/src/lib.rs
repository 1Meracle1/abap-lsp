use abap_ast::File;
use abap_lexer::{TextRange, Token, TokenKind};
use abap_parser::{Interner, Symbol as InternedSymbol};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: InternedSymbol,
    pub kind: SymbolKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SymbolTable {
    pub symbols: Vec<Symbol>,
}

pub fn index_file(
    _file: &File,
    tokens: &[Token],
    token_symbols: &[Option<InternedSymbol>],
) -> SymbolTable {
    let symbols = tokens
        .iter()
        .zip(token_symbols.iter().copied())
        .filter(|(token, _)| token.kind == TokenKind::Ident)
        .filter_map(|(token, name)| {
            name.map(|name| Symbol {
                name,
                kind: SymbolKind::Identifier,
                range: token.range.clone(),
            })
        })
        .collect();

    SymbolTable { symbols }
}

pub fn resolve_name<'a>(interner: &'a Interner, symbol: &Symbol) -> &'a str {
    interner.resolve(symbol.name)
}

#[cfg(test)]
mod tests {
    use abap_parser::parse;

    use super::{index_file, resolve_name};

    #[test]
    fn collects_identifier_symbols() {
        let parsed = parse("DATA foo data = 42.");
        let table = index_file(&parsed.file, &parsed.tokens, &parsed.token_symbols);

        assert_eq!(table.symbols.len(), 3);
        assert!(table
            .symbols
            .iter()
            .any(|symbol| resolve_name(&parsed.interner, symbol) == "data"));
    }
}
