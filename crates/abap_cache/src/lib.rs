use std::collections::HashMap;
use std::sync::Arc;

use abap_parser::{ParseResult, parse};
use abap_symbols::{SymbolTable, index_file};
use parking_lot::RwLock;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisSnapshot {
    pub uri: Arc<str>,
    pub version: i32,
    pub text: Arc<str>,
    pub parse: Arc<ParseResult>,
    pub symbols: Arc<SymbolTable>,
}

#[derive(Debug, Default)]
pub struct DocumentStore {
    documents: RwLock<HashMap<Arc<str>, Arc<AnalysisSnapshot>>>,
}

impl DocumentStore {
    pub fn publish(&self, uri: impl Into<Arc<str>>, version: i32, text: &str) -> Arc<AnalysisSnapshot> {
        let uri = uri.into();
        let parse = Arc::new(parse(text));
        let symbols = Arc::new(index_file(
            &parse.file,
            &parse.tokens,
            &parse.token_symbols,
        ));
        let snapshot = Arc::new(AnalysisSnapshot {
            uri: Arc::clone(&uri),
            version,
            text: Arc::<str>::from(text),
            parse,
            symbols,
        });

        self.documents
            .write()
            .insert(Arc::clone(&uri), Arc::clone(&snapshot));

        snapshot
    }

    pub fn get(&self, uri: &str) -> Option<Arc<AnalysisSnapshot>> {
        self.documents.read().get(uri).cloned()
    }

    pub fn len(&self) -> usize {
        self.documents.read().len()
    }
}

#[cfg(test)]
mod tests {
    use super::DocumentStore;

    #[test]
    fn publishes_snapshots_immutably() {
        let store = DocumentStore::default();
        let snapshot = store.publish("file:///demo.abap", 1, "DATA foo = 42.");

        assert_eq!(store.len(), 1);
        assert_eq!(snapshot.symbols.symbols.len(), 2);
        assert_eq!(store.get("file:///demo.abap").unwrap().version, 1);
    }
}
