use std::collections::HashMap;
use std::sync::Arc;

use abap_parser::{ParseResult, parse};
use abap_symbols::{ProjectAnalysis, ProjectInput, UnitAnalysis, analyze_project};
use parking_lot::RwLock;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisSnapshot {
    pub uri: Arc<str>,
    pub version: i32,
    pub text: Arc<str>,
    pub parse: Arc<ParseResult>,
    pub symbols: Arc<UnitAnalysis>,
    pub project: Arc<ProjectAnalysis>,
}

#[derive(Debug, Default)]
pub struct DocumentStore {
    documents: RwLock<HashMap<Arc<str>, Arc<AnalysisSnapshot>>>,
}

impl DocumentStore {
    pub fn publish(&self, uri: impl Into<Arc<str>>, version: i32, text: &str) -> Arc<AnalysisSnapshot> {
        let uri = uri.into();
        let text = Arc::<str>::from(text);
        let parse = Arc::new(parse(&text));

        let existing = self.documents.read();
        let mut staged: Vec<(Arc<str>, i32, Arc<str>, Arc<ParseResult>)> = existing
            .values()
            .map(|snapshot| {
                (
                    Arc::clone(&snapshot.uri),
                    snapshot.version,
                    Arc::clone(&snapshot.text),
                    Arc::clone(&snapshot.parse),
                )
            })
            .collect();
        drop(existing);

        if let Some(existing) = staged.iter_mut().find(|(existing_uri, _, _, _)| existing_uri.as_ref() == uri.as_ref()) {
            *existing = (Arc::clone(&uri), version, Arc::clone(&text), Arc::clone(&parse));
        } else {
            staged.push((Arc::clone(&uri), version, Arc::clone(&text), Arc::clone(&parse)));
        }

        let inputs: Vec<ProjectInput<'_>> = staged
            .iter()
            .map(|(uri, _, text, parse)| ProjectInput {
                uri: uri.as_ref(),
                source: text.as_ref(),
                parse,
            })
            .collect();
        let project = Arc::new(analyze_project(&inputs));

        let mut rebuilt = HashMap::new();
        let mut published = None;
        for (entry_uri, entry_version, entry_text, entry_parse) in staged {
            let unit = project
                .unit_by_uri(entry_uri.as_ref())
                .cloned()
                .expect("project analysis should include every published document");
            let snapshot = Arc::new(AnalysisSnapshot {
                uri: Arc::clone(&entry_uri),
                version: entry_version,
                text: entry_text,
                parse: entry_parse,
                symbols: Arc::new(unit),
                project: Arc::clone(&project),
            });
            if entry_uri.as_ref() == uri.as_ref() {
                published = Some(Arc::clone(&snapshot));
            }
            rebuilt.insert(entry_uri, snapshot);
        }

        self.documents.write().clone_from(&rebuilt);
        published.expect("published snapshot should exist")
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
        let snapshot = store.publish("file:///demo.abap", 1, "DATA foo TYPE i.");

        assert_eq!(store.len(), 1);
        assert!(snapshot
            .symbols
            .symbols
            .iter()
            .any(|symbol| symbol.name.as_ref() == "foo"));
        assert_eq!(store.get("file:///demo.abap").unwrap().version, 1);
    }
}
