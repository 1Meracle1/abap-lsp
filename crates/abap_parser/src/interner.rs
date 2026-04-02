use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub u32);

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Interner {
    strings: Vec<Arc<str>>,
    index: HashMap<Arc<str>, Symbol>,
}

impl Interner {
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    pub fn intern_lowercase(&mut self, text: &str, scratch: &mut String) -> Symbol {
        scratch.clear();
        scratch.reserve(text.len());
        scratch.extend(text.chars().flat_map(char::to_lowercase));

        if let Some(&symbol) = self.index.get(scratch.as_str()) {
            return symbol;
        }

        let symbol = Symbol(self.strings.len() as u32);
        let interned: Arc<str> = Arc::from(scratch.as_str());
        self.strings.push(Arc::clone(&interned));
        self.index.insert(interned, symbol);
        symbol
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        self.strings
            .get(symbol.0 as usize)
            .map(|text| text.as_ref())
            .unwrap_or("")
    }
}
