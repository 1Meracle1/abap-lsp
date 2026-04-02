mod builtins;
mod collector;
mod def_map;
mod ids;
mod project;
mod resolver;
mod scope;
mod validate;

pub use def_map::{
    Diagnostic, DiagnosticKind, FieldAccess, IncludeEdge, ReferenceData, ReferenceKind, Resolution,
    StructureData, StructureFieldData, SymbolData, SymbolKind, UnitAnalysis,
};
pub use ids::{ReferenceId, ScopeId, StructureId, SymbolHandle, SymbolId, UnitId};
pub use project::{ProjectAnalysis, ProjectInput, analyze_project, analyze_unit};
pub use scope::{Namespace, ScopeData, ScopeKind};

#[cfg(test)]
mod tests {
    use abap_parser::parse;

    use super::{Namespace, SymbolKind, analyze_unit};

    #[test]
    fn collects_definitions_and_references() {
        let src = "DATA lv_value TYPE i. lv_value = lv_value + 1.";
        let parsed = parse(src);
        let unit = analyze_unit("file:///demo.abap", src, &parsed);

        assert!(unit
            .symbols
            .iter()
            .any(|symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "lv_value"));
        assert!(unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Value
                && reference.name.as_ref() == "lv_value"
                && reference.resolution.is_some()
        }));
    }
}
