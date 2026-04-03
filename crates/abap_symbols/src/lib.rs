mod builtins;
mod collector;
mod def_map;
mod ids;
mod project;
mod resolver;
mod scope;
mod validate;

pub use builtins::{BuiltinRoutineParamSpec, BuiltinRoutineSpec, builtin_routine_spec};
pub use def_map::{
    ClassMemberData, ClassMemberKind, ClassMemberParameterData, Diagnostic, DiagnosticKind,
    FieldAccess, FieldAccessSegment, FieldTypeRefData, FormParameterData,
    FormParameterPassingKind, FormParameterSection, FormRoutineData, IncludeEdge,
    NamedArgumentAccess, NamedArgumentTarget, PerformArgumentData, PerformCallData,
    PerformParameterSection, ReferenceData, ReferenceKind, Resolution, StructureData,
    StructureFieldData, StructureFieldInfo, StructureFieldShape, SymbolData, SymbolKind,
    UnitAnalysis, Visibility,
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

        assert!(unit.symbols.iter().any(
            |symbol| symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "lv_value"
        ));
        assert!(unit.references.iter().any(|reference| {
            reference.namespace == Namespace::Value
                && reference.name.as_ref() == "lv_value"
                && reference.resolution.is_some()
        }));
    }

    #[test]
    fn event_block_uses_full_hyphenated_keyword_as_symbol_name() {
        let src = "START-OF-SELECTION.\n  DATA lv TYPE i.\n";
        let parsed = parse(src);
        let unit = analyze_unit("file:///event.abap", src, &parsed);

        let event = unit
            .symbols
            .iter()
            .find(|symbol| symbol.kind == SymbolKind::Event)
            .expect("event symbol");
        assert_eq!(event.name.as_ref(), "start-of-selection");
        assert_eq!(&src[event.decl_range.clone()], "START-OF-SELECTION");
    }
}
