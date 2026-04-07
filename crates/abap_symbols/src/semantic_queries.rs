use abap_lexer::TextRange;

use crate::def_map::{
    ClassInheritanceData, ClassMemberData, FormParameterData, FormRoutineData, ReferenceData,
    SqlNameRefData, StructureFieldData, StructureFieldInfo, SymbolData, SymbolKind, UnitAnalysis,
};
use crate::ids::{ScopeId, StructureId, SymbolHandle, SymbolId};

#[derive(Clone, Copy)]
pub struct SemanticQueries<'a> {
    unit: &'a UnitAnalysis,
}

#[derive(Clone, Copy)]
pub struct DeclQueries<'a> {
    unit: &'a UnitAnalysis,
}

#[derive(Clone, Copy)]
pub struct RefQueries<'a> {
    unit: &'a UnitAnalysis,
}

#[derive(Clone, Copy)]
pub struct SqlQueries<'a> {
    unit: &'a UnitAnalysis,
}

impl<'a> SemanticQueries<'a> {
    pub(crate) fn new(unit: &'a UnitAnalysis) -> Self {
        Self { unit }
    }

    pub fn decls(self) -> DeclQueries<'a> {
        DeclQueries { unit: self.unit }
    }

    pub fn refs(self) -> RefQueries<'a> {
        RefQueries { unit: self.unit }
    }

    pub fn sql(self) -> SqlQueries<'a> {
        SqlQueries { unit: self.unit }
    }
}

impl<'a> DeclQueries<'a> {
    pub fn symbol_at_offset(self, offset: usize) -> Option<&'a SymbolData> {
        self.unit.symbol_at_offset(offset)
    }

    pub fn structure_field(
        self,
        structure_id: StructureId,
        field_name: &str,
    ) -> Option<&'a StructureFieldData> {
        self.unit.structure_field(structure_id, field_name)
    }

    pub fn structure_field_info(
        self,
        structure_id: StructureId,
        field_name: &str,
    ) -> Option<StructureFieldInfo> {
        self.unit.structure_field_info(structure_id, field_name)
    }

    pub fn structure_field_infos(self, structure_id: StructureId) -> Vec<StructureFieldInfo> {
        self.unit.structure_field_infos(structure_id)
    }

    pub fn resolve_structure_field_path(
        self,
        structure_id: StructureId,
        field_path: &[&str],
    ) -> Option<StructureFieldInfo> {
        self.unit
            .resolve_structure_field_path(structure_id, field_path)
    }

    pub fn class_member_at_offset(self, offset: usize) -> Option<&'a ClassMemberData> {
        self.unit.class_member_at_offset(offset)
    }

    pub fn class_member(self, class_symbol: SymbolId, name: &str) -> Option<&'a ClassMemberData> {
        self.unit.class_member(class_symbol, name)
    }

    pub fn class_members_for(
        self,
        class_symbol: SymbolId,
    ) -> impl Iterator<Item = &'a ClassMemberData> + 'a {
        self.unit.class_members_for(class_symbol)
    }

    pub fn class_superclass(self, class_symbol: SymbolId) -> Option<&'a ClassInheritanceData> {
        self.unit.class_superclass(class_symbol)
    }

    pub fn form_routine(self, symbol: SymbolId) -> Option<&'a FormRoutineData> {
        self.unit.form_routine(symbol)
    }

    pub fn form_parameter(self, symbol: SymbolId) -> Option<&'a FormParameterData> {
        self.unit.form_parameter(symbol)
    }

    pub fn routine_parameters(
        self,
        routine_symbol: SymbolId,
    ) -> impl Iterator<Item = &'a SymbolData> + 'a {
        self.unit.routine_parameters(routine_symbol)
    }

    pub fn symbol_with_kind_and_decl_range(
        self,
        kind: SymbolKind,
        range: &TextRange,
    ) -> Option<&'a SymbolData> {
        self.unit.symbol_with_kind_and_decl_range(kind, range)
    }

    pub fn structure_field_at_offset(self, offset: usize) -> Option<StructureFieldInfo> {
        self.unit.structure_field_at_offset(offset)
    }
}

impl<'a> RefQueries<'a> {
    pub fn reference_at_offset(self, offset: usize) -> Option<&'a ReferenceData> {
        self.unit.reference_at_offset(offset)
    }

    pub fn all(self) -> impl Iterator<Item = &'a ReferenceData> + 'a {
        self.unit.references.iter()
    }

    pub fn type_reference_at_offset(self, offset: usize) -> Option<&'a ReferenceData> {
        self.unit.type_reference_at_offset(offset)
    }

    pub fn resolving_to(
        self,
        handle: SymbolHandle,
    ) -> impl Iterator<Item = &'a ReferenceData> + 'a {
        self.unit.references_resolving_to(handle)
    }

    pub fn type_named(self, name: &str) -> impl Iterator<Item = &'a ReferenceData> + 'a {
        self.unit.type_references_named(name)
    }

    pub fn in_scope(self, scope: ScopeId) -> impl Iterator<Item = &'a ReferenceData> + 'a {
        self.unit.references_in_scope(scope)
    }
}

impl<'a> SqlQueries<'a> {
    pub fn name_ref_at_offset(self, offset: usize) -> Option<&'a SqlNameRefData> {
        self.unit.sql_name_ref_at_offset(offset)
    }

    pub fn source_name_refs_named(
        self,
        name: &str,
    ) -> impl Iterator<Item = &'a SqlNameRefData> + 'a {
        self.unit.sql_source_name_refs_named(name)
    }

    pub fn name_refs(self) -> impl Iterator<Item = &'a SqlNameRefData> + 'a {
        self.unit.sql_name_refs.iter()
    }

    pub fn has_source_named(self, name: &str) -> bool {
        self.unit.has_sql_source_named(name)
    }
}
