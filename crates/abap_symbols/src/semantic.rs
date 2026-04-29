#![allow(dead_code)]

use std::collections::HashMap;

use abap_lexer::TextRange;

use crate::def_map::{
    ClassMemberKind, ReferenceKind, Resolution, SqlNameRefKind, SymbolKind, UnitAnalysis,
};
use crate::ids::{ReferenceId, ScopeId, StructureId, SymbolHandle, SymbolId};

macro_rules! semantic_id_type {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub(crate) struct $name(pub(crate) u32);

        impl $name {
            pub(crate) const fn as_usize(self) -> usize {
                self.0 as usize
            }
        }
    };
}

semantic_id_type!(SemSymbolId);
semantic_id_type!(SemReferenceId);
semantic_id_type!(SemScopeId);
semantic_id_type!(SemSqlQueryId);
semantic_id_type!(SemSqlNameRefId);
semantic_id_type!(SemClassMemberId);
semantic_id_type!(SemStructureFieldId);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemSymbol {
    pub(crate) symbol_id: SymbolId,
    pub(crate) scope: ScopeId,
    pub(crate) decl_range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemReference {
    pub(crate) reference_id: ReferenceId,
    pub(crate) scope: ScopeId,
    pub(crate) range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemScope {
    pub(crate) scope_id: ScopeId,
    pub(crate) parent: Option<ScopeId>,
    pub(crate) range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemSqlQuery {
    pub(crate) query_id: usize,
    pub(crate) scope: ScopeId,
    pub(crate) range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemSqlNameRef {
    pub(crate) raw_index: usize,
    pub(crate) query_id: usize,
    pub(crate) range: TextRange,
    pub(crate) kind: SqlNameRefKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemClassMember {
    pub(crate) raw_index: usize,
    pub(crate) class_symbol: SymbolId,
    pub(crate) kind: ClassMemberKind,
    pub(crate) decl_range: TextRange,
    pub(crate) implementation_range: Option<TextRange>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SemStructureField {
    pub(crate) structure_id: StructureId,
    pub(crate) raw_index: usize,
    pub(crate) name: String,
    pub(crate) decl_range: TextRange,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct SemanticIndex {
    symbols: Vec<SemSymbol>,
    references: Vec<SemReference>,
    scopes: Vec<SemScope>,
    sql_queries: Vec<SemSqlQuery>,
    sql_name_refs: Vec<SemSqlNameRef>,
    class_members: Vec<SemClassMember>,
    structure_fields: Vec<SemStructureField>,
    references_by_resolution: HashMap<SymbolHandle, Vec<SemReferenceId>>,
    references_by_scope: HashMap<ScopeId, Vec<SemReferenceId>>,
    type_references_by_name: HashMap<String, Vec<SemReferenceId>>,
    sql_source_name_refs_by_name: HashMap<String, Vec<SemSqlNameRefId>>,
    sql_sources_by_name: HashMap<String, Vec<usize>>,
    symbols_by_kind_and_range: HashMap<(u8, usize, usize), Vec<SemSymbolId>>,
}

impl SemanticIndex {
    pub(crate) fn build(unit: &UnitAnalysis) -> Self {
        let mut symbols = Vec::with_capacity(unit.symbols.len());
        let mut symbols_by_kind_and_range: HashMap<(u8, usize, usize), Vec<SemSymbolId>> =
            HashMap::new();
        for (idx, symbol) in unit.symbols.iter().enumerate() {
            let id = SemSymbolId(idx as u32);
            symbols.push(SemSymbol {
                symbol_id: SymbolId(idx as u32),
                scope: symbol.scope,
                decl_range: symbol.decl_range.clone(),
            });
            symbols_by_kind_and_range
                .entry((
                    symbol_kind_key(symbol.kind),
                    symbol.decl_range.start,
                    symbol.decl_range.end,
                ))
                .or_default()
                .push(id);
        }
        let mut references = Vec::with_capacity(unit.references.len());
        let mut references_by_resolution: HashMap<SymbolHandle, Vec<SemReferenceId>> =
            HashMap::new();
        let mut references_by_scope: HashMap<ScopeId, Vec<SemReferenceId>> = HashMap::new();
        let mut type_references_by_name: HashMap<String, Vec<SemReferenceId>> = HashMap::new();
        for (idx, reference) in unit.references.iter().enumerate() {
            let id = SemReferenceId(idx as u32);
            references.push(SemReference {
                reference_id: ReferenceId(idx as u32),
                scope: reference.scope,
                range: reference.range.clone(),
            });
            if let Some(Resolution::Symbol(handle)) = reference.resolution {
                references_by_resolution.entry(handle).or_default().push(id);
            }
            references_by_scope
                .entry(reference.scope)
                .or_default()
                .push(id);
            if reference.kind == ReferenceKind::TypeRef {
                type_references_by_name
                    .entry(reference.name.to_ascii_lowercase())
                    .or_default()
                    .push(id);
            }
        }
        let scopes = unit
            .scopes
            .iter()
            .enumerate()
            .map(|(idx, scope)| SemScope {
                scope_id: ScopeId(idx as u32),
                parent: scope.parent,
                range: scope.range.clone(),
            })
            .collect();
        let sql_queries = unit
            .sql_queries
            .iter()
            .enumerate()
            .map(|(idx, query)| SemSqlQuery {
                query_id: idx,
                scope: query.scope,
                range: query.range.clone(),
            })
            .collect();
        let mut sql_name_refs = Vec::with_capacity(unit.sql_name_refs.len());
        let mut sql_source_name_refs_by_name: HashMap<String, Vec<SemSqlNameRefId>> =
            HashMap::new();
        for (idx, sql_ref) in unit.sql_name_refs.iter().enumerate() {
            let id = SemSqlNameRefId(idx as u32);
            sql_name_refs.push(SemSqlNameRef {
                raw_index: idx,
                query_id: sql_ref.query_id,
                range: sql_ref.range.clone(),
                kind: sql_ref.kind,
            });
            if sql_ref.kind == SqlNameRefKind::Source {
                sql_source_name_refs_by_name
                    .entry(sql_ref.name.to_ascii_lowercase())
                    .or_default()
                    .push(id);
            }
        }
        let class_members = unit
            .class_members
            .iter()
            .enumerate()
            .map(|(idx, member)| SemClassMember {
                raw_index: idx,
                class_symbol: member.class_symbol,
                kind: member.kind,
                decl_range: member.decl_range.clone(),
                implementation_range: member.implementation_range.clone(),
            })
            .collect();
        let structure_fields = unit
            .structures
            .iter()
            .flat_map(|structure| {
                structure
                    .fields
                    .iter()
                    .enumerate()
                    .filter_map(move |(idx, field)| {
                        let decl_range = field.decl_range.clone()?;
                        Some(SemStructureField {
                            structure_id: structure.id,
                            raw_index: idx,
                            name: field.name.to_ascii_lowercase(),
                            decl_range,
                        })
                    })
            })
            .collect();
        let mut sql_sources_by_name: HashMap<String, Vec<usize>> = HashMap::new();
        for (idx, source) in unit.sql_sources.iter().enumerate() {
            sql_sources_by_name
                .entry(source.name.to_ascii_lowercase())
                .or_default()
                .push(idx);
        }

        Self {
            symbols,
            references,
            scopes,
            sql_queries,
            sql_name_refs,
            class_members,
            structure_fields,
            references_by_resolution,
            references_by_scope,
            type_references_by_name,
            sql_source_name_refs_by_name,
            sql_sources_by_name,
            symbols_by_kind_and_range,
        }
    }

    pub(crate) fn symbol(&self, id: SemSymbolId) -> &SemSymbol {
        &self.symbols[id.as_usize()]
    }

    pub(crate) fn reference(&self, id: SemReferenceId) -> &SemReference {
        &self.references[id.as_usize()]
    }

    pub(crate) fn scope(&self, id: SemScopeId) -> &SemScope {
        &self.scopes[id.as_usize()]
    }

    pub(crate) fn sql_query(&self, id: SemSqlQueryId) -> &SemSqlQuery {
        &self.sql_queries[id.as_usize()]
    }

    pub(crate) fn sql_name_ref(&self, id: SemSqlNameRefId) -> &SemSqlNameRef {
        &self.sql_name_refs[id.as_usize()]
    }

    pub(crate) fn class_member(&self, id: SemClassMemberId) -> &SemClassMember {
        &self.class_members[id.as_usize()]
    }

    pub(crate) fn structure_field(&self, id: SemStructureFieldId) -> &SemStructureField {
        &self.structure_fields[id.as_usize()]
    }

    pub(crate) fn symbol_at_offset(&self, offset: usize) -> Option<SemSymbolId> {
        self.symbols
            .iter()
            .enumerate()
            .filter(|(_, symbol)| {
                symbol.decl_range.start <= offset && offset < symbol.decl_range.end
            })
            .min_by_key(|(_, symbol)| {
                symbol
                    .decl_range
                    .end
                    .saturating_sub(symbol.decl_range.start)
            })
            .map(|(idx, _)| SemSymbolId(idx as u32))
    }

    pub(crate) fn class_member_at_offset(&self, offset: usize) -> Option<SemClassMemberId> {
        self.class_members
            .iter()
            .enumerate()
            .filter(|(_, member)| {
                (member.decl_range.start <= offset && offset < member.decl_range.end)
                    || member
                        .implementation_range
                        .as_ref()
                        .is_some_and(|range| range.start <= offset && offset < range.end)
            })
            .min_by_key(|(_, member)| {
                let decl_width = member
                    .decl_range
                    .end
                    .saturating_sub(member.decl_range.start);
                let impl_width = member
                    .implementation_range
                    .as_ref()
                    .map(|range| range.end.saturating_sub(range.start))
                    .unwrap_or(usize::MAX);
                decl_width.min(impl_width)
            })
            .map(|(idx, _)| SemClassMemberId(idx as u32))
    }

    pub(crate) fn symbol_with_kind_and_decl_range(
        &self,
        kind: SymbolKind,
        range: &TextRange,
    ) -> Option<SemSymbolId> {
        self.symbols_by_kind_and_range
            .get(&(symbol_kind_key(kind), range.start, range.end))
            .and_then(|ids| ids.first().copied())
    }

    pub(crate) fn structure_field_at_offset(&self, offset: usize) -> Option<SemStructureFieldId> {
        self.structure_fields
            .iter()
            .enumerate()
            .filter(|(_, field)| field.decl_range.start <= offset && offset < field.decl_range.end)
            .min_by_key(|(_, field)| field.decl_range.end.saturating_sub(field.decl_range.start))
            .map(|(idx, _)| SemStructureFieldId(idx as u32))
    }

    pub(crate) fn reference_at_offset(&self, offset: usize) -> Option<SemReferenceId> {
        self.references
            .iter()
            .enumerate()
            .filter(|(_, reference)| {
                reference.range.start <= offset && offset < reference.range.end
            })
            .min_by_key(|(_, reference)| reference.range.end.saturating_sub(reference.range.start))
            .map(|(idx, _)| SemReferenceId(idx as u32))
    }

    pub(crate) fn references_resolving_to(
        &self,
        handle: SymbolHandle,
    ) -> impl Iterator<Item = SemReferenceId> + '_ {
        self.references_by_resolution
            .get(&handle)
            .into_iter()
            .flat_map(|ids| ids.iter().copied())
    }

    pub(crate) fn references_in_scope(
        &self,
        scope: ScopeId,
    ) -> impl Iterator<Item = SemReferenceId> + '_ {
        self.references_by_scope
            .get(&scope)
            .into_iter()
            .flat_map(|ids| ids.iter().copied())
    }

    pub(crate) fn type_references_named(
        &self,
        name: &str,
    ) -> impl Iterator<Item = SemReferenceId> + '_ {
        self.type_references_by_name
            .get(&name.to_ascii_lowercase())
            .into_iter()
            .flat_map(|ids| ids.iter().copied())
    }

    pub(crate) fn sql_source_name_refs_named(
        &self,
        name: &str,
    ) -> impl Iterator<Item = SemSqlNameRefId> + '_ {
        self.sql_source_name_refs_by_name
            .get(&name.to_ascii_lowercase())
            .into_iter()
            .flat_map(|ids| ids.iter().copied())
    }

    pub(crate) fn has_sql_source_named(&self, name: &str) -> bool {
        self.sql_sources_by_name
            .contains_key(&name.to_ascii_lowercase())
    }

    pub(crate) fn sql_name_ref_at_offset(&self, offset: usize) -> Option<SemSqlNameRefId> {
        self.sql_name_refs
            .iter()
            .enumerate()
            .filter(|(_, sql_ref)| sql_ref.range.start <= offset && offset < sql_ref.range.end)
            .min_by_key(|(_, sql_ref)| sql_ref.range.end.saturating_sub(sql_ref.range.start))
            .map(|(idx, _)| SemSqlNameRefId(idx as u32))
    }
}

fn symbol_kind_key(kind: SymbolKind) -> u8 {
    match kind {
        SymbolKind::BuiltinType => 0,
        SymbolKind::BuiltinRoutine => 1,
        SymbolKind::BuiltinConstant => 2,
        SymbolKind::BuiltinVariable => 3,
        SymbolKind::Variable => 4,
        SymbolKind::Constant => 5,
        SymbolKind::TypeDef => 6,
        SymbolKind::FieldSymbol => 7,
        SymbolKind::Form => 8,
        SymbolKind::Parameter => 9,
        SymbolKind::Class => 10,
        SymbolKind::Interface => 11,
        SymbolKind::Method => 12,
        SymbolKind::Field => 13,
        SymbolKind::Include => 14,
        SymbolKind::Event => 15,
        SymbolKind::Module => 16,
        SymbolKind::Control => 17,
        SymbolKind::Report => 18,
    }
}
