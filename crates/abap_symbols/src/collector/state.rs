use std::collections::HashMap;
use std::sync::Arc;

use crate::builtins::{BUILTIN_STRUCTURES, BUILTIN_SYMBOLS, BuiltinTypeKind};
use crate::def_map::{
    Diagnostic, DiagnosticKind, FieldTypeRefData, ReferenceData, ReferenceKind, StructureData,
    StructureFieldData, SymbolData, SymbolKind,
};
use crate::ids::{ReferenceId, ScopeId, StructureId, SymbolId};
use crate::scope::{Namespace, ScopeData, ScopeKind};

use super::{Collector, PendingStructure, PendingStructureMember, ScopeLookupKey};
use abap_lexer::TextRange;

impl<'a> Collector<'a> {
    pub(super) fn push_scope(
        &mut self,
        kind: ScopeKind,
        range: TextRange,
        parent: Option<ScopeId>,
        owner: Option<SymbolId>,
    ) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(ScopeData {
            id,
            kind,
            range,
            parent,
            owner,
            declarations: Vec::new(),
            children: Vec::new(),
            allows_internal_table_line_selector: false,
        });
        self.scope_symbols.push(HashMap::new());
        if let Some(parent_id) = parent {
            self.scopes[parent_id.as_usize()].children.push(id);
        }
        id
    }

    pub(super) fn declare_symbol(
        &mut self,
        scope: ScopeId,
        name: Arc<str>,
        kind: SymbolKind,
        decl_range: TextRange,
        structure: Option<StructureId>,
        declared_type: Option<FieldTypeRefData>,
        type_clause_display: Option<Arc<str>>,
        value_clause_display: Option<Arc<str>>,
    ) -> SymbolId {
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(SymbolData {
            id,
            name: Arc::clone(&name),
            kind,
            scope,
            decl_range: decl_range.clone(),
            structure,
            declared_type,
            type_clause_display,
            value_clause_display,
        });
        self.scopes[scope.as_usize()].declarations.push(id);
        for &namespace in kind.namespaces() {
            let key = ScopeLookupKey {
                namespace,
                name: Arc::clone(&name),
            };
            if let Some(existing) = self.scope_symbols[scope.as_usize()].get(&key)
                && !existing.is_empty()
                && !kind.is_builtin()
                && existing
                    .iter()
                    .any(|existing_id| !self.symbols[existing_id.as_usize()].kind.is_builtin())
            {
                self.diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::DuplicateDeclaration,
                    range: decl_range.clone(),
                    message: format!("duplicate declaration of '{}'", name),
                });
            } else if !kind.is_builtin()
                && self
                    .find_ancestor_symbol(scope, namespace, name.as_ref())
                    .is_some_and(|symbol_id| !self.symbol(symbol_id).kind.is_builtin())
            {
                self.diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::ShadowedSymbol,
                    range: decl_range.clone(),
                    message: format!("'{}' shadows an outer declaration", name),
                });
            }
            self.scope_symbols[scope.as_usize()]
                .entry(key)
                .or_default()
                .push(id);
        }
        id
    }

    pub(super) fn declare_plain_symbol(
        &mut self,
        scope: ScopeId,
        name: Arc<str>,
        kind: SymbolKind,
        decl_range: TextRange,
    ) -> SymbolId {
        self.declare_symbol(scope, name, kind, decl_range, None, None, None, None)
    }

    pub(super) fn push_structure(
        &mut self,
        name: Arc<str>,
        fields: impl IntoIterator<Item = StructureFieldData>,
    ) -> StructureId {
        let id = StructureId(self.structures.len() as u32);
        self.structures.push(StructureData {
            id,
            origin_unit: self.unit_id,
            origin_structure: id,
            name,
            fields: fields.into_iter().collect(),
        });
        id
    }

    pub(super) fn register_structure(
        &mut self,
        scope: ScopeId,
        structure: PendingStructure,
    ) -> StructureId {
        let mut fields = Vec::new();
        for member in structure.members {
            match member {
                PendingStructureMember::Field(field) => {
                    fields.push(StructureFieldData {
                        name: field.name,
                        decl_range: Some(field.decl_range),
                        decl_unit: self.unit_id,
                        structure: field
                            .structure
                            .map(|nested| self.register_structure(scope, nested))
                            .or_else(|| {
                                field.type_ref.as_ref().and_then(|type_ref| {
                                    self.resolve_field_type_ref(scope, type_ref)
                                })
                            }),
                        type_ref: field.type_ref,
                        value_clause_display: field.value_clause_display,
                    });
                }
                PendingStructureMember::Include { type_ref } => {
                    if let Some(structure_id) = self.resolve_field_type_ref(scope, &type_ref)
                        && let Some(included) = self.structure(structure_id)
                    {
                        fields.extend(included.fields.iter().cloned());
                    }
                }
            }
        }
        self.push_structure(structure.name, fields)
    }

    pub(super) fn install_builtin_symbols(&mut self, root_scope: ScopeId) {
        let mut structure_ids = HashMap::new();
        let unit_id = self.unit_id;
        for structure in BUILTIN_STRUCTURES {
            let id = self.push_structure(
                Arc::<str>::from(structure.name),
                structure.fields.iter().map(|field| StructureFieldData {
                    name: Arc::<str>::from(field.name),
                    decl_range: None,
                    decl_unit: unit_id,
                    structure: None,
                    type_ref: None,
                    value_clause_display: None,
                }),
            );
            structure_ids.insert(structure.name, id);
        }

        for symbol in BUILTIN_SYMBOLS {
            let kind = match symbol.kind {
                BuiltinTypeKind::Type => SymbolKind::BuiltinType,
                BuiltinTypeKind::Constant => SymbolKind::BuiltinConstant,
                BuiltinTypeKind::Variable => SymbolKind::BuiltinVariable,
            };
            self.declare_symbol(
                root_scope,
                Arc::<str>::from(symbol.name),
                kind,
                0..0,
                symbol
                    .structure_name
                    .and_then(|name| structure_ids.get(name).copied()),
                None,
                None,
                None,
            );
        }
    }

    pub(super) fn add_reference(
        &mut self,
        scope: ScopeId,
        name: Arc<str>,
        namespace: Namespace,
        kind: ReferenceKind,
        range: TextRange,
    ) {
        let id = ReferenceId(self.references.len() as u32);
        self.references.push(ReferenceData {
            id,
            name,
            namespace,
            kind,
            scope,
            range,
            resolution: None,
        });
    }
}
