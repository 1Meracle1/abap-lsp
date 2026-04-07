use abap_ast::{SyntaxKind, arena::NodeId};

use super::Collector;
use super::context::CollectorContext;
use crate::def_map::SymbolKind;
use crate::ids::ScopeId;

pub(super) fn walk_root(ctx: &mut CollectorContext<'_, '_>, node: NodeId, scope: ScopeId) {
    ctx.collector.walk_children(node, scope);
}

impl<'a> Collector<'a> {
    pub(super) fn walk_children(&mut self, node: NodeId, scope: ScopeId) {
        for child in self.file.children(node) {
            self.walk_node(child, scope);
        }
    }

    pub(super) fn walk_node(&mut self, node: NodeId, scope: ScopeId) {
        match self.file.kind(node) {
            SyntaxKind::Token => {}
            SyntaxKind::Error => {
                let tokens = self.syntax_token_nodes(node);
                if !tokens.is_empty() {
                    self.collect_token_expression_refs_infos(&tokens, scope, true);
                }
            }
            SyntaxKind::DataDecl | SyntaxKind::StaticsDecl => self
                .decl_lowering()
                .walk_data_like_decl(node, scope, SymbolKind::Variable),
            SyntaxKind::TypesDecl => {
                self.decl_lowering()
                    .walk_data_like_decl(node, scope, SymbolKind::TypeDef)
            }
            SyntaxKind::ConstantsDecl => {
                self.decl_lowering()
                    .walk_data_like_decl(node, scope, SymbolKind::Constant)
            }
            SyntaxKind::FieldSymbolsDecl => {
                self.decl_lowering()
                    .walk_data_like_decl(node, scope, SymbolKind::FieldSymbol)
            }
            SyntaxKind::DataInlineDecl => self.decl_lowering().walk_inline_decl(node, scope),
            SyntaxKind::IncludeStmt => self.decl_lowering().walk_include_stmt(node, scope),
            SyntaxKind::ReportStmt => self.decl_lowering().walk_named_header_decl(
                node,
                scope,
                SymbolKind::Report,
                crate::ScopeKind::File,
            ),
            SyntaxKind::FormDecl => self.decl_lowering().walk_block_decl(
                node,
                scope,
                SymbolKind::Form,
                crate::ScopeKind::Form,
            ),
            SyntaxKind::ModuleDecl => self.decl_lowering().walk_block_decl(
                node,
                scope,
                SymbolKind::Module,
                crate::ScopeKind::Module,
            ),
            SyntaxKind::EventBlock => self.decl_lowering().walk_event_block(node, scope),
            SyntaxKind::ClassDecl => self.class_lowering().walk_class_decl(node, scope),
            SyntaxKind::InterfaceDecl => self.decl_lowering().walk_block_decl(
                node,
                scope,
                SymbolKind::Interface,
                crate::ScopeKind::Interface,
            ),
            SyntaxKind::MethodDecl => self.decl_lowering().walk_method_decl(node, scope),
            SyntaxKind::IfStmt => self.control_lowering().walk_if_stmt(node, scope),
            SyntaxKind::ElseifClause => {
                self.control_lowering().walk_nested_block(
                    node,
                    scope,
                    crate::ScopeKind::ElseifBranch,
                );
            }
            SyntaxKind::ElseClause => {
                self.control_lowering().walk_nested_block(
                    node,
                    scope,
                    crate::ScopeKind::ElseBranch,
                );
            }
            SyntaxKind::WhenClause => {
                self.control_lowering().walk_nested_block(
                    node,
                    scope,
                    crate::ScopeKind::WhenBranch,
                );
            }
            SyntaxKind::WhileStmt => {
                self.control_lowering()
                    .walk_nested_block(node, scope, crate::ScopeKind::WhileBlock)
            }
            SyntaxKind::DoStmt => {
                self.control_lowering()
                    .walk_nested_block(node, scope, crate::ScopeKind::DoBlock)
            }
            SyntaxKind::LoopStmt => self.control_lowering().walk_loop_stmt(node, scope),
            SyntaxKind::TryStmt => {
                self.control_lowering()
                    .walk_nested_block(node, scope, crate::ScopeKind::TryBlock)
            }
            SyntaxKind::CatchClause => self.control_lowering().walk_catch_clause(node, scope),
            SyntaxKind::CleanupClause => self.control_lowering().walk_nested_block(
                node,
                scope,
                crate::ScopeKind::CleanupClause,
            ),
            SyntaxKind::SelectStmt => self.sql_lowering().collect_select_stmt(node, scope),
            SyntaxKind::AppendStmt
            | SyntaxKind::InsertTableStmt
            | SyntaxKind::MoveCorrespondingStmt
            | SyntaxKind::MoveStmt
            | SyntaxKind::ModifyStmt
            | SyntaxKind::DeleteDbTableStmt
            | SyntaxKind::ReadTableStmt
            | SyntaxKind::GetBitStmt
            | SyntaxKind::SetBitStmt => self.walk_children(node, scope),
            SyntaxKind::DeleteStmt => self.stmt_lowering().collect_delete_stmt(node, scope),
            SyntaxKind::SortStmt => self.control_lowering().collect_sort_stmt(node, scope),
            SyntaxKind::TypeRefSimple => self.decl_lowering().collect_type_ref(node, scope),
            SyntaxKind::ExprIdent
            | SyntaxKind::SelectorExpr
            | SyntaxKind::CallExpr
            | SyntaxKind::BinaryExpr
            | SyntaxKind::UnaryExpr
            | SyntaxKind::ParenExpr
            | SyntaxKind::ConstructorExpr
            | SyntaxKind::TemplateExpr
            | SyntaxKind::TemplateInterpolation
            | SyntaxKind::TemplateFormatSpec
            | SyntaxKind::CharStringTemplate
            | SyntaxKind::IsPredicate
            | SyntaxKind::InstanceOfPredicate
            | SyntaxKind::BetweenExpr
            | SyntaxKind::AssignStmt => self.expr_lowering().collect_expr(node, scope),
            SyntaxKind::AssignSourceExpr | SyntaxKind::CallMethodTarget => {
                self.walk_children(node, scope)
            }
            SyntaxKind::AssignKeywordStmt => self
                .stmt_lowering()
                .collect_assign_keyword_stmt(node, scope),
            SyntaxKind::FieldSymbolInlineDecl => self
                .decl_lowering()
                .walk_inline_field_symbol_decl(node, scope),
            SyntaxKind::GetTimeStampStmt => self
                .stmt_lowering()
                .collect_get_time_stamp_stmt(node, scope),
            SyntaxKind::FindStmt => self.stmt_lowering().collect_find_stmt(node, scope),
            SyntaxKind::CallStmt => self.stmt_lowering().collect_call_stmt(node, scope),
            SyntaxKind::MessageStmt => self.stmt_lowering().collect_message_stmt(node, scope),
            SyntaxKind::UnparsedStmt
            | SyntaxKind::CommitWorkStmt
            | SyntaxKind::RollbackWorkStmt
            | SyntaxKind::RaiseStmt
            | SyntaxKind::EndAtStmt => self
                .stmt_lowering()
                .collect_generic_simple_stmt(node, scope),
            SyntaxKind::MethodsStmt => self.stmt_lowering().collect_methods_stmt(node, scope),
            SyntaxKind::AssertStmt | SyntaxKind::CheckStmt => self
                .stmt_lowering()
                .collect_assert_or_check_stmt(node, scope),
            SyntaxKind::PerformStmt => self.forms_lowering().collect_perform_stmt_node(node, scope),
            SyntaxKind::CreateObjectStmt => self
                .stmt_lowering()
                .collect_create_object_stmt_node(node, scope),
            SyntaxKind::CreateDataStmt => self
                .stmt_lowering()
                .collect_create_data_stmt_node(node, scope),
            SyntaxKind::CallMethodStmt => self
                .stmt_lowering()
                .collect_call_method_stmt_node(node, scope),
            SyntaxKind::WriteStmt => self.stmt_lowering().collect_write_stmt(node, scope),
            SyntaxKind::ConcatenateStmt => {
                self.stmt_lowering().collect_concatenate_stmt(node, scope)
            }
            SyntaxKind::StructuredFieldClause => {
                let hint = self.typed_clause_namespace_hint(node);
                if let Some(ns) = hint {
                    self.type_clause_ns_stack.push(ns);
                }
                self.walk_children(node, scope);
                if hint.is_some() {
                    self.type_clause_ns_stack.pop();
                }
            }
            _ => self.walk_children(node, scope),
        }
    }
}
