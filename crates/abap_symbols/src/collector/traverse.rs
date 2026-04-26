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
            SyntaxKind::SqlScriptIsland => {}
            SyntaxKind::Error => {
                let tokens = self.syntax_token_nodes(node);
                if !tokens.is_empty() {
                    self.collect_token_expression_refs_infos(&tokens, scope, true);
                }
            }
            SyntaxKind::DataDecl
            | SyntaxKind::TablesDecl
            | SyntaxKind::RangesDecl
            | SyntaxKind::ControlsDecl
            | SyntaxKind::ParametersDecl
            | SyntaxKind::SelectOptionsDecl
            | SyntaxKind::StaticsDecl => {
                self.decl_lowering()
                    .walk_data_like_decl(node, scope, SymbolKind::Variable)
            }
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
            SyntaxKind::ValueClause => {
                let tokens = self.syntax_token_nodes(node);
                if tokens.len() > 1 {
                    self.collect_token_expression_refs_infos(&tokens[1..], scope, true);
                }
            }
            SyntaxKind::IncludeStmt => self.decl_lowering().walk_include_stmt(node, scope),
            SyntaxKind::ReportStmt => self.decl_lowering().walk_report_decl(node, scope),
            SyntaxKind::FormDecl => self.decl_lowering().walk_block_decl(
                node,
                scope,
                SymbolKind::Form,
                crate::ScopeKind::Form,
            ),
            SyntaxKind::FunctionDecl => self.decl_lowering().walk_block_decl(
                node,
                scope,
                SymbolKind::Module,
                crate::ScopeKind::Module,
            ),
            SyntaxKind::ModuleDecl => self.decl_lowering().walk_block_decl(
                node,
                scope,
                SymbolKind::Module,
                crate::ScopeKind::Module,
            ),
            SyntaxKind::EventBlock => self.decl_lowering().walk_event_block(node, scope),
            SyntaxKind::ClassDecl => self.class_lowering().walk_class_decl(node, scope),
            SyntaxKind::ClassDeferredStmt => {
                self.class_lowering().walk_class_deferred_stmt(node, scope)
            }
            SyntaxKind::InterfaceDeferredStmt => self
                .class_lowering()
                .walk_interface_deferred_stmt(node, scope),
            SyntaxKind::InterfaceDecl => self.class_lowering().walk_interface_decl(node, scope),
            SyntaxKind::MethodDecl => self.decl_lowering().walk_method_decl(node, scope),
            SyntaxKind::IfStmt => self.control_lowering().walk_if_stmt(node, scope),
            SyntaxKind::CaseStmt => self.control_lowering().walk_case_stmt(node, scope),
            SyntaxKind::ElseifClause => {
                let _ = self.control_lowering().walk_nested_block(
                    node,
                    scope,
                    crate::ScopeKind::ElseifBranch,
                );
            }
            SyntaxKind::ElseClause => {
                let _ = self.control_lowering().walk_nested_block(
                    node,
                    scope,
                    crate::ScopeKind::ElseBranch,
                );
            }
            SyntaxKind::WhenClause => {
                let _ = self.control_lowering().walk_when_clause(node, scope);
            }
            SyntaxKind::WhileStmt => self.control_lowering().walk_while_stmt(node, scope),
            SyntaxKind::DoStmt => self.control_lowering().walk_do_stmt(node, scope),
            SyntaxKind::LoopStmt => self.control_lowering().walk_loop_stmt(node, scope),
            SyntaxKind::AtStmt => self.control_lowering().walk_at_stmt(node, scope),
            SyntaxKind::TryStmt => self.control_lowering().walk_try_stmt(node, scope),
            SyntaxKind::CatchClause => {
                let _ = self.control_lowering().walk_catch_clause(node, scope);
            }
            SyntaxKind::CleanupClause => {
                let _ = self.control_lowering().walk_nested_block(
                    node,
                    scope,
                    crate::ScopeKind::CleanupClause,
                );
            }
            SyntaxKind::SelectStmt => self.sql_lowering().collect_select_stmt(node, scope),
            SyntaxKind::AppendStmt => self.stmt_lowering().collect_append_stmt(node, scope),
            SyntaxKind::InsertTableStmt => {
                self.stmt_lowering().collect_insert_table_stmt(node, scope)
            }
            SyntaxKind::InsertTextpoolStmt => self
                .stmt_lowering()
                .collect_insert_textpool_stmt(node, scope),
            SyntaxKind::MoveStmt | SyntaxKind::MoveCorrespondingStmt => {
                self.stmt_lowering().collect_move_stmt(node, scope)
            }
            SyntaxKind::UpdateTarget
            | SyntaxKind::GetReferenceStmt
            | SyntaxKind::GetBitStmt
            | SyntaxKind::SetBitStmt => self.walk_children(node, scope),
            SyntaxKind::ModifyStmt => self.stmt_lowering().collect_modify_stmt(node, scope),
            SyntaxKind::UpdateStmt => self.stmt_lowering().collect_update_stmt(node, scope),
            SyntaxKind::ReadTableStmt => self.stmt_lowering().collect_read_table_stmt(node, scope),
            SyntaxKind::AuthorityCheckStmt => self
                .stmt_lowering()
                .collect_authority_check_stmt(node, scope),
            SyntaxKind::InsertDbTableStmt => self
                .sql_lowering()
                .collect_insert_db_table_stmt(node, scope),
            SyntaxKind::DeleteDbTableStmt => self
                .sql_lowering()
                .collect_delete_db_table_stmt(node, scope),
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
            | SyntaxKind::LetExpr
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
            SyntaxKind::AliasesStmt => self.stmt_lowering().collect_aliases_stmt(node, scope),
            SyntaxKind::ClearStmt => self.stmt_lowering().collect_clear_stmt(node, scope),
            SyntaxKind::RefreshStmt => self.stmt_lowering().collect_refresh_stmt(node, scope),
            SyntaxKind::CollectStmt
            | SyntaxKind::FreeStmt
            | SyntaxKind::UnassignStmt
            | SyntaxKind::ImportMemoryStmt
            | SyntaxKind::ExportMemoryStmt => self
                .stmt_lowering()
                .collect_structured_effect_stmt(node, scope),
            SyntaxKind::ConvertStmt => self.stmt_lowering().collect_convert_stmt(node, scope),
            SyntaxKind::DescribeStmt => self.stmt_lowering().collect_describe_stmt(node, scope),
            SyntaxKind::FindStmt => self.stmt_lowering().collect_find_stmt(node, scope),
            SyntaxKind::SubmitStmt => self.stmt_lowering().collect_submit_stmt(node, scope),
            SyntaxKind::CallStmt => self.stmt_lowering().collect_call_stmt(node, scope),
            SyntaxKind::MessageStmt => self.stmt_lowering().collect_message_stmt(node, scope),
            SyntaxKind::ReplaceStmt => self.stmt_lowering().collect_replace_stmt(node, scope),
            SyntaxKind::CloseCursorStmt => {
                self.stmt_lowering().collect_close_cursor_stmt(node, scope)
            }
            SyntaxKind::SelectionScreenStmt => self
                .stmt_lowering()
                .collect_selection_screen_stmt(node, scope),
            SyntaxKind::UnparsedStmt
            | SyntaxKind::FunctionPoolStmt
            | SyntaxKind::ClassLoadStmt
            | SyntaxKind::ContinueStmt
            | SyntaxKind::ExitStmt
            | SyntaxKind::ReturnStmt
            | SyntaxKind::StopStmt
            | SyntaxKind::SetPfStatusStmt
            | SyntaxKind::SetTitlebarStmt
            | SyntaxKind::CommitWorkStmt
            | SyntaxKind::RollbackWorkStmt
            | SyntaxKind::EndAtStmt => self
                .stmt_lowering()
                .collect_generic_simple_stmt(node, scope),
            SyntaxKind::RaiseStmt => self.stmt_lowering().collect_raise_stmt(node, scope),
            SyntaxKind::RaiseEventStmt => {
                self.stmt_lowering().collect_raise_event_stmt(node, scope)
            }
            SyntaxKind::LeaveStmt => self.stmt_lowering().collect_leave_stmt(node, scope),
            SyntaxKind::TypePoolsStmt => {}
            SyntaxKind::MethodsStmt => self.stmt_lowering().collect_methods_stmt(node, scope),
            SyntaxKind::EventsStmt => self.stmt_lowering().collect_events_stmt(node, scope),
            SyntaxKind::InterfacesStmt => self.stmt_lowering().collect_interfaces_stmt(node, scope),
            SyntaxKind::AssertStmt | SyntaxKind::CheckStmt => self
                .stmt_lowering()
                .collect_assert_or_check_stmt(node, scope),
            SyntaxKind::PerformStmt => self.forms_lowering().collect_perform_stmt_node(node, scope),
            SyntaxKind::WaitStmt => self.stmt_lowering().collect_wait_stmt(node, scope),
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
            SyntaxKind::SplitStmt => self.stmt_lowering().collect_split_stmt(node, scope),
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
            SyntaxKind::StructuredIncludeClause => {
                let hint = self.structured_include_namespace_hint(node);
                if let Some(ns) = hint {
                    self.type_clause_ns_stack.push(ns);
                }
                self.walk_children(node, scope);
                if hint.is_some() {
                    self.type_clause_ns_stack.pop();
                }
            }
            SyntaxKind::StructuredDecl => {
                self.check_structured_decl_end_name(node);
                self.walk_children(node, scope);
            }
            _ => self.walk_children(node, scope),
        }
    }
}
