package lsp

import "core:encoding/json"

InitializeParams :: struct {
	rootUri:          string,
	workspaceFolders: []WorkspaceFolder,
	// capabilities: ClientCapabilities,
}

WorkspaceFolder :: struct {
	uri:  string,
	name: string,
}

ClientCapabilities :: struct {}

InitializeResult :: struct {
	capabilities: ServerCapabilities,
}

ServerCapabilities :: struct {
	textDocumentSync:         TextDocumentSyncKind,
	completionProvider:       CompletionOptions,
	definitionProvider:       bool,
	hoverProvider:            bool,
	diagnosticProvider:       Maybe(DiagnosticOptions),
	semanticTokensProvider:   Maybe(SemanticTokensOptions),
}

DiagnosticOptions :: struct {
	interFileDependencies: bool,
	workspaceDiagnostics:  bool,
}

TextDocumentSyncKind :: enum int {
	None        = 0,
	Full        = 1,
	Incremental = 2,
}

CompletionOptions :: struct {
	triggerCharacters: []string,
	resolveProvider:   bool,
}

ErrorCodes :: enum int {
	// Defined by JSON-RPC
	ParseError                     = -32700,
	InvalidRequest                 = -32600,
	MethodNotFound                 = -32601,
	InvalidParams                  = -32602,
	InternalError                  = -32603,

	/**
	 * This is the start range of JSON-RPC reserved error codes.
	 * It doesn't denote a real error code. No LSP error codes should
	 * be defined between the start and end range. For backwards
	 * compatibility the `ServerNotInitialized` and the `UnknownErrorCode`
	 * are left in the range.
	 *
	 * @since 3.16.0
	 */
	jsonrpcReservedErrorRangeStart = -32099,

	/**
	 * Error code indicating that a server received a notification or
	 * request before the server received the `initialize` request.
	 */
	ServerNotInitialized           = -32002,
	UnknownErrorCode               = -32001,

	/**
	 * This is the end range of JSON-RPC reserved error codes.
	 * It doesn't denote a real error code.
	 *
	 * @since 3.16.0
	 */
	jsonrpcReservedErrorRangeEnd   = -32000,

	/**
	 * This is the start range of LSP reserved error codes.
	 * It doesn't denote a real error code.
	 *
	 * @since 3.16.0
	 */
	lspReservedErrorRangeStart     = -32899,

	/**
	 * A request failed but it was syntactically correct, e.g the
	 * method name was known and the parameters were valid. The error
	 * message should contain human readable information about why
	 * the request failed.
	 *
	 * @since 3.17.0
	 */
	RequestFailed                  = -32803,

	/**
	 * The server cancelled the request. This error code should
	 * only be used for requests that explicitly support being
	 * server cancellable.
	 *
	 * @since 3.17.0
	 */
	ServerCancelled                = -32802,

	/**
	 * The server detected that the content of a document got
	 * modified outside normal conditions. A server should
	 * NOT send this error code if it detects a content change
	 * in its unprocessed messages. The result even computed
	 * on an older state might still be useful for the client.
	 *
	 * If a client decides that a result is not of any use anymore
	 * the client should cancel the request.
	 */
	ContentModified                = -32801,

	/**
	 * The client has canceled a request and a server has detected
	 * the cancel.
	 */
	RequestCancelled               = -32800,

	/**
	 * This is the end range of LSP reserved error codes.
	 * It doesn't denote a real error code.
	 *
	 * @since 3.16.0
	 */
	lspReservedErrorRangeEnd       = -32800,
}

DidOpenTextDocumentParams :: struct {
	textDocument: TextDocumentItem,
}

TextDocumentItem :: struct {
	uri:        string,
	languageId: string,
	version:    int,
	text:       string,
}

TextDocumentIdentifier :: struct {
	uri: string,
}

Position :: struct {
	line:      int,
	character: int,
}

TextDocumentPositionParams :: struct {
	textDocument: TextDocumentIdentifier,
	position:     Position,
}

WorkDoneProgressParams :: struct {
	workDoneToken: json.Value,
}

HoverParams :: struct {
	textDocument: TextDocumentIdentifier,
	position:     Position,
	// using text_document_position_params: TextDocumentPositionParams,
	// using work_done_progress_params:     WorkDoneProgressParams,
}

Hover :: struct {
	contents: MarkupContent,
	range:    Maybe(Range),
}

MarkupKind :: string
MarkupKind_PlainText :: "plaintext"
MarkupKind_Markdown :: "markdown"

MarkupContent :: struct {
	kind:  MarkupKind,
	value: string,
}

Range :: struct {
	start: Position,
	end:   Position,
}

VersionedTextDocumentIdentifier :: struct {
	uri:     string,
	version: int,
}

TextDocumentContentChangeEvent :: struct {
	range: Maybe(Range),
	text:  string,
}

DidChangeTextDocumentParams :: struct {
	textDocument:   VersionedTextDocumentIdentifier,
	contentChanges: []TextDocumentContentChangeEvent,
}

// Diagnostic severity levels
DiagnosticSeverity :: enum int {
	Error       = 1,
	Warning     = 2,
	Information = 3,
	Hint        = 4,
}

// A diagnostic (error, warning, etc.)
Diagnostic :: struct {
	range:    Range,
	severity: Maybe(DiagnosticSeverity),
	code:     Maybe(string),
	source:   Maybe(string),
	message:  string,
}

// Parameters for publishing diagnostics
PublishDiagnosticsParams :: struct {
	uri:         string,
	version:     Maybe(int),
	diagnostics: []Diagnostic,
}

// Parameters for textDocument/diagnostic request (pull-based)
DocumentDiagnosticParams :: struct {
	textDocument: TextDocumentIdentifier,
}

// Response for textDocument/diagnostic
DocumentDiagnosticReportKind :: string
DocumentDiagnosticReportKind_Full :: "full"
DocumentDiagnosticReportKind_Unchanged :: "unchanged"

FullDocumentDiagnosticReport :: struct {
	kind:     DocumentDiagnosticReportKind,
	resultId: Maybe(string),
	items:    []Diagnostic,
}

// ============================================================================
// Semantic Tokens
// ============================================================================

// Parameters for textDocument/semanticTokens/full request
SemanticTokensParams :: struct {
	textDocument: TextDocumentIdentifier,
}

// Response for textDocument/semanticTokens/full
SemanticTokens :: struct {
	resultId: Maybe(string),
	data:     []u32,
}

// Semantic token types - indices into the legend
SemanticTokenType :: enum u32 {
	Namespace     = 0,
	Type          = 1,
	Class         = 2,
	Enum          = 3,
	Interface     = 4,
	Struct        = 5,
	TypeParameter = 6,
	Parameter     = 7,
	Variable      = 8,
	Property      = 9,
	EnumMember    = 10,
	Event         = 11,
	Function      = 12,
	Method        = 13,
	Macro         = 14,
	Keyword       = 15,
	Modifier      = 16,
	Comment       = 17,
	String        = 18,
	Number        = 19,
	Regexp        = 20,
	Operator      = 21,
}

// Semantic token modifiers - bit flags
SemanticTokenModifier :: enum u32 {
	Declaration    = 0,
	Definition     = 1,
	Readonly       = 2,
	Static         = 3,
	Deprecated     = 4,
	Abstract       = 5,
	Async          = 6,
	Modification   = 7,
	Documentation  = 8,
	DefaultLibrary = 9,
}

// Legend describing available token types and modifiers
SemanticTokensLegend :: struct {
	tokenTypes:     []string,
	tokenModifiers: []string,
}

// Options for semantic tokens provider
SemanticTokensOptions :: struct {
	legend: SemanticTokensLegend,
	range:  bool,
	full:   bool,
}

// ============================================================================
// Completion
// ============================================================================

// Parameters for textDocument/completion request
CompletionParams :: struct {
	textDocument: TextDocumentIdentifier,
	position:     Position,
	context_:     Maybe(CompletionContext),
}

CompletionContext :: struct {
	triggerKind:      CompletionTriggerKind,
	triggerCharacter: Maybe(string),
}

CompletionTriggerKind :: enum int {
	Invoked                         = 1,
	TriggerCharacter                = 2,
	TriggerForIncompleteCompletions = 3,
}

// Completion item kinds
CompletionItemKind :: enum int {
	Text          = 1,
	Method        = 2,
	Function      = 3,
	Constructor   = 4,
	Field         = 5,
	Variable      = 6,
	Class         = 7,
	Interface     = 8,
	Module        = 9,
	Property      = 10,
	Unit          = 11,
	Value         = 12,
	Enum          = 13,
	Keyword       = 14,
	Snippet       = 15,
	Color         = 16,
	File          = 17,
	Reference     = 18,
	Folder        = 19,
	EnumMember    = 20,
	Constant      = 21,
	Struct        = 22,
	Event         = 23,
	Operator      = 24,
	TypeParameter = 25,
}

// A completion item
CompletionItem :: struct {
	label:         string,
	kind:          Maybe(CompletionItemKind),
	detail:        Maybe(string),
	documentation: Maybe(MarkupContent),
	insertText:    Maybe(string),
	insertTextFormat: Maybe(InsertTextFormat),
}

InsertTextFormat :: enum int {
	PlainText = 1,
	Snippet   = 2,
}

// Completion response - can be an array of items or a CompletionList
CompletionList :: struct {
	isIncomplete: bool,
	items:        []CompletionItem,
}