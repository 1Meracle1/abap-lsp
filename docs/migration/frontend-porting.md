# Front-End Porting Strategy

## Port Order

1. `abap_jsonrpc`
2. `abap_lexer`
3. `abap_ast`
4. `abap_parser`
5. `abap_symbols`
6. `abap_cache`
7. `abap_lsp`
8. `abap_lsp_server`

This matches the legacy layering and keeps syntax-only code separate from semantic and protocol-facing logic.

## Parser Migration Rules

- Keep parser code syntax-only. Name resolution and validation stay out of `abap_parser`.
- Port Odin parser tests from `legacy/tests/parser/` in focused batches rather than a giant one-shot move.
- Prefer golden fixtures and narrow behavior assertions over restating implementation details.
- Keep AST shape decisions explicit and versioned. Churn here multiplies into symbols, cache, and LSP layers.

## Initial Rust Scaffolding Goals

- `abap_lexer` owns ranges and tokenization primitives.
- `abap_ast` owns a stable syntax tree surface that later semantic layers can consume.
- `abap_parser` produces a file node, raw token stream, and parse diagnostics/errors.

## Test Porting Sequence

1. High-signal lexer tests.
2. Broad parser smoke cases.
3. Focused parser topic suites such as declarations, control flow, OOP, and SQL.
4. Semantic suites only after parser output is stable.

## Performance Expectations

- Optimize parser data layout before introducing specialized allocators.
- Use profiling to justify bump allocation, token interning, or green-tree style sharing.
- Preserve deterministic output so parser tests remain trustworthy under parallel execution later.
