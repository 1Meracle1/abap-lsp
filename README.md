# ABAP Language Server (abap-lsp)

A [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) implementation for **SAP ABAP**, written in [Odin](https://odin-lang.org/). It is aimed at fast, accurate feedback in the editor and at supporting modern workflows (navigation, diagnostics, completions, semantic highlighting) including **AI-assisted development**, where a structured understanding of the program beats plain text.

## Goals

- **Editor experience**: hovers, go-to-definition-style resolution where implemented, semantic tokens, completions, and diagnostics driven by real parsing and symbol tables—not only regex or text heuristics.
- **Throughput**: keep analysis incremental and cache-friendly so edits on large sources stay responsive.
- **Faithful ABAP modeling**: lexer → AST → symbol resolution mirror ABAP constructs so features stay aligned with the language rather than generic “C-like” assumptions.

## Architecture

Data generally flows **up** from source text through the language stack, then **sideways** into LSP handlers that read cached snapshots:

```mermaid
flowchart LR
  subgraph ingest
    SRC[Source text]
    LX[lexer]
    PR[parser]
    AST[ast]
    SRC --> LX --> PR --> AST
  end
  subgraph analysis
    SY[symbols]
    AST --> SY
  end
  subgraph serve
    CA[cache]
    LS[lsp]
    SY --> CA
    CA --> LS
  end
```

**Dependency direction** (who may import whom): lower layers do not depend on higher ones.

```mermaid
flowchart TB
  subgraph entry
    M[abap-lsp main]
  end
  subgraph protocol
    LS[lsp]
    JR[jsonrpc]
  end
  subgraph workspace
    CA[cache]
  end
  subgraph lang["lang — ABAP front-end"]
    PR[parser]
    SY["symbols resolver/types/validate"]
    AS[ast]
    LX[lexer]
  end
  M --> LS
  M --> JR
  LS --> CA
  LS --> JR
  LS --> AS
  LS --> SY
  LS --> LX
  CA --> AS
  CA --> PR
  CA --> SY
  PR --> AS
  PR --> LX
  SY --> AS
  SY --> LX
  AS --> LX
```

- **`jsonrpc`**: JSON-RPC framing and message types; no ABAP knowledge.
- **`lang/lexer`**: tokens, positions, and low-level text utilities used everywhere ranges are needed.
- **`lang/ast`**: syntax tree and node unions; depends only on the lexer package for ranges/tokens.
- **`lang/parser`**: builds `ast.File` from tokens; depends on **lexer + ast** only (not on `symbols`).
- **`lang/symbols`**: symbol tables, types, name resolution, validation; depends on **ast + lexer** (not on `parser`).
- **`cache`**: documents, workspaces, parsing and re-resolution orchestration; bridges **parser + symbols + ast**.
- **`lsp`**: LSP method handlers (hover, completion, diagnostics, semantic tokens, etc.) on top of **cache** and language results.

Editor integration assets (for example VS Code grammar or extension metadata) live under `editors/`.

## Repository layout

| Path | Role |
|------|------|
| `src/abap-lsp/` | Executable entry (transport + server startup). |
| `src/jsonrpc/` | JSON-RPC I/O and message structures. |
| `src/lsp/` | LSP features: requests/notifications, JSON types, positioning helpers. |
| `src/cache/` | Document and workspace cache, publish/refresh pipelines. |
| `src/lang/lexer/` | Tokenizer. |
| `src/lang/ast/` | AST definitions, lookups (`lookup.odin`), construction helpers (`utils.odin`). |
| `src/lang/parser/` | Parser (`parser.odin` and focused modules, e.g. Open SQL, OOP). |
| `src/lang/symbols/` | Symbols, types, resolver, semantic validation. |
| `tests/` | Package tests: `lexer`, `parser`, `symbols`, `cache`, `lsp`, etc. |

## Building

On Windows, from the repo root:

```bat
.\build.bat abap-lsp
```

Debug is the default. For an optimized build:

```bat
.\build.bat release abap-lsp
```

Equivalent Odin invocations (after adjusting paths for your machine):

```text
odin build src\abap-lsp -debug -o:none -out:bin\debug\abap-lsp.exe
odin build src\abap-lsp -o:speed -out:bin\release\abap-lsp.exe
```

## Tests

```bat
.\test.bat
```

Underlying command:

```text
odin test tests/ -all-packages -debug -o:none -out:bin/debug/tests.exe
```

Run **build** and **tests** after non-trivial changes so compilation and behavior stay green.

## References

- [Odin language](https://odin-lang.org/docs/overview/)
- [Odin standard library](https://pkg.odin-lang.org/)
- [ABAP documentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/ABENABAP.html)

## Extending the language model

Adding or changing an AST node touches several places (lookup, symbols, LSP). See `.cursor/rules/abap-lsp-project-rule.mdc` for a maintained checklist and naming conventions.
