# Verification scripts

This directory contains small local verification helpers for behavior that is
awkward to reproduce with unit tests alone.

## ABAP LSP completion end-to-end probe

Run:

```powershell
python .\scripts\verify_lsp_completion_e2e.py
```

The script builds the debug binaries with `build.bat`, starts
`bin\debug\abap_language_server.exe` over stdio, opens in-memory ABAP
documents, sends `textDocument/completion`, and exits non-zero unless the
server returns the expected completion item. The default `all` case verifies
both an incomplete `rv_` statement returning `rv_res` and an incomplete `me->`
selector returning `method_name`.

Use `--skip-build` to reuse an existing binary:

```powershell
python .\scripts\verify_lsp_completion_e2e.py --skip-build
```

Run a single case with `--case`:

```powershell
python .\scripts\verify_lsp_completion_e2e.py --case me-selector --skip-build
```

This verifies the language-server protocol response used by editors such as
Zed. It also checks the repository Zed ABAP language config against Zed's
generic completion filtering behavior so selector completions returned by the
server are not hidden by client-side query filtering. It does not automate the
Zed UI popup itself.
