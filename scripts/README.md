# Verification scripts

This directory contains small local verification helpers for behavior that is
awkward to reproduce with unit tests alone.

## ABAP LSP completion end-to-end probe

Run:

```powershell
python .\scripts\verify_lsp_completion_e2e.py
```

The script builds the debug binaries with `build.bat`, starts
`bin\debug\abap_language_server.exe` over stdio, opens an in-memory ABAP
document containing an incomplete `rv_` statement inside a method
implementation, sends `textDocument/completion`, and exits non-zero unless the
server returns a completion item labeled `rv_res`.

Use `--skip-build` to reuse an existing binary:

```powershell
python .\scripts\verify_lsp_completion_e2e.py --skip-build
```

This verifies the language-server protocol response used by editors such as
Zed. It does not automate the Zed UI popup itself.
