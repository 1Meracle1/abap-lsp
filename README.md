Build Language Server:
```
odin build src/abap-lsp/ -debug -out:bin/debug/abap-lsp.exe -o:none

odin build src/abap-lsp/ -out:bin/release/abap-lsp.exe -o:speed
```

Run tests:
```
odin test tests/ -all-packages -debug -o:none -out:bin/debug/tests.exe
```