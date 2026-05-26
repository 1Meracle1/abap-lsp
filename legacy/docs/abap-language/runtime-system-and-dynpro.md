# Runtime, System, And Dynpro Statements

ABAP source interacts with the runtime through messages, authorization checks,
memory IDs, dataset files, transactions, screens, update tasks, generated
source, and system fields.

`abap-lsp` support: `MESSAGE`, `AUTHORITY-CHECK`, `CALL FUNCTION`, `CALL
METHOD`, `CALL TRANSACTION`, `CALL SCREEN`, `CALL TRANSFORMATION`, `CALL BADI`,
`CALL CUSTOMER-FUNCTION`, `CREATE OBJECT`, `CREATE DATA`, `GET/SET PARAMETER`,
`GET TIME`, `GET TIME STAMP`, `GET/SET BIT`, `GET/SET CURSOR`, `GET/SET
PF-STATUS`, `SET TITLEBAR`, `SET SCREEN`, `SET HANDLER`, dataset statements,
memory `IMPORT`/`EXPORT`, `READ/INSERT TEXTPOOL`, `READ/INSERT/DELETE REPORT`,
`SYNTAX-CHECK`, `GENERATE`, `WAIT`, `COMMIT WORK`, and `ROLLBACK WORK` are
recognized in common forms.

## Messages

```abap
" Compact form: message number and class are combined in s001(zdemo).
MESSAGE s001(zdemo) WITH lv_order_id.

" Dynamic text form. The TYPE controls runtime behavior.
MESSAGE lv_text TYPE 'E'.

" INTO formats the message text into a variable instead of displaying it.
MESSAGE s001(zdemo) WITH lv_order_id INTO DATA(lv_message_text).
```

Semantics:

- message type `E` usually raises an error in dialog/report contexts,
- message type `A` aborts processing,
- message type `S`, `I`, and `W` have context-dependent display behavior,
- `WITH` operands fill placeholders in the message text,
- `DISPLAY LIKE` changes display severity without necessarily changing control
  flow.

## Authorization Checks

```abap
AUTHORITY-CHECK OBJECT 'Z_ORDER'
  ID 'ACTVT' FIELD '03'
  ID 'BUKRS' FIELD lv_bukrs.

IF sy-subrc <> 0.
  MESSAGE 'Not authorized for this company code' TYPE 'E'.
ENDIF.
```

Semantics:

- `AUTHORITY-CHECK` asks the runtime authorization system whether the current
  user has matching authorization values,
- `sy-subrc = 0` means authorized,
- nonzero `sy-subrc` values distinguish failure reasons but are usually handled
  as denial,
- always check the result before performing the protected action.

## Function, Method, And Transaction Calls

```abap
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING
    wait = abap_true.

CALL METHOD lo_service->run
  EXPORTING
    iv_order_id = lv_order_id
  IMPORTING
    ev_status   = DATA(lv_status).

" CALL TRANSACTION runs a transaction code. Batch-input additions can provide
" screens and options.
CALL TRANSACTION lv_tcode WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
```

Semantics:

- `CALL FUNCTION` calls a function module by name and may cross RFC/update-task
  boundaries depending on additions,
- `CALL METHOD` is the classic method-call statement form,
- expression-style calls such as `lo_service->run( )` are preferred in modern
  code when no classic additions are needed,
- `CALL TRANSACTION` changes runtime context and should be guarded carefully.

## Runtime Values And Parameters

```abap
" SPA/GPA parameters are user/session memory slots addressed by an ID.
SET PARAMETER ID 'BUK' FIELD lv_bukrs.
GET PARAMETER ID 'BUK' FIELD DATA(lv_saved_bukrs).

" GET TIME updates system time fields; FIELD writes the current time.
GET TIME FIELD DATA(lv_time).

" Timestamp form writes a UTC timestamp-like value to the target.
GET TIME STAMP FIELD DATA(lv_timestamp).

" WAIT pauses the current work process. Use sparingly in application logic.
WAIT UP TO 1 SECONDS.
```

Semantics:

- parameter IDs are shared runtime state and can surprise tests,
- time statements read runtime clock values,
- `WAIT` can trigger commits in some contexts and should not be used as a
  substitute for proper synchronization.

## Memory Clusters And Data Buffers

```abap
" EXPORT writes named values to ABAP memory under an ID.
EXPORT order_id = lv_order_id
  TO MEMORY ID 'ZDEMO_ORDER'.

" IMPORT reads them back by matching exported names.
IMPORT order_id = DATA(lv_imported_order)
  FROM MEMORY ID 'ZDEMO_ORDER'.

FREE MEMORY ID 'ZDEMO_ORDER'.
```

Semantics:

- ABAP memory is internal-session scoped,
- names in `EXPORT`/`IMPORT` are part of the cluster contract,
- database clusters and data buffers are older persistence/serialization forms
  and should be wrapped behind small routines when encountered.

## Dataset Files

```abap
OPEN DATASET lv_file IN TEXT MODE FOR OUTPUT ENCODING UTF-8.
IF sy-subrc <> 0.
  MESSAGE 'Could not open output file' TYPE 'E'.
ENDIF.

TRANSFER lv_line TO lv_file.

CLOSE DATASET lv_file.
```

Semantics:

- datasets are application-server files, not frontend files,
- always check `sy-subrc` after opening or reading,
- `TEXT MODE` and `BINARY MODE` determine conversion behavior,
- `ENCODING` matters for portable text processing.

## Source And Dynpro Maintenance

```abap
" These statements maintain generated source-like repository objects.
" They are powerful and should be isolated from business logic.
READ REPORT lv_program INTO DATA(lt_source).
SYNTAX-CHECK FOR lt_source MESSAGE DATA(lv_msg) LINE DATA(lv_line) WORD DATA(lv_word).

" Dynpro statements interact with classic screens.
SET SCREEN 100.
SET CURSOR FIELD 'P_ORDER_ID'.
GET CURSOR FIELD DATA(lv_field) LINE DATA(lv_line_no) OFFSET DATA(lv_offset).
```

Semantics:

- source maintenance statements operate on ABAP source text and syntax checks,
- generated subroutine pools and dynpros create runtime artifacts from source
  or screen definitions,
- dynpro cursor and screen statements affect classic SAP GUI screen processing.

