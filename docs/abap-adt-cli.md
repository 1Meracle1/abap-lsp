# `abap-adt` CLI

`abap-adt` is a blocking Rust CLI for querying a remote SAP system through the ABAP Development Tools (ADT) HTTP API.

It is intended both for humans and for automation. By default it prints JSON so shells, scripts, and AI agents can consume results directly.

## What It Can Query

- Repository search results.
- ABAP source for reports, includes, classes, function groups, function modules, and interfaces.
- DDIC metadata for data elements, table types, structures, views, and tables.
- Child objects below packages, reports, and function groups.

## Connection Configuration

`abap-adt` accepts connection details from three sources, in this order:

1. CLI flags.
2. Existing environment variables.
3. `.env` loaded automatically from the git repo root.

Supported variable names:

```env
ABAP_ADT_URL=https://sap.example.com/sap/bc/adt
ABAP_ADT_USER=YOUR_SAP_USERNAME
ABAP_ADT_PASSWORD=YOUR_SAP_PASSWORD
ABAP_ADT_CLIENT=100
```

Accepted aliases:

- `ABAP_ADT_BASE_URL` or `SAPBASE_URL`
- `ABAP_ADT_USERNAME` or `SAPUSER`
- `SAPPASS`
- `SAPCLIENT`

If the configured URL does not already end in `/sap/bc/adt`, the CLI appends that suffix automatically.

For CSRF session bootstrap, the CLI advertises both `application/atom+xml;type=feed`
and `application/xml` on `/runtime/systemmessages`. This keeps ADT lookups working
across SAP releases that expose that endpoint with different feed representations.

## Command Shapes

```text
abap-adt [connection options] search <query> [--max-results N]
abap-adt [connection options] get source <kind> <name> [--group <function-group>] [--raw]
abap-adt [connection options] get ddic <kind> <name> [--raw]
abap-adt [connection options] children <kind> <name>
```

Connection options:

```text
--url <URL>
--user <USER>
--password <PASSWORD>
--sap-client <CLIENT>
```

Source kinds:

```text
report, include, class, function-group, function-module, interface
```

DDIC kinds:

```text
data-element, table-type, structure, view, table
```

Children kinds:

```text
package, report, function-group
```

## Output Behavior

- Default output is JSON.
- `get source` returns the fetched source inside the JSON payload.
- `get ddic` returns the fetched XML inside the JSON payload.
- `--raw` prints only the source or XML body for `get source` and `get ddic`.

## Examples

Search the repository:

```powershell
cargo run -p abap_adt_cli -- search "MARA"
```

Fetch a class source:

```powershell
cargo run -p abap_adt_cli -- get source class ZCL_DEMO
```

Fetch a function module directly:

```powershell
cargo run -p abap_adt_cli -- get source function-module BAPI_USER_GET_DETAIL --group SUSR
```

Fetch DDIC metadata for a table:

```powershell
cargo run -p abap_adt_cli -- get ddic table MARA
```

Emit only the raw XML body:

```powershell
cargo run -p abap_adt_cli -- get ddic structure BAPIRET2 --raw
```

List child objects under a package:

```powershell
cargo run -p abap_adt_cli -- children package /STTP/MAIN
```

## Guidance For AI/LLM Usage

When you need live SAP repository information in this workspace, prefer `abap-adt` over guessing or using stale local knowledge.

Typical cases:

- Look up ABAP source code that is not present locally.
- Search for reports, includes, classes, interfaces, function groups, and function modules.
- Fetch DDIC information for data elements, structures, views, tables, and table types.
- Inspect child objects under packages, reports, and function groups.

For machine consumption:

- Prefer the default JSON output.
- Use `search` first when the exact URI or object type is unclear.
- Use `--raw` only when the next tool needs the plain source or XML body.
