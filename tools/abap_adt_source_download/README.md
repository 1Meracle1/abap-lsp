# ABAP ADT Source Download

Stdlib-only Go exporter for downloading ABAP package content over ADT.

The tool is intended for environments like Citrix where you want to upload a single executable and run it without pulling external Go modules on the target machine.

## Build

```bash
go build .
```

## Configuration

CLI flags are preferred, but `.env` in the current working directory is also supported.

Supported environment keys:

```env
ABAP_ADT_URL=https://your-sap-host:port/sap/bc/adt
ABAP_ADT_USER=your_username
ABAP_ADT_PASSWORD=your_password
ABAP_ADT_CLIENT=100
ABAP_ADT_OUTPUT=D:\dev\abap\sap_system_export
ABAP_ADT_PACKAGES=/STTP/MAIN,/AIF/MAIN
RATE_LIMIT_RPM=60
MAX_CONCURRENT_REQUESTS=4
```

`ABAP_ADT_URL` may also be given as the SAP host root. The exporter appends `/sap/bc/adt` automatically when needed.

## Usage

```bash
.\abap_adt_source_download.exe ^
  -url https://your-sap-host:port ^
  -user your_username ^
  -pass your_password ^
  -client 100 ^
  -output D:\dev\abap\sap_system_export ^
  -package /STTP/MAIN ^
  -package /AIF/MAIN
```

Options:

- `-package` is repeatable.
- `-clean` removes the existing export root contents before download.
- `-rpm` and `-parallel` control request pacing.

## Export Layout Compatibility

The VS Code extension's local export fallback does a recursive scan under each configured export root and looks for files named:

- `encodeURIComponent(OBJECT_NAME).abap`
- `encodeURIComponent(OBJECT_NAME).xml`

The exporter in this folder keeps encoded filenames and package directories, so it is compatible with that lookup strategy.

Example sidecar:

```toml
[local_export]
roots = ["D:/dev/abap/sap_system_export"]

[dependencies]
source = "local-first"
```

Important: this local export lookup is currently implemented in the VS Code client extension. The Rust workspace loader in this repo does not appear to consume `[local_export]` on its own yet.
