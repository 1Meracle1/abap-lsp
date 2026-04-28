# Change Log

All notable changes to the ABAP LSP VS Code extension are documented here.

## [Unreleased]

### Added

- Product metadata for the ABAP LSP extension package.
- Onboarding documentation for stdio and TCP server startup, workspace
  manifests, SAP ADT connection setup, dependency cache behavior, and common
  troubleshooting flows.

### Changed

- Replaced generated sample-extension README content with ABAP LSP-specific
  setup and operation guidance.
- Aligned the nested VS Code client package metadata with this repository.
- Replaced generated sample client tests with an ABAP LSP activation smoke and
  a cross-platform VS Code test runner.

### Fixed

- CASE folding now treats `ELSE` as the nearest open IF or CASE arm, so CASE
  branch folding matches the client helper tests.

### Removed

- Generated VS Code extension quickstart document from the extension package
  source tree.
