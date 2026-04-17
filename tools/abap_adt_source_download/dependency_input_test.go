package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadDependencyTextDefaultsKindAndDedupes(t *testing.T) {
	t.Parallel()

	tempDir := t.TempDir()
	path := filepath.Join(tempDir, "candidates.txt")
	err := os.WriteFile(path, []byte("ZCL_DEMO\nzcl_demo|type\n/aif/file_process_data|function\n"), 0o644)
	if err != nil {
		t.Fatalf("write candidates file: %v", err)
	}

	input, err := loadDependencyInput(path, tempDir)
	if err != nil {
		t.Fatalf("load dependency input: %v", err)
	}

	if got, want := len(input.Candidates), 2; got != want {
		t.Fatalf("candidate count = %d, want %d", got, want)
	}
	if input.Candidates[0].Name != "/aif/file_process_data" || input.Candidates[0].Kind != "function" {
		t.Fatalf("unexpected first candidate: %#v", input.Candidates[0])
	}
	if input.Candidates[1].Name != "zcl_demo" || input.Candidates[1].Kind != "type" {
		t.Fatalf("unexpected second candidate: %#v", input.Candidates[1])
	}
}

func TestLoadDependencyJSONBuildsSourceFileMap(t *testing.T) {
	t.Parallel()

	workspaceRoot := t.TempDir()
	inputPath := filepath.Join(workspaceRoot, "candidates.json")
	err := os.WriteFile(inputPath, []byte(`{
  "workspace_root_uri": "file:///`+filepath.ToSlash(workspaceRoot)+`",
  "source_uris": ["file:///`+filepath.ToSlash(filepath.Join(workspaceRoot, "src", "ZMAIN.abap"))+`"],
  "source_candidates": {
    "file:///`+filepath.ToSlash(filepath.Join(workspaceRoot, "src", "ZMAIN.abap"))+`": [
      { "name": "zcl_demo", "kind": "type" },
      { "name": "ZCL_DEMO", "kind": "symbol" }
    ]
  },
  "candidates": [
    { "name": "zcl_demo", "kind": "type" }
  ]
}`), 0o644)
	if err != nil {
		t.Fatalf("write candidates file: %v", err)
	}

	input, err := loadDependencyInput(inputPath, workspaceRoot)
	if err != nil {
		t.Fatalf("load dependency input: %v", err)
	}

	key := remoteCandidateKey(RemoteDependencyCandidate{Name: "zcl_demo", Kind: "type"})
	sourceFiles := input.SourceFiles[key]
	if len(sourceFiles) != 1 || sourceFiles[0] != "src/ZMAIN.abap" {
		t.Fatalf("unexpected source files: %#v", sourceFiles)
	}
}
