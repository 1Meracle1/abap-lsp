package main

import (
	"os"
	"path/filepath"
	"reflect"
	"testing"
)

func TestLoadObjectFilterTrimsCommentsAndDedupesCase(t *testing.T) {
	t.Parallel()

	tempDir := t.TempDir()
	objectsFile := filepath.Join(tempDir, "objects.txt")
	err := os.WriteFile(objectsFile, []byte("\uFEFF# comment\nZCL_FOO\n\nzcl_foo\n; ignored\n/AIF/FILE_PROCESS_DATA\n"), 0o644)
	if err != nil {
		t.Fatalf("write objects file: %v", err)
	}

	filter, err := loadObjectFilter(objectsFile)
	if err != nil {
		t.Fatalf("load object filter: %v", err)
	}

	if got, want := filter.RequestedCount(), 2; got != want {
		t.Fatalf("requested count = %d, want %d", got, want)
	}
	if !filter.Match("zcl_foo") {
		t.Fatal("expected zcl_foo to match")
	}
	if !filter.Match("/aif/file_process_data") {
		t.Fatal("expected /aif/file_process_data to match")
	}
	if filter.Match("zcl_bar") {
		t.Fatal("did not expect zcl_bar to match")
	}
}

func TestObjectFilterReportsUnmatchedRequestedNames(t *testing.T) {
	t.Parallel()

	filter := &ObjectFilter{
		requested: map[string]string{
			"zcl_first":  "ZCL_FIRST",
			"zcl_second": "zcl_second",
		},
		matched: map[string]struct{}{},
	}

	if !filter.Match("ZCL_FIRST") {
		t.Fatal("expected ZCL_FIRST to match")
	}

	got := filter.Unmatched()
	want := []string{"zcl_second"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("unmatched = %#v, want %#v", got, want)
	}
}
