package tests_lsp

import "../../src/lsp"
import "core:strings"
import "core:testing"

@(test)
completion_after_insert_assigning_field_symbol_offers_struct_fields_test :: proc(t: ^testing.T) {
	// Cursor immediately after `-` (before component name); rest of line kept valid for the parser.
	source :=
		`TYPES:
BEGIN OF ts_ui_funcs,
  textid TYPE c LENGTH 3,
  text_add TYPE string,
  disabled TYPE c LENGTH 1,
END OF ts_ui_funcs,
tt_ui_funcs TYPE STANDARD TABLE OF ts_ui_funcs WITH DEFAULT KEY.
DATA: lt_rep_response TYPE tt_ui_funcs.
INSERT INITIAL LINE INTO TABLE lt_rep_response ASSIGNING FIELD-SYMBOL(<ls_enc>).
<ls_enc>-textid.`
	snap := make_snapshot(t, source)
	if snap == nil do return

	prefix := "<ls_enc>-"
	idx := strings.index(snap.text, prefix)
	if !testing.expect(t, idx >= 0, "expected field-symbol access in source") do return
	offset := idx + len(prefix)
	items := lsp.collect_completion_items(snap, offset, snap.symbol_table)

	labels_builder := strings.builder_make(context.temp_allocator)
	for it, i in items {
		if i > 0 do strings.write_string(&labels_builder, ", ")
		strings.write_string(&labels_builder, it.label)
	}
	labels_concat := strings.to_string(labels_builder)
	testing.expectf(t, len(items) == 3, "expected 3 field items, got %d: %s", len(items), labels_concat)
	testing.expectf(
		t,
		completion_has_label(items[:], "textid"),
		"missing textid in %s",
		labels_concat,
	)
	testing.expectf(
		t,
		completion_has_label(items[:], "text_add"),
		"missing text_add in %s",
		labels_concat,
	)
	testing.expectf(
		t,
		completion_has_label(items[:], "disabled"),
		"missing disabled in %s",
		labels_concat,
	)
}

@(test)
completion_unresolved_structure_selector_returns_empty_test :: proc(t: ^testing.T) {
	source := `DATA: lt TYPE i.
lt_unknown-field = 1.`
	snap := make_snapshot(t, source)
	if snap == nil do return

	prefix := "lt_unknown-"
	idx := strings.index(snap.text, prefix)
	if !testing.expect(t, idx >= 0, "expected lt_unknown- in source") do return
	offset := idx + len(prefix)
	items := lsp.collect_completion_items(snap, offset, snap.symbol_table)

	testing.expectf(t, len(items) == 0, "expected no fallback items, got %d", len(items))
}

@(test)
completion_in_class_method_after_insert_assigning_field_symbol_test :: proc(t: ^testing.T) {
	source := `CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create_entry_cusset.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
  METHOD create_entry_cusset.
    TYPES:
      BEGIN OF ts_ui_funcs,
        textid TYPE char3,
        text_add TYPE string,
        disabled TYPE char01,
      END OF ts_ui_funcs,
      tt_ui_funcs TYPE STANDARD TABLE OF ts_ui_funcs WITH DEFAULT KEY.
    DATA: lt_rep_response TYPE tt_ui_funcs.
    INSERT INITIAL LINE INTO TABLE lt_rep_response ASSIGNING FIELD-SYMBOL(<ls_enc>).
    <ls_enc>-textid.
  ENDMETHOD.
ENDCLASS.`
	snap := make_snapshot(t, source)
	if snap == nil do return

	prefix := "<ls_enc>-"
	idx := strings.index(snap.text, prefix)
	if !testing.expect(t, idx >= 0, "expected field-symbol access in source") do return
	offset := idx + len(prefix)
	items := lsp.collect_completion_items(snap, offset, snap.symbol_table)

	labels_builder := strings.builder_make(context.temp_allocator)
	for it, i in items {
		if i > 0 do strings.write_string(&labels_builder, ", ")
		strings.write_string(&labels_builder, it.label)
	}
	labels_concat := strings.to_string(labels_builder)
	testing.expectf(t, len(items) == 3, "expected 3 field items, got %d: %s", len(items), labels_concat)
	testing.expectf(t, completion_has_label(items[:], "textid"), "missing textid in %s", labels_concat)
	testing.expectf(t, completion_has_label(items[:], "text_add"), "missing text_add in %s", labels_concat)
	testing.expectf(t, completion_has_label(items[:], "disabled"), "missing disabled in %s", labels_concat)
}

@(test)
completion_nested_class_constants_selector_chain_test :: proc(t: ^testing.T) {
	source := `CLASS zcl_demo DEFINITION
  FINAL
  CREATE PUBLIC .
PUBLIC SECTION.
  METHODS exec.
PRIVATE SECTION.
  CONSTANTS:
    BEGIN OF gcs_const_level1,
        BEGIN OF const_level2,
          const_level3_1 TYPE string  VALUE 'VALUE1' ##no_text,
          const_level3_2 TYPE string  VALUE 'VALUE2' ##no_text,
        END OF const_level2,
      END OF gcs_const_level1 .
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD exec.
      DATA(lv_some_val) = gcs_const_level1-const_level2-MARK.
  ENDMETHOD.
ENDCLASS.`
	snap := make_snapshot(t, source)
	if snap == nil do return

	mark := "MARK."
	midx := strings.index(snap.text, mark)
	if !testing.expect(t, midx >= 0, "expected MARK placeholder in source") do return
	offset := midx

	items := lsp.collect_completion_items(snap, offset, snap.symbol_table)
	labels_builder := strings.builder_make(context.temp_allocator)
	for it, i in items {
		if i > 0 do strings.write_string(&labels_builder, ", ")
		strings.write_string(&labels_builder, it.label)
	}
	labels_concat := strings.to_string(labels_builder)
	testing.expectf(t, len(items) == 2, "expected 2 leaf const fields, got %d: %s", len(items), labels_concat)
	testing.expectf(
		t,
		completion_has_label(items[:], "const_level3_1"),
		"missing const_level3_1 in %s",
		labels_concat,
	)
	testing.expectf(
		t,
		completion_has_label(items[:], "const_level3_2"),
		"missing const_level3_2 in %s",
		labels_concat,
	)

	prefix2 := "gcs_const_level1-"
	idx2 := strings.index(snap.text, prefix2)
	if !testing.expect(t, idx2 >= 0, "expected gcs_const_level1- in source") do return
	offset2 := idx2 + len(prefix2)
	items2 := lsp.collect_completion_items(snap, offset2, snap.symbol_table)
	testing.expectf(
		t,
		completion_has_label(items2[:], "const_level2"),
		"after first dash: expected const_level2 in completions, got %d items",
		len(items2),
	)
}

completion_has_label :: proc(items: []lsp.CompletionItem, want: string) -> bool {
	for it in items {
		if it.label == want {
			return true
		}
	}
	return false
}
