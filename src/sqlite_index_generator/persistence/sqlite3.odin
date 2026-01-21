package persistence

import "base:runtime"
import "core:fmt"
import "core:mem"
import "core:strconv"
import "core:strings"
import "sqlite3"

Field :: struct {
	name:      cstring,
	raw_value: cstring,
}

field_to_int :: proc(field: Field) -> (value: i64, ok: bool) {
	data_str := string(field.raw_value)
	if len(data_str) == 0 {
		return 0, false
	}
	return strconv.parse_i64(data_str)
}

field_to_float :: proc(field: Field) -> (value: f64, ok: bool) {
	data_str := string(field.raw_value)
	if len(data_str) == 0 {
		return 0, false
	}
	return strconv.parse_f64(data_str)
}

Row :: struct {
	fields: [dynamic]Field,
}

row_field_by_name :: proc(row: Row, field_name: cstring) -> Maybe(Field) {
	for field in row.fields {
		if field.name == field_name {
			return field
		}
	}
	return nil
}

Query_Result :: struct {
	rows: [dynamic]Row,
}

query_result_deinit :: proc(query_result: ^Query_Result) {
	if query_result.rows != nil {
		for row in query_result.rows {
			delete(row.fields)
		}
		delete(query_result.rows)
	}
}

// Parameter value for binding
ParamValue :: union {
	cstring,
	string,
	i32,
	i64,
	f64,
	[]byte,
}

exec_with_params :: proc(
	allocator: runtime.Allocator,
	conn: rawptr,
	query_str: string,
	params: []ParamValue,
) -> (
	query_results: Query_Result,
	ok: bool,
	err_descr: cstring,
) {
	context.allocator = allocator

	query_c := strings.clone_to_cstring(query_str, context.temp_allocator)

	stmt: sqlite3.Stmt
	conn := cast(sqlite3.Conn)conn
	rc := sqlite3.prepare_v2(conn, query_c, -1, &stmt, nil)
	if rc != sqlite3.OK {
		err_msg := clone_cstring(sqlite3.errmsg(conn))
		return query_results, false, err_msg
	}
	defer sqlite3.finalize(stmt)

	// Bind parameters
	for param, i in params {
		param_idx := i32(i + 1) // SQLite3 uses 1-based indexing
		rc: i32

		switch v in param {
		case cstring:
			rc = sqlite3.bind_text(stmt, param_idx, v, -1, nil)
		case string:
			param_cstr := strings.clone_to_cstring(v, context.temp_allocator)
			rc = sqlite3.bind_text(stmt, param_idx, param_cstr, -1, nil)
		case i32:
			rc = sqlite3.bind_int(stmt, param_idx, v)
		case i64:
			rc = sqlite3.bind_int64(stmt, param_idx, v)
		case f64:
			rc = sqlite3.bind_double(stmt, param_idx, v)
		case []byte:
			if len(v) > 0 {
				rc = sqlite3.bind_blob(stmt, param_idx, raw_data(v), i32(len(v)), nil)
			} else {
				rc = sqlite3.bind_null(stmt, param_idx)
			}
		}

		if rc != sqlite3.OK {
			err_msg := clone_cstring(sqlite3.errmsg(conn))
			return query_results, false, err_msg
		}
	}

	num_cols := sqlite3.column_count(stmt)

	column_names := make([dynamic]cstring, num_cols, context.temp_allocator)
	for i in 0 ..< num_cols {
		column_names[i] = clone_cstring(sqlite3.column_name(stmt, i32(i)))
	}

	query_results.rows = make([dynamic]Row)

	for {
		rc = sqlite3.step(stmt)
		if rc == sqlite3.DONE {
			break
		}
		if rc != sqlite3.ROW {
			err_msg := clone_cstring(sqlite3.errmsg(conn))
			return query_results, false, err_msg
		}
		if num_cols <= 0 {
			return query_results, true, nil
		}

		fields := make([dynamic]Field, 0, num_cols)
		for col_idx in 0 ..< num_cols {
			col_name := column_names[col_idx]
			col_type := sqlite3.column_type(stmt, i32(col_idx))

			field := Field {
				name      = col_name,
				raw_value = "",
			}

			if col_type == sqlite3.NULL {
				field.raw_value = ""
			} else if col_type == sqlite3.TEXT {
				text_val := sqlite3.column_text(stmt, i32(col_idx))
				if text_val != nil {
					text_str := strings.clone_to_cstring(string(text_val))
					field.raw_value = text_str
				}
			} else if col_type == sqlite3.INTEGER {
				int_val := sqlite3.column_int64(stmt, i32(col_idx))
				int_str := fmt.aprintf("%d", int_val)
				field.raw_value = strings.clone_to_cstring(int_str)
			} else if col_type == sqlite3.FLOAT {
				float_val := sqlite3.column_double(stmt, i32(col_idx))
				// Convert to string
				float_str := fmt.aprintf("%.17g", float_val)
				field.raw_value = strings.clone_to_cstring(float_str)
			} else if col_type == sqlite3.BLOB {
				blob_ptr := sqlite3.column_blob(stmt, i32(col_idx))
				blob_len := sqlite3.column_bytes(stmt, i32(col_idx))
				if blob_ptr != nil && blob_len > 0 {
					// Convert blob to hex string representation
					blob_bytes := mem.slice_ptr((^byte)(blob_ptr), int(blob_len))
					hex_str := fmt.aprintf("%x", blob_bytes)
					field.raw_value = strings.clone_to_cstring(hex_str)
				}
			}

			append(&fields, field)
		}
		append(&query_results.rows, Row{fields})
	}

	return query_results, true, nil
}

exec :: proc(
	allocator: runtime.Allocator,
	conn: rawptr,
	query_str: string,
) -> (
	query_results: Query_Result,
	ok: bool,
	err_descr: cstring,
) {
	return exec_with_params(allocator, conn, query_str, {})
}
