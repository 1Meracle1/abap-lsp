package abap_frontend_persistence_sqlite3

import "core:c"
import "core:testing"

@(test)
memory_database_round_trips_values :: proc(t: ^testing.T) {
	db: ^Connection
	testing.expect_value(t, open(":memory:", &db), OK)
	defer close(db)

	testing.expect_value(t, exec(db, "create table item(id integer primary key, name text)", nil, nil, nil), OK)

	insert: ^Statement
	testing.expect_value(t, prepare_v2(db, "insert into item(name) values (?)", -1, &insert, nil), OK)
	testing.expect_value(t, bind_text(insert, 1, "alpha", -1, DESTRUCTOR_TRANSIENT), OK)
	testing.expect_value(t, step(insert), DONE)
	testing.expect_value(t, finalize(insert), OK)

	select_stmt: ^Statement
	testing.expect_value(t, prepare_v2(db, "select id, name from item", -1, &select_stmt, nil), OK)
	defer finalize(select_stmt)

	testing.expect_value(t, step(select_stmt), ROW)
	testing.expect_value(t, column_int(select_stmt, 0), c.int(1))
	testing.expect_value(t, column_type(select_stmt, 1), TYPE_TEXT)
	testing.expect_value(t, column_bytes(select_stmt, 1), c.int(5))
	testing.expect(t, column_text(select_stmt, 1) != nil)
	testing.expect_value(t, step(select_stmt), DONE)
}

@(test)
reports_linked_sqlite_version :: proc(t: ^testing.T) {
	testing.expect_value(t, libversion_number(), c.int(VERSION_NUMBER))
	testing.expect(t, threadsafe() != 0)
}
