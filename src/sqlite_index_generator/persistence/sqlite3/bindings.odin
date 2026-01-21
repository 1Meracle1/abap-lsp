package persistence_sqlite3

// odinfmt: disable
foreign import lib {
	"../sqlite3_x64-windows-static/debug/lib/sqlite3.lib" when ODIN_DEBUG else "../sqlite3_x64-windows-static/lib/sqlite3.lib",
}
// odinfmt: enable

Conn :: distinct rawptr
Stmt :: distinct rawptr

OK :: 0 /* Successful result */
/* beginning-of-error-codes */
ERROR :: 1 /* Generic error */
INTERNAL :: 2 /* Internal logic error in SQLite */
PERM :: 3 /* Access permission denied */
ABORT :: 4 /* Callback routine requested an abort */
BUSY :: 5 /* The database file is locked */
LOCKED :: 6 /* A table in the database is locked */
NOMEM :: 7 /* A malloc() failed */
READONLY :: 8 /* Attempt to write a readonly database */
INTERRUPT :: 9 /* Operation terminated by sqlite3_interrupt()*/
IOERR :: 10 /* Some kind of disk I/O error occurred */
CORRUPT :: 11 /* The database disk image is malformed */
NOTFOUND :: 12 /* Unknown opcode in sqlite3_file_control() */
FULL :: 13 /* Insertion failed because database is full */
CANTOPEN :: 14 /* Unable to open the database file */
PROTOCOL :: 15 /* Database lock protocol error */
EMPTY :: 16 /* Internal use only */
SCHEMA :: 17 /* The database schema changed */
TOOBIG :: 18 /* String or BLOB exceeds size limit */
CONSTRAINT :: 19 /* Abort due to constraint violation */
MISMATCH :: 20 /* Data type mismatch */
MISUSE :: 21 /* Library used incorrectly */
NOLFS :: 22 /* Uses OS features not supported on host */
AUTH :: 23 /* Authorization denied */
FORMAT :: 24 /* Not used */
RANGE :: 25 /* 2nd parameter to sqlite3_bind out of range */
NOTADB :: 26 /* File opened that is not a database file */
NOTICE :: 27 /* Notifications from sqlite3_log() */
WARNING :: 28 /* Warnings from sqlite3_log() */
ROW :: 100 /* sqlite3_step() has another row ready */
DONE :: 101 /* sqlite3_step() has finished executing */

INTEGER :: 1
FLOAT :: 2
TEXT :: 3
BLOB :: 4
NULL :: 5

Exec_Callback_Proc :: #type proc(_: rawptr, _: i32, _: cstring, _: cstring) -> i32
Bind_Destructor_Proc :: #type proc(_: rawptr)

@(link_prefix = "sqlite3_")
foreign lib {
	open :: proc(filename: cstring, db: ^Conn) -> i32 ---
	close :: proc(db: Conn) -> i32 ---
	errmsg :: proc(db: Conn) -> cstring ---
	exec :: proc(db: Conn, sql: cstring, callback: Exec_Callback_Proc, userptr: rawptr, errmsg: ^cstring) -> i32 ---
	prepare_v2 :: proc(db: Conn,
		zSql: cstring, /* SQL statement, UTF-8 encoded */
		nByte: i32, /* Maximum length of zSql in bytes. */
		ppStmt: ^Stmt, /* OUT: Statement handle */
		pzTail: ^cstring, /* OUT: Pointer to unused portion of zSql */) -> i32 ---
	bind_text :: proc(stmt: Stmt, index: i32, value: cstring, length: i32, destructor: Bind_Destructor_Proc) -> i32 ---
	bind_int :: proc(stmt: Stmt, index: i32, value: i32) -> i32 ---
	bind_int64 :: proc(stmt: Stmt, index: i32, value: i64) -> i32 ---
	bind_double :: proc(stmt: Stmt, index: i32, value: f64) -> i32 ---
	bind_blob :: proc(stmt: Stmt, index: i32, value: rawptr, n: i32, destructor: Bind_Destructor_Proc) -> i32 ---
	bind_null :: proc(stmt: Stmt, index: i32) -> i32 ---
	bind_parameter_index :: proc(stmt: Stmt, zName: cstring) -> i32 ---
	bind_parameter_count :: proc(stmt: Stmt) -> i32 ---
	reset :: proc(stmt: Stmt) -> i32 ---
	finalize :: proc(stmt: Stmt) -> i32 ---
	step :: proc(stmt: Stmt) -> i32 ---
	column_count :: proc(stmt: Stmt) -> i32 ---
	column_name :: proc(stmt: Stmt, iCol: i32) -> cstring ---
	column_type :: proc(stmt: Stmt, iCol: i32) -> i32 ---
	column_int :: proc(stmt: Stmt, iCol: i32) -> i32 ---
	column_int64 :: proc(stmt: Stmt, iCol: i32) -> i64 ---
	column_double :: proc(stmt: Stmt, iCol: i32) -> f64 ---
	column_text :: proc(stmt: Stmt, iCol: i32) -> cstring ---
	column_blob :: proc(stmt: Stmt, iCol: i32) -> rawptr ---
	column_bytes :: proc(stmt: Stmt, iCol: i32) -> i32 ---
	changes :: proc(db: Conn) -> i32 ---
	last_insert_rowid :: proc(db: Conn) -> i64 ---
	errcode :: proc(db: Conn) -> i32 ---
}
