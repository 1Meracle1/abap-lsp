package abap_frontend_persistence_sqlite3

import "core:c"

VERSION :: "3.53.1"
VERSION_NUMBER :: 3053001

Connection :: struct {}
Statement :: struct {}
Backup :: struct {}
Blob :: struct {}

Result_Code :: distinct c.int

OK         :: Result_Code(0)
ERROR      :: Result_Code(1)
INTERNAL   :: Result_Code(2)
PERM       :: Result_Code(3)
ABORT      :: Result_Code(4)
BUSY       :: Result_Code(5)
LOCKED     :: Result_Code(6)
NOMEM      :: Result_Code(7)
READONLY   :: Result_Code(8)
INTERRUPT  :: Result_Code(9)
IOERR      :: Result_Code(10)
CORRUPT    :: Result_Code(11)
NOTFOUND   :: Result_Code(12)
FULL       :: Result_Code(13)
CANTOPEN   :: Result_Code(14)
PROTOCOL   :: Result_Code(15)
EMPTY      :: Result_Code(16)
SCHEMA     :: Result_Code(17)
TOOBIG     :: Result_Code(18)
CONSTRAINT :: Result_Code(19)
MISMATCH   :: Result_Code(20)
MISUSE     :: Result_Code(21)
NOLFS      :: Result_Code(22)
AUTH       :: Result_Code(23)
FORMAT     :: Result_Code(24)
RANGE      :: Result_Code(25)
NOTADB     :: Result_Code(26)
NOTICE     :: Result_Code(27)
WARNING    :: Result_Code(28)
ROW        :: Result_Code(100)
DONE       :: Result_Code(101)

Open_Flags :: distinct c.int

OPEN_READONLY     :: Open_Flags(0x00000001)
OPEN_READWRITE    :: Open_Flags(0x00000002)
OPEN_CREATE       :: Open_Flags(0x00000004)
OPEN_URI          :: Open_Flags(0x00000040)
OPEN_MEMORY       :: Open_Flags(0x00000080)
OPEN_NOMUTEX      :: Open_Flags(0x00008000)
OPEN_FULLMUTEX    :: Open_Flags(0x00010000)
OPEN_SHAREDCACHE  :: Open_Flags(0x00020000)
OPEN_PRIVATECACHE :: Open_Flags(0x00040000)
OPEN_NOFOLLOW     :: Open_Flags(0x01000000)

Prepare_Flags :: distinct c.uint

PREPARE_PERSISTENT :: Prepare_Flags(0x01)
PREPARE_NORMALIZE  :: Prepare_Flags(0x02)
PREPARE_NO_VTAB    :: Prepare_Flags(0x04)
PREPARE_DONT_LOG   :: Prepare_Flags(0x10)

Column_Type :: distinct c.int

TYPE_INTEGER :: Column_Type(1)
TYPE_FLOAT   :: Column_Type(2)
TYPE_TEXT    :: Column_Type(3)
TYPE_BLOB    :: Column_Type(4)
TYPE_NULL    :: Column_Type(5)

Destructor_Behavior :: enum int {
	Static = 0,
	Transient = -1,
}

Destructor :: struct #raw_union {
	callback: proc "c" (rawptr),
	behavior: Destructor_Behavior,
}

DESTRUCTOR_STATIC: Destructor = Destructor{behavior = .Static}
DESTRUCTOR_TRANSIENT: Destructor = Destructor{behavior = .Transient}

Exec_Callback :: #type proc "c" (
	ctx: rawptr,
	column_count: c.int,
	values: [^]cstring,
	column_names: [^]cstring,
) -> c.int

when ODIN_OS == .Windows && ODIN_ARCH == .amd64 {
	when ODIN_DEBUG {
		foreign import sqlite3_lib "lib/windows-amd64/debug/sqlite3.lib"
	} else {
		foreign import sqlite3_lib "lib/windows-amd64/release/sqlite3.lib"
	}
} else {
	foreign import sqlite3_lib "system:sqlite3"
}

@(default_calling_convention = "c", link_prefix = "sqlite3_")
foreign sqlite3_lib {
	libversion :: proc() -> cstring ---
	libversion_number :: proc() -> c.int ---
	sourceid :: proc() -> cstring ---
	compileoption_used :: proc(name: cstring) -> c.int ---
	compileoption_get :: proc(index: c.int) -> cstring ---
	threadsafe :: proc() -> c.int ---
	initialize :: proc() -> Result_Code ---
	shutdown :: proc() -> Result_Code ---

	open :: proc(filename: cstring, db: ^^Connection) -> Result_Code ---
	open_v2 :: proc(filename: cstring, db: ^^Connection, flags: Open_Flags, vfs: cstring) -> Result_Code ---
	close :: proc(db: ^Connection) -> Result_Code ---
	close_v2 :: proc(db: ^Connection) -> Result_Code ---
	errcode :: proc(db: ^Connection) -> Result_Code ---
	extended_errcode :: proc(db: ^Connection) -> Result_Code ---
	extended_result_codes :: proc(db: ^Connection, onoff: c.int) -> c.int ---
	errmsg :: proc(db: ^Connection) -> cstring ---
	errstr :: proc(code: Result_Code) -> cstring ---
	exec :: proc(db: ^Connection, sql: cstring, callback: Exec_Callback, ctx: rawptr, errmsg: ^cstring) -> Result_Code ---
	free :: proc(ptr: rawptr) ---
	interrupt :: proc(db: ^Connection) ---
	is_interrupted :: proc(db: ^Connection) -> c.int ---
	busy_timeout :: proc(db: ^Connection, ms: c.int) -> Result_Code ---
	last_insert_rowid :: proc(db: ^Connection) -> c.int64_t ---
	set_last_insert_rowid :: proc(db: ^Connection, rowid: c.int64_t) ---
	changes :: proc(db: ^Connection) -> c.int ---
	changes64 :: proc(db: ^Connection) -> c.int64_t ---
	total_changes :: proc(db: ^Connection) -> c.int ---
	total_changes64 :: proc(db: ^Connection) -> c.int64_t ---

	prepare :: proc(db: ^Connection, sql: cstring, byte_count: c.int, stmt: ^^Statement, tail: ^cstring) -> Result_Code ---
	prepare_v2 :: proc(db: ^Connection, sql: cstring, byte_count: c.int, stmt: ^^Statement, tail: ^cstring) -> Result_Code ---
	prepare_v3 :: proc(db: ^Connection, sql: cstring, byte_count: c.int, flags: Prepare_Flags, stmt: ^^Statement, tail: ^cstring) -> Result_Code ---
	step :: proc(stmt: ^Statement) -> Result_Code ---
	reset :: proc(stmt: ^Statement) -> Result_Code ---
	finalize :: proc(stmt: ^Statement) -> Result_Code ---
	clear_bindings :: proc(stmt: ^Statement) -> Result_Code ---
	sql :: proc(stmt: ^Statement) -> cstring ---
	expanded_sql :: proc(stmt: ^Statement) -> cstring ---
	stmt_readonly :: proc(stmt: ^Statement) -> c.int ---
	stmt_busy :: proc(stmt: ^Statement) -> c.int ---

	bind_parameter_count :: proc(stmt: ^Statement) -> c.int ---
	bind_parameter_name :: proc(stmt: ^Statement, index: c.int) -> cstring ---
	bind_parameter_index :: proc(stmt: ^Statement, name: cstring) -> c.int ---
	bind_null :: proc(stmt: ^Statement, index: c.int) -> Result_Code ---
	bind_int :: proc(stmt: ^Statement, index: c.int, value: c.int) -> Result_Code ---
	bind_int64 :: proc(stmt: ^Statement, index: c.int, value: c.int64_t) -> Result_Code ---
	bind_double :: proc(stmt: ^Statement, index: c.int, value: c.double) -> Result_Code ---
	bind_text :: proc(stmt: ^Statement, index: c.int, value: cstring, byte_count: c.int, destructor: Destructor) -> Result_Code ---
	bind_blob :: proc(stmt: ^Statement, index: c.int, value: rawptr, byte_count: c.int, destructor: Destructor) -> Result_Code ---
	bind_zeroblob :: proc(stmt: ^Statement, index: c.int, byte_count: c.int) -> Result_Code ---

	data_count :: proc(stmt: ^Statement) -> c.int ---
	column_count :: proc(stmt: ^Statement) -> c.int ---
	column_name :: proc(stmt: ^Statement, index: c.int) -> cstring ---
	column_decltype :: proc(stmt: ^Statement, index: c.int) -> cstring ---
	column_type :: proc(stmt: ^Statement, index: c.int) -> Column_Type ---
	column_bytes :: proc(stmt: ^Statement, index: c.int) -> c.int ---
	column_blob :: proc(stmt: ^Statement, index: c.int) -> rawptr ---
	column_double :: proc(stmt: ^Statement, index: c.int) -> c.double ---
	column_int :: proc(stmt: ^Statement, index: c.int) -> c.int ---
	column_int64 :: proc(stmt: ^Statement, index: c.int) -> c.int64_t ---
	column_text :: proc(stmt: ^Statement, index: c.int) -> cstring ---

	backup_init :: proc(dest: ^Connection, dest_name: cstring, source: ^Connection, source_name: cstring) -> ^Backup ---
	backup_step :: proc(backup: ^Backup, page_count: c.int) -> Result_Code ---
	backup_finish :: proc(backup: ^Backup) -> Result_Code ---
	backup_remaining :: proc(backup: ^Backup) -> c.int ---
	backup_pagecount :: proc(backup: ^Backup) -> c.int ---

	blob_open :: proc(db: ^Connection, db_name, table, column: cstring, rowid: c.int64_t, flags: c.int, blob: ^^Blob) -> Result_Code ---
	blob_close :: proc(blob: ^Blob) -> Result_Code ---
	blob_bytes :: proc(blob: ^Blob) -> c.int ---
	blob_read :: proc(blob: ^Blob, buffer: rawptr, byte_count: c.int, offset: c.int) -> Result_Code ---
	blob_write :: proc(blob: ^Blob, buffer: rawptr, byte_count: c.int, offset: c.int) -> Result_Code ---
}
