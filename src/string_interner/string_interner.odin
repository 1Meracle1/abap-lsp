package string_interner

import "core:hash"
import "core:mem"
import virtual "core:mem/virtual"
import "core:sync"

CELL_WIDTH :: 8
MUTEX_STRIPE_COUNT :: 1024
MUTEX_STRIPE_MASK :: MUTEX_STRIPE_COUNT - 1
THREAD_LOCAL_SIZE :: 2 * mem.Megabyte
CACHE_LINE_SIZE :: 128

DEFAULT_CELL_COUNT :: 1 << 17
DEFAULT_RESERVE_SIZE :: mem.Gigabyte
DEFAULT_COMMIT_SIZE :: 8 * mem.Megabyte

String :: distinct u32

Cell :: struct #align (CACHE_LINE_SIZE) {
	hashes:  [CELL_WIDTH]u64,
	offsets: [CELL_WIDTH]String,
	next:    ^Cell,
}

Padded_Mutex :: struct #align (CACHE_LINE_SIZE) {
	mutex: sync.Mutex,
}

Padded_U64 :: struct #align (CACHE_LINE_SIZE) {
	value: u64,
}

Options :: struct {
	cell_count:   int,
	reserve_size: uint,
	commit_size:  uint,
	track_count:  bool,
}

Interner :: struct #align (CACHE_LINE_SIZE) {
	cells:       []Cell,
	cell_mask:   u64,
	mutexes:     [MUTEX_STRIPE_COUNT]Padded_Mutex,
	arena:       virtual.Arena,
	blank_ident: String,
	track_count: bool,
	count:       Padded_U64,
}

Thread_Local_Arena :: struct {
	interner: ^Interner,
	data:     [^]u8,
	cursor:   uint,
}

@(private = "file", thread_local)
thread_local_arena: Thread_Local_Arena

create :: proc(options := Options{}) -> ^Interner {
	opts := normalize_options(options)

	interner, err := virtual.arena_static_bootstrap_new(Interner, "arena", opts.reserve_size)
	assert(err == nil && interner != nil)
	interner.arena.default_commit_size = opts.commit_size
	interner.track_count = opts.track_count

	cell_storage, cell_err := virtual.arena_alloc(
		&interner.arena,
		uint(opts.cell_count * size_of(Cell)),
		align_of(Cell),
	)
	assert(cell_err == nil, "string interner cell allocation failed")
	mem.zero(raw_data(cell_storage), len(cell_storage))
	interner.cells = (([^]Cell)(raw_data(cell_storage)))[:opts.cell_count]
	interner.cell_mask = u64(opts.cell_count - 1)

	interner.blank_ident = insert(interner, "_")
	return interner
}

destroy :: proc(interner: ^Interner) {
	if interner == nil {
		return
	}
	if thread_local_arena.interner == interner {
		thread_local_arena = {}
	}
	block := interner.arena.curr_block
	if block != nil {
		virtual.memory_block_dealloc(block)
	}
}

insert :: proc(
	interner: ^Interner,
	value: string,
	hash_value: u32 = 0,
	new_hash: ^u32 = nil,
) -> String {
	assert(interner != nil)
	if len(value) == 0 {
		if new_hash != nil {
			new_hash^ = string_hash(value)
		}
		return String(0)
	}

	hash_value := hash_value
	if hash_value == 0 {
		hash_value = string_hash(value)
	}
	if new_hash != nil {
		new_hash^ = hash_value
	}

	cell_idx := u64(hash_value) & interner.cell_mask
	cell := &interner.cells[int(cell_idx)]
	for {
		next := sync.atomic_load_explicit(&cell.next, .Acquire)

		for i in 0 ..< CELL_WIDTH {
			if sync.atomic_load_explicit(&cell.hashes[i], .Acquire) == u64(hash_value) {
				to_compare := load(interner, cell.offsets[i])
				if to_compare == value {
					return cell.offsets[i]
				}
			}
		}
		if next == nil {
			break
		}
		cell = next
	}

	mutex_cell := int(cell_idx & MUTEX_STRIPE_MASK)
	sync.mutex_lock(&interner.mutexes[mutex_cell].mutex)
	defer sync.mutex_unlock(&interner.mutexes[mutex_cell].mutex)

	load_cell: ^Cell
	for cell != nil {
		for i in 0 ..< CELL_WIDTH {
			if sync.atomic_load_explicit(&cell.hashes[i], .Relaxed) == u64(hash_value) {
				to_compare := load(interner, cell.offsets[i])
				if to_compare == value {
					return cell.offsets[i]
				}
			}
		}
		load_cell = cell
		cell = sync.atomic_load_explicit(&cell.next, .Relaxed)
	}
	assert(load_cell != nil)

	data_to_allocate := uint(4 + len(value) + 1)
	data := thread_local_arena_alloc(interner, data_to_allocate, 8)
	bytes := data[:int(data_to_allocate)]
	((^u32)(raw_data(bytes)))^ = u32(len(value))
	copy(bytes[4:], value)
	bytes[4 + len(value)] = 0

	offset_value := uintptr(data) - uintptr(interner)
	assert(offset_value <= uintptr(max(u32)))
	offset := String(u32(offset_value))

	for i in 0 ..< CELL_WIDTH {
		if sync.atomic_load_explicit(&load_cell.hashes[i], .Relaxed) == 0 {
			load_cell.offsets[i] = offset
			sync.atomic_store_explicit(&load_cell.hashes[i], u64(hash_value), .Release)
			increment_count(interner)
			return offset
		}
	}

	new_cell_data := thread_local_arena_alloc(interner, uint(size_of(Cell)), align_of(Cell))
	new_cell := (^Cell)(new_cell_data)
	new_cell^ = {}
	new_cell.offsets[0] = offset
	sync.atomic_store_explicit(&new_cell.hashes[0], u64(hash_value), .Relaxed)
	sync.atomic_store_explicit(&load_cell.next, new_cell, .Release)

	increment_count(interner)
	return offset
}

intern_string :: proc(
	interner: ^Interner,
	value: string,
	hash_value := u32(0),
	new_hash: ^u32 = nil,
) -> string {
	interned := insert(interner, value, hash_value, new_hash)
	return load(interner, interned)
}

intern_cstring :: proc(
	interner: ^Interner,
	value: string,
	hash_value := u32(0),
	new_hash: ^u32 = nil,
) -> cstring {
	interned := insert(interner, value, hash_value, new_hash)
	return load_cstring(interner, interned)
}

load :: proc(interner: ^Interner, interned: String) -> string {
	assert(interner != nil)
	if u32(interned) == 0 {
		return ""
	}
	base := ([^]u8)(uintptr(interner) + uintptr(u32(interned)))
	str_len := int(((^u32)(base))^)
	text := ([^]u8)(uintptr(base) + 4)
	return string(text[:str_len])
}

load_cstring :: proc(interner: ^Interner, interned: String) -> cstring {
	assert(interner != nil)
	if u32(interned) == 0 {
		return cstring("")
	}
	base := ([^]u8)(uintptr(interner) + uintptr(u32(interned)))
	text := ([^]u8)(uintptr(base) + 4)
	return cstring(text)
}

hash_interned :: proc(interner: ^Interner, interned: String) -> u32 {
	return string_hash(load(interner, interned))
}

is_blank :: proc(interner: ^Interner, interned: String) -> bool {
	assert(interner != nil)
	return interned == interner.blank_ident
}

is_valid :: proc(interned: String) -> bool {
	return u32(interned) != 0
}

string_hash :: proc "contextless" (value: string) -> u32 {
	res := hash.fnv32a(transmute([]byte)value) & 0x7fff_ffff
	if res == 0 {
		return 1
	}
	return res
}

tracked_count :: proc(interner: ^Interner) -> u64 {
	assert(interner != nil)
	return sync.atomic_load_explicit(&interner.count.value, .Acquire)
}

normalize_options :: proc(options: Options) -> Options {
	opts := options
	if opts.cell_count == 0 {
		opts.cell_count = DEFAULT_CELL_COUNT
	}
	if opts.reserve_size == 0 {
		opts.reserve_size = DEFAULT_RESERVE_SIZE
	}
	if opts.commit_size == 0 {
		opts.commit_size = DEFAULT_COMMIT_SIZE
	}
	assert(is_power_of_two(opts.cell_count))
	assert(opts.cell_count > 0)
	assert(is_power_of_two_uint(opts.reserve_size))
	assert(is_power_of_two_uint(opts.commit_size))
	assert(opts.commit_size <= opts.reserve_size)
	return opts
}

is_power_of_two :: proc "contextless" (n: int) -> bool {
	return n > 0 && (n & (n - 1)) == 0
}

is_power_of_two_uint :: proc "contextless" (n: uint) -> bool {
	return n > 0 && (n & (n - 1)) == 0
}

increment_count :: proc "contextless" (interner: ^Interner) {
	if interner.track_count {
		sync.atomic_add_explicit(&interner.count.value, u64(1), .Relaxed)
	}
}

thread_local_arena_alloc :: proc(interner: ^Interner, size: uint, alignment: uint) -> [^]u8 {
	assert(size <= THREAD_LOCAL_SIZE)
	assert(is_power_of_two_uint(alignment))

	tl := &thread_local_arena
	if tl.interner != interner {
		tl.interner = interner
		tl.data = nil
		tl.cursor = THREAD_LOCAL_SIZE
	}
	if tl.data == nil {
		tl.cursor = THREAD_LOCAL_SIZE
	}

	new_head := mem.align_forward_uint(tl.cursor, alignment)
	cursor := new_head + size
	if cursor > THREAD_LOCAL_SIZE {
		slab, slab_err := virtual.arena_alloc(&interner.arena, THREAD_LOCAL_SIZE, 4096)
		assert(slab_err == nil, "string interner slab allocation failed")
		tl.data = ([^]u8)(raw_data(slab))
		tl.cursor = 0
		return thread_local_arena_alloc(interner, size, alignment)
	}

	out := ([^]u8)(uintptr(tl.data) + uintptr(new_head))
	tl.cursor = cursor
	return out
}
