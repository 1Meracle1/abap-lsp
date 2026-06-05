package string_interner

import "core:testing"
import "core:thread"

test_options :: proc() -> Options {
	return Options {
		cell_count   = 16,
		reserve_size = 64 * 1024 * 1024,
		commit_size  = 1024 * 1024,
		track_count  = true,
	}
}

@(test)
empty_string_is_the_zero_interned_value :: proc(t: ^testing.T) {
	interner := create(test_options())
	defer destroy(interner)

	hash: u32
	interned := insert(interner, "", new_hash = &hash)

	testing.expect(t, !is_valid(interned))
	testing.expect_value(t, load(interner, interned), "")
	testing.expect_value(t, hash, string_hash(""))
}

@(test)
same_string_reuses_the_existing_offset :: proc(t: ^testing.T) {
	interner := create(test_options())
	defer destroy(interner)

	a := insert(interner, "zcl_demo")
	b := insert(interner, "zcl_demo")

	testing.expect_value(t, a, b)
	testing.expect_value(t, load(interner, a), "zcl_demo")
	testing.expect(t, is_blank(interner, insert(interner, "_")))
	testing.expect_value(t, tracked_count(interner), u64(2))
}

@(test)
hash_collisions_keep_distinct_strings :: proc(t: ^testing.T) {
	interner := create(test_options())
	defer destroy(interner)

	collision_hash := u32(0x1234)
	left := insert(interner, "left", collision_hash)
	right := insert(interner, "right", collision_hash)
	left_again := insert(interner, "left", collision_hash)

	testing.expect(t, left != right)
	testing.expect_value(t, left, left_again)
	testing.expect_value(t, load(interner, left), "left")
	testing.expect_value(t, load(interner, right), "right")
}

@(test)
overflow_cells_are_linked_and_searchable :: proc(t: ^testing.T) {
	options := test_options()
	options.cell_count = 1
	interner := create(options)
	defer destroy(interner)

	values := [?]string{
		"v00", "v01", "v02", "v03", "v04",
		"v05", "v06", "v07", "v08", "v09",
	}
	interned: [len(values)]String
	for value, i in values {
		interned[i] = insert(interner, value)
	}
	for value, i in values {
		testing.expect_value(t, load(interner, interned[i]), value)
		testing.expect_value(t, insert(interner, value), interned[i])
	}
	testing.expect_value(t, tracked_count(interner), u64(len(values) + 1))
}

Thread_Work :: struct {
	interner: ^Interner,
	values:   []string,
	ok:       bool,
}

thread_worker :: proc(worker: ^thread.Thread) {
	work := cast(^Thread_Work)worker.data
	work.ok = true
	for value in work.values {
		interned := insert(work.interner, value)
		if load(work.interner, interned) != value {
			work.ok = false
			return
		}
		shared := insert(work.interner, "shared")
		if load(work.interner, shared) != "shared" {
			work.ok = false
			return
		}
	}
}

@(test)
multiple_threads_share_the_table_with_thread_local_slabs :: proc(t: ^testing.T) {
	options := test_options()
	options.cell_count = 64
	interner := create(options)
	defer destroy(interner)

	values := [?]string{
		"thread-0-a", "thread-0-b", "thread-0-c", "thread-0-d",
		"thread-1-a", "thread-1-b", "thread-1-c", "thread-1-d",
		"thread-2-a", "thread-2-b", "thread-2-c", "thread-2-d",
		"thread-3-a", "thread-3-b", "thread-3-c", "thread-3-d",
	}

	works: [4]Thread_Work
	threads: [4]^thread.Thread
	for i in 0 ..< len(works) {
		works[i] = Thread_Work {
			interner = interner,
			values   = values[i * 4:][:4],
		}
		threads[i] = thread.create(thread_worker)
		threads[i].data = &works[i]
		thread.start(threads[i])
	}
	for worker in threads {
		thread.join(worker)
		thread.destroy(worker)
	}
	for work in works {
		testing.expect(t, work.ok)
	}
	for value in values {
		testing.expect_value(t, load(interner, insert(interner, value)), value)
	}
	testing.expect_value(t, load(interner, insert(interner, "shared")), "shared")
	testing.expect_value(t, tracked_count(interner), u64(len(values) + 2))
}
