package cache

import "core:container/xar"
import "core:mem"
import "core:mem/virtual"
import "core:sync"

Arena_Slot :: struct {
	pool:      ^Arena_Pool,
	arena:     virtual.Arena,
	allocator: mem.Allocator,
	in_use:    bool,
}

Arena_Pool :: struct {
	mutex: sync.Mutex,
	slots: xar.Array(^Arena_Slot, 4),
}

arena_pool_init :: proc(initial_slots: int) -> ^Arena_Pool {
	pool := new(Arena_Pool)
	xar.init(&pool.slots)
	for _ in 0 ..< initial_slots {
		slot := arena_slot_create(pool)
		xar.append(&pool.slots, slot)
	}

	return pool
}

arena_pool_deinit :: proc(pool: ^Arena_Pool) {
	if sync.mutex_guard(&pool.mutex) {
		it := xar.iterator(&pool.slots)
		for slot in xar.iterate_by_val(&it) {
			virtual.arena_destroy(&slot.arena)
			free(slot)
		}
		xar.destroy(&pool.slots)
	}

	free(pool)
}

arena_slot_acquire :: proc(pool: ^Arena_Pool) -> ^Arena_Slot {
	if sync.mutex_guard(&pool.mutex) {
		it := xar.iterator(&pool.slots)
		for slot in xar.iterate_by_val(&it) {
			if !slot.in_use {
				slot.in_use = true
				return slot
			}
		}

		slot := arena_slot_create(pool)
		slot.in_use = true
		xar.append(&pool.slots, slot)
		return slot
	}

	return nil
}

arena_slot_release :: proc(slot: ^Arena_Slot) {
	pool := slot.pool
	if sync.mutex_guard(&pool.mutex) {
		if !slot.in_use {
			return
		}
		virtual.arena_free_all(&slot.arena)
		slot.in_use = false
	}
}

arena_slot_create :: proc(pool: ^Arena_Pool) -> ^Arena_Slot {
	slot := new(Arena_Slot)
	slot.pool = pool
	_ = virtual.arena_init_growing(&slot.arena)
	slot.allocator = virtual.arena_allocator(&slot.arena)
	return slot
}
