package cache

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
	slots: [dynamic]^Arena_Slot,
}

arena_pool_init :: proc(initial_slots: int) -> ^Arena_Pool {
	pool := new(Arena_Pool)
	pool.slots = make([dynamic]^Arena_Slot, 0, max(initial_slots, 0))

	for _ in 0 ..< initial_slots {
		append(&pool.slots, arena_slot_create(pool))
	}

	return pool
}

arena_pool_deinit :: proc(pool: ^Arena_Pool) {
	if pool == nil {
		return
	}

	if sync.mutex_guard(&pool.mutex) {
		for slot in pool.slots {
			virtual.arena_destroy(&slot.arena)
			free(slot)
		}
		delete(pool.slots)
	}

	free(pool)
}

arena_slot_acquire :: proc(pool: ^Arena_Pool) -> ^Arena_Slot {
	if pool == nil {
		return nil
	}

	if sync.mutex_guard(&pool.mutex) {
		for slot in pool.slots {
			if !slot.in_use {
				slot.in_use = true
				return slot
			}
		}

		slot := arena_slot_create(pool)
		slot.in_use = true
		append(&pool.slots, slot)
		return slot
	}

	return nil
}

arena_slot_release :: proc(slot: ^Arena_Slot) {
	if slot == nil || slot.pool == nil {
		return
	}

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
