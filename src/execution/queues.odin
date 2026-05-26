package abap_frontend_execution

import "core:sync"

CACHE_LINE_SIZE :: 128
INDEX_NONE :: u32(0xFFFF_FFFF)

is_power_of_two :: proc(n: int) -> bool {
	return n > 0 && (n & (n - 1)) == 0
}

Mpmc_Cell :: struct {
	sequence: u64,
	value:    u32,
}

Mpmc_Index_Ring :: struct #align(CACHE_LINE_SIZE) {
	enqueue_pos: u64,
	_padding0:   [120]u8,
	dequeue_pos: u64,
	_padding1:   [120]u8,
	mask:        u64,
	buffer:      []Mpmc_Cell,
}

mpmc_index_ring_init :: proc(ring: ^Mpmc_Index_Ring, buffer: []Mpmc_Cell) {
	assert(is_power_of_two(len(buffer)))
	ring.enqueue_pos = 0
	ring.dequeue_pos = 0
	ring.mask = u64(len(buffer) - 1)
	ring.buffer = buffer
	for i in 0 ..< len(buffer) {
		buffer[i].sequence = u64(i)
		buffer[i].value = INDEX_NONE
	}
}

mpmc_index_ring_enqueue :: proc "contextless" (ring: ^Mpmc_Index_Ring, value: u32) -> bool {
	pos := sync.atomic_load_explicit(&ring.enqueue_pos, .Relaxed)
	for {
		cell := &ring.buffer[pos & ring.mask]
		seq := sync.atomic_load_explicit(&cell.sequence, .Acquire)
		diff := i64(seq) - i64(pos)
		if diff == 0 {
			if _, ok := sync.atomic_compare_exchange_weak_explicit(&ring.enqueue_pos, pos, pos + 1, .Relaxed, .Relaxed); ok {
				cell.value = value
				sync.atomic_store_explicit(&cell.sequence, pos + 1, .Release)
				return true
			}
			pos = sync.atomic_load_explicit(&ring.enqueue_pos, .Relaxed)
		} else if diff < 0 {
			return false
		} else {
			pos = sync.atomic_load_explicit(&ring.enqueue_pos, .Relaxed)
		}
	}
}

mpmc_index_ring_dequeue :: proc "contextless" (ring: ^Mpmc_Index_Ring) -> (u32, bool) {
	pos := sync.atomic_load_explicit(&ring.dequeue_pos, .Relaxed)
	for {
		cell := &ring.buffer[pos & ring.mask]
		seq := sync.atomic_load_explicit(&cell.sequence, .Acquire)
		diff := i64(seq) - i64(pos + 1)
		if diff == 0 {
			if _, ok := sync.atomic_compare_exchange_weak_explicit(&ring.dequeue_pos, pos, pos + 1, .Relaxed, .Relaxed); ok {
				value := cell.value
				sync.atomic_store_explicit(&cell.sequence, pos + ring.mask + 1, .Release)
				return value, true
			}
			pos = sync.atomic_load_explicit(&ring.dequeue_pos, .Relaxed)
		} else if diff < 0 {
			return INDEX_NONE, false
		} else {
			pos = sync.atomic_load_explicit(&ring.dequeue_pos, .Relaxed)
		}
	}
}

Work_Deque :: struct #align(CACHE_LINE_SIZE) {
	top:       i64,
	_padding0: [120]u8,
	bottom:    i64,
	_padding1: [120]u8,
	mask:      i64,
	buffer:    []u32,
}

work_deque_init :: proc(deque: ^Work_Deque, buffer: []u32) {
	assert(is_power_of_two(len(buffer)))
	deque.top = 0
	deque.bottom = 0
	deque.mask = i64(len(buffer) - 1)
	deque.buffer = buffer
	for i in 0 ..< len(buffer) {
		buffer[i] = INDEX_NONE
	}
}

work_deque_push :: proc "contextless" (deque: ^Work_Deque, value: u32) -> bool {
	bottom := sync.atomic_load_explicit(&deque.bottom, .Relaxed)
	top := sync.atomic_load_explicit(&deque.top, .Acquire)
	if bottom - top >= i64(len(deque.buffer)) {
		return false
	}
	deque.buffer[bottom & deque.mask] = value
	sync.atomic_store_explicit(&deque.bottom, bottom + 1, .Release)
	return true
}

work_deque_pop :: proc "contextless" (deque: ^Work_Deque) -> (u32, bool) {
	bottom := sync.atomic_load_explicit(&deque.bottom, .Relaxed) - 1
	sync.atomic_store_explicit(&deque.bottom, bottom, .Relaxed)
	top := sync.atomic_load_explicit(&deque.top, .Acquire)

	if top <= bottom {
		value := deque.buffer[bottom & deque.mask]
		if top == bottom {
			if _, ok := sync.atomic_compare_exchange_strong_explicit(&deque.top, top, top + 1, .Release, .Relaxed); !ok {
				sync.atomic_store_explicit(&deque.bottom, bottom + 1, .Relaxed)
				return INDEX_NONE, false
			}
			sync.atomic_store_explicit(&deque.bottom, bottom + 1, .Relaxed)
		}
		return value, true
	}

	sync.atomic_store_explicit(&deque.bottom, bottom + 1, .Relaxed)
	return INDEX_NONE, false
}

work_deque_steal :: proc "contextless" (deque: ^Work_Deque) -> (u32, bool) {
	top := sync.atomic_load_explicit(&deque.top, .Acquire)
	bottom := sync.atomic_load_explicit(&deque.bottom, .Acquire)
	if top >= bottom {
		return INDEX_NONE, false
	}
	value := deque.buffer[top & deque.mask]
	if _, ok := sync.atomic_compare_exchange_strong_explicit(&deque.top, top, top + 1, .Release, .Relaxed); ok {
		return value, true
	}
	return INDEX_NONE, false
}

Main_Queue_Cell :: struct {
	sequence: u64,
	cell:     ^Task_Cell,
}

Main_Queue :: struct #align(CACHE_LINE_SIZE) {
	enqueue_pos: u64,
	_padding0:   [120]u8,
	dequeue_pos: u64,
	_padding1:   [120]u8,
	mask:        u64,
	buffer:      []Main_Queue_Cell,
}

main_queue_init :: proc(queue: ^Main_Queue, buffer: []Main_Queue_Cell) {
	assert(is_power_of_two(len(buffer)))
	queue.enqueue_pos = 0
	queue.dequeue_pos = 0
	queue.mask = u64(len(buffer) - 1)
	queue.buffer = buffer
	for i in 0 ..< len(buffer) {
		buffer[i].sequence = u64(i)
		buffer[i].cell = nil
	}
}

main_queue_enqueue :: proc "contextless" (queue: ^Main_Queue, cell: ^Task_Cell) -> bool {
	pos := sync.atomic_load_explicit(&queue.enqueue_pos, .Relaxed)
	for {
		slot := &queue.buffer[pos & queue.mask]
		seq := sync.atomic_load_explicit(&slot.sequence, .Acquire)
		diff := i64(seq) - i64(pos)
		if diff == 0 {
			if _, ok := sync.atomic_compare_exchange_weak_explicit(&queue.enqueue_pos, pos, pos + 1, .Relaxed, .Relaxed); ok {
				slot.cell = cell
				sync.atomic_store_explicit(&slot.sequence, pos + 1, .Release)
				return true
			}
			pos = sync.atomic_load_explicit(&queue.enqueue_pos, .Relaxed)
		} else if diff < 0 {
			return false
		} else {
			pos = sync.atomic_load_explicit(&queue.enqueue_pos, .Relaxed)
		}
	}
}

main_queue_dequeue :: proc "contextless" (queue: ^Main_Queue) -> (^Task_Cell, bool) {
	pos := sync.atomic_load_explicit(&queue.dequeue_pos, .Relaxed)
	for {
		slot := &queue.buffer[pos & queue.mask]
		seq := sync.atomic_load_explicit(&slot.sequence, .Acquire)
		diff := i64(seq) - i64(pos + 1)
		if diff == 0 {
			if _, ok := sync.atomic_compare_exchange_weak_explicit(&queue.dequeue_pos, pos, pos + 1, .Relaxed, .Relaxed); ok {
				cell := slot.cell
				slot.cell = nil
				sync.atomic_store_explicit(&slot.sequence, pos + queue.mask + 1, .Release)
				return cell, true
			}
			pos = sync.atomic_load_explicit(&queue.dequeue_pos, .Relaxed)
		} else if diff < 0 {
			return nil, false
		} else {
			pos = sync.atomic_load_explicit(&queue.dequeue_pos, .Relaxed)
		}
	}
}
