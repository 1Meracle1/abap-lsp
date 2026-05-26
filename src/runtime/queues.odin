package abap_frontend_runtime

import "core:sync"

CACHE_LINE_SIZE :: 128
INDEX_NONE :: u32(0xFFFF_FFFF)

is_power_of_two :: proc(n: int) -> bool {
	return n > 0 && (n & (n - 1)) == 0
}

Spsc_Result :: enum u8 {
	Success,
	Full,
	Empty,
}

Spsc_Ring :: struct($T: typeid) #align(CACHE_LINE_SIZE) {
	write_sequence:        u64,
	local_write_sequence:  u64,
	cached_read_sequence:  u64,
	_padding0:             [104]u8,

	read_sequence:         u64,
	local_read_sequence:   u64,
	cached_write_sequence: u64,
	_padding1:             [104]u8,

	capacity:      u64,
	capacity_mask: u64,
	buffer:        [^]T,
}

spsc_ring_init :: proc(ring: ^Spsc_Ring($T), buffer: []T) -> bool {
	if !is_power_of_two(len(buffer)) {
		return false
	}
	ring.write_sequence = 0
	ring.local_write_sequence = 0
	ring.cached_read_sequence = 0
	ring.read_sequence = 0
	ring.local_read_sequence = 0
	ring.cached_write_sequence = 0
	ring.capacity = u64(len(buffer))
	ring.capacity_mask = ring.capacity - 1
	ring.buffer = raw_data(buffer)
	return true
}

spsc_ring_enqueue :: proc "contextless" (ring: ^Spsc_Ring($T), value: T) -> Spsc_Result {
	if ring.local_write_sequence - ring.cached_read_sequence >= ring.capacity {
		ring.cached_read_sequence = sync.atomic_load_explicit(&ring.read_sequence, .Acquire)
		if ring.local_write_sequence - ring.cached_read_sequence >= ring.capacity {
			return .Full
		}
	}

	index := ring.local_write_sequence & ring.capacity_mask
	ring.buffer[index] = value
	ring.local_write_sequence += 1
	return .Success
}

spsc_ring_flush_producer :: proc "contextless" (ring: ^Spsc_Ring($T)) {
	if ring.write_sequence != ring.local_write_sequence {
		sync.atomic_store_explicit(&ring.write_sequence, ring.local_write_sequence, .Release)
	}
}

spsc_ring_available_to_read :: proc "contextless" (ring: ^Spsc_Ring($T)) -> u64 {
	if ring.cached_write_sequence <= ring.local_read_sequence {
		ring.cached_write_sequence = sync.atomic_load_explicit(&ring.write_sequence, .Acquire)
	}
	return ring.cached_write_sequence - ring.local_read_sequence
}

spsc_ring_get_read_ptr :: proc "contextless" (ring: ^Spsc_Ring($T), offset: u64) -> ^T {
	index := (ring.local_read_sequence + offset) & ring.capacity_mask
	return &ring.buffer[index]
}

spsc_ring_commit_read :: proc "contextless" (ring: ^Spsc_Ring($T), count: u64) {
	if count == 0 {
		return
	}
	ring.local_read_sequence += count
	sync.atomic_store_explicit(&ring.read_sequence, ring.local_read_sequence, .Release)
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

mpmc_index_ring_init :: proc(ring: ^Mpmc_Index_Ring, buffer: []Mpmc_Cell) -> bool {
	if !is_power_of_two(len(buffer)) {
		return false
	}
	ring.enqueue_pos = 0
	ring.dequeue_pos = 0
	ring.mask = u64(len(buffer) - 1)
	ring.buffer = buffer
	for i in 0 ..< len(buffer) {
		buffer[i].sequence = u64(i)
		buffer[i].value = INDEX_NONE
	}
	return true
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
	top:      i64,
	_padding0: [120]u8,
	bottom:   i64,
	_padding1: [120]u8,
	mask:     i64,
	buffer:   []u32,
}

work_deque_init :: proc(deque: ^Work_Deque, buffer: []u32) -> bool {
	if !is_power_of_two(len(buffer)) {
		return false
	}
	deque.top = 0
	deque.bottom = 0
	deque.mask = i64(len(buffer) - 1)
	deque.buffer = buffer
	for i in 0 ..< len(buffer) {
		buffer[i] = INDEX_NONE
	}
	return true
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
				value = INDEX_NONE
				sync.atomic_store_explicit(&deque.bottom, bottom + 1, .Relaxed)
				return value, false
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
