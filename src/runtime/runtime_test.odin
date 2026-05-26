package abap_frontend_runtime

import "core:nbio"
import "core:sync"
import "core:testing"

square :: proc(v: int) -> int {
	return v * v
}

inc :: proc(v: int) -> int {
	return v + 1
}

sum_values :: proc(values: []int) -> int {
	total := 0
	for value in values {
		total += value
	}
	return total
}

add_one_ptr :: proc(data: rawptr) {
	p := cast(^int)data
	sync.atomic_add_explicit(p, 1, .Relaxed)
}

@(test)
auto_worker_count_uses_hardware_limit :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = AUTO_WORKER_COUNT, task_capacity = 8}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	testing.expect_value(t, pool.options.worker_count, recommended_worker_count())
}

@(test)
explicit_worker_count_is_clamped_to_hardware_limit :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = recommended_worker_count() + 8, task_capacity = 8}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	testing.expect_value(t, pool.options.worker_count, recommended_worker_count())
}

@(test)
spsc_ring_batches_until_flush :: proc(t: ^testing.T) {
	buffer: [4]int
	ring: Spsc_Ring(int)
	testing.expect(t, spsc_ring_init(&ring, buffer[:]))
	testing.expect_value(t, spsc_ring_enqueue(&ring, 10), Spsc_Result.Success)
	testing.expect_value(t, spsc_ring_enqueue(&ring, 20), Spsc_Result.Success)
	testing.expect_value(t, spsc_ring_available_to_read(&ring), u64(0))
	spsc_ring_flush_producer(&ring)
	testing.expect_value(t, spsc_ring_available_to_read(&ring), u64(2))
	testing.expect_value(t, spsc_ring_get_read_ptr(&ring, 0)^, 10)
	testing.expect_value(t, spsc_ring_get_read_ptr(&ring, 1)^, 20)
	spsc_ring_commit_read(&ring, 2)
	testing.expect_value(t, spsc_ring_available_to_read(&ring), u64(0))
}

@(test)
mpmc_index_ring_reports_full_and_empty :: proc(t: ^testing.T) {
	buffer: [2]Mpmc_Cell
	ring: Mpmc_Index_Ring
	testing.expect(t, mpmc_index_ring_init(&ring, buffer[:]))
	testing.expect(t, mpmc_index_ring_enqueue(&ring, 1))
	testing.expect(t, mpmc_index_ring_enqueue(&ring, 2))
	testing.expect(t, !mpmc_index_ring_enqueue(&ring, 3))
	v, ok := mpmc_index_ring_dequeue(&ring)
	testing.expect(t, ok)
	testing.expect_value(t, v, u32(1))
	v, ok = mpmc_index_ring_dequeue(&ring)
	testing.expect(t, ok)
	testing.expect_value(t, v, u32(2))
	_, ok = mpmc_index_ring_dequeue(&ring)
	testing.expect(t, !ok)
}

@(test)
work_deque_owner_pop_and_steal :: proc(t: ^testing.T) {
	buffer: [4]u32
	deque: Work_Deque
	testing.expect(t, work_deque_init(&deque, buffer[:]))
	testing.expect(t, work_deque_push(&deque, 11))
	testing.expect(t, work_deque_push(&deque, 12))
	stolen, ok := work_deque_steal(&deque)
	testing.expect(t, ok)
	testing.expect_value(t, stolen, u32(11))
	popped, popped_ok := work_deque_pop(&deque)
	testing.expect(t, ok)
	testing.expect(t, popped_ok)
	testing.expect_value(t, popped, u32(12))
}

@(test)
inline_submit_waits_for_value :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 0, task_capacity = 8}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	task, err := submit_value(&pool, 7, square)
	testing.expect_value(t, err, Submit_Error.None)
	value, wait_err := wait(task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect_value(t, value, 49)
}

@(test)
payload_is_copied_before_caller_mutates_it :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 0, task_capacity = 8}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	value := 8
	task, err := submit_value(&pool, value, square)
	value = 99
	testing.expect_value(t, err, Submit_Error.None)
	result, wait_err := wait(task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect_value(t, result, 64)
}

Pair :: struct {
	a: int,
	b: int,
}

pair_sum :: proc(p: Pair) -> int {
	return p.a + p.b
}

@(test)
struct_payload_crosses_task_boundary :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 2, task_capacity = 32, queue_capacity = 8, deque_capacity = 8}, context.allocator), Submit_Error.None)
	testing.expect_value(t, pool_start(&pool), Submit_Error.None)
	defer pool_destroy(&pool)

	task, err := submit_value(&pool, Pair{a = 3, b = 4}, pair_sum)
	testing.expect_value(t, err, Submit_Error.None)
	value, wait_err := wait(task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect_value(t, value, 7)
	pool_join(&pool)
}

nested_work :: proc(v: int) -> int {
	pool := current_pool()
	task, err := submit_value(pool, v + 1, square)
	if err != .None {
		return -1
	}
	result, wait_err := wait(task)
	if wait_err != .None {
		return -2
	}
	return result + 1
}

worker_temp_allocator_is_active :: proc(_: int) -> bool {
	pool := current_pool()
	return pool != nil &&
	       len(pool.workers) == 1 &&
	       context.temp_allocator.data == pool.workers[0].temp_allocator.data
}

alloc_temp_value :: proc(v: int) -> int {
	bytes := make([]byte, 16, context.temp_allocator)
	bytes[0] = byte(v)
	return int(bytes[0])
}

worker_temp_survives_nested_wait :: proc(v: int) -> bool {
	bytes := make([]byte, 16, context.temp_allocator)
	bytes[0] = 123
	task, err := submit_value(current_pool(), v, alloc_temp_value)
	if err != .None {
		return false
	}
	nested, wait_err := wait(task)
	return wait_err == .None && nested == v && bytes[0] == 123
}

@(test)
threaded_worker_uses_task_temp_allocator :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 1, task_capacity = 16, queue_capacity = 8, deque_capacity = 8}, context.allocator), Submit_Error.None)
	testing.expect_value(t, pool_start(&pool), Submit_Error.None)
	defer pool_destroy(&pool)

	task, err := submit_value(&pool, 0, worker_temp_allocator_is_active)
	testing.expect_value(t, err, Submit_Error.None)
	value, wait_err := wait(task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect(t, value)
	testing.expect_value(t, pool.workers[0].temp_arena.total_used, uint(0))
	pool_join(&pool)
}

@(test)
worker_temp_reset_waits_for_outer_task :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 1, task_capacity = 32, queue_capacity = 8, deque_capacity = 8}, context.allocator), Submit_Error.None)
	testing.expect_value(t, pool_start(&pool), Submit_Error.None)
	defer pool_destroy(&pool)

	task, err := submit_value(&pool, 7, worker_temp_survives_nested_wait)
	testing.expect_value(t, err, Submit_Error.None)
	value, wait_err := wait(task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect(t, value)
	testing.expect_value(t, pool.workers[0].temp_arena.total_used, uint(0))
	pool_join(&pool)
}

@(test)
nested_worker_submit_wait_does_not_deadlock :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 1, task_capacity = 32, queue_capacity = 8, deque_capacity = 8}, context.allocator), Submit_Error.None)
	testing.expect_value(t, pool_start(&pool), Submit_Error.None)
	defer pool_destroy(&pool)

	task, err := submit_value(&pool, 4, nested_work)
	testing.expect_value(t, err, Submit_Error.None)
	value, wait_err := wait(task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect_value(t, value, 26)
	pool_join(&pool)
}

@(test)
continuation_chain_runs_in_order :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 0, task_capacity = 16}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	task, err := submit_value(&pool, 1, inc)
	testing.expect_value(t, err, Submit_Error.None)
	next, err2 := then(task, inc)
	testing.expect_value(t, err2, Submit_Error.None)
	final_task, err3 := then(next, inc)
	testing.expect_value(t, err3, Submit_Error.None)
	value, wait_err := wait(final_task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect_value(t, value, 4)
}

@(test)
then_all_runs_after_all_parent_tasks :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 0, task_capacity = 16}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	tasks: [4]Task(int)
	for i in 0 ..< len(tasks) {
		task, err := submit_value(&pool, i, inc)
		testing.expect_value(t, err, Submit_Error.None)
		tasks[i] = task
	}

	all_task, err := then_all(&pool, tasks[:], sum_values)
	testing.expect_value(t, err, Submit_Error.None)
	value, wait_err := wait(all_task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect_value(t, value, 10)
	testing.expect_value(t, pool_stats(&pool).outstanding, u64(0))
}

@(test)
then_all_works_with_threaded_parents :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 4, task_capacity = 64, queue_capacity = 16, deque_capacity = 16}, context.allocator), Submit_Error.None)
	testing.expect_value(t, pool_start(&pool), Submit_Error.None)
	defer pool_destroy(&pool)

	tasks: [16]Task(int)
	for i in 0 ..< len(tasks) {
		task, err := submit_value(&pool, i, inc)
		testing.expect_value(t, err, Submit_Error.None)
		tasks[i] = task
	}

	all_task, err := then_all(&pool, tasks[:], sum_values)
	testing.expect_value(t, err, Submit_Error.None)
	value, wait_err := wait(all_task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect_value(t, value, 136)
	testing.expect_value(t, pool_stats(&pool).outstanding, u64(0))
	pool_join(&pool)
}

@(test)
bounded_task_slots_fail_fast :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 0, task_capacity = 1}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	_, _, err := make_deferred(&pool, int)
	testing.expect_value(t, err, Submit_Error.None)
	_, err2 := try_submit_value(&pool, 1, inc)
	testing.expect_value(t, err2, Submit_Error.No_Task_Slots)
}

@(test)
deferred_completion_wakes_wait :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 0, task_capacity = 8}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	task, completer, err := make_deferred(&pool, int)
	testing.expect_value(t, err, Submit_Error.None)
	testing.expect_value(t, complete(completer, 42), Complete_Error.None)
	value, wait_err := wait(task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect_value(t, value, 42)
}

complete_from_nbio :: proc(op: ^nbio.Operation, completer: Completer(int)) {
	_ = complete(completer, 77)
	_ = op
}

store_nbio_result :: proc(value: int, data: rawptr) {
	out := cast(^int)data
	out^ = value
}

@(test)
nbio_callback_completes_deferred_task :: proc(t: ^testing.T) {
	if nbio.acquire_thread_event_loop() != nil {
		return
	}
	defer nbio.release_thread_event_loop()

	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 0, task_capacity = 8}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	task, completer, err := make_deferred(&pool, int)
	testing.expect_value(t, err, Submit_Error.None)
	nbio.next_tick_poly(completer, complete_from_nbio)
	_ = nbio.tick(0)
	value, wait_err := wait(task)
	testing.expect_value(t, wait_err, Wait_Error.None)
	testing.expect_value(t, value, 77)
}

@(test)
then_on_nbio_posts_cpu_result_to_event_loop :: proc(t: ^testing.T) {
	if nbio.acquire_thread_event_loop() != nil {
		return
	}
	defer nbio.release_thread_event_loop()

	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 0, task_capacity = 8}, context.allocator), Submit_Error.None)
	defer pool_destroy(&pool)

	result := 0
	task, err := submit_value(&pool, 6, square)
	testing.expect_value(t, err, Submit_Error.None)
	testing.expect_value(t, then_on_nbio(task, nbio.current_thread_event_loop(), store_nbio_result, &result), Submit_Error.None)
	_ = nbio.tick(0)
	testing.expect_value(t, result, 36)
	testing.expect_value(t, pool_stats(&pool).outstanding, u64(0))
}

@(test)
stress_many_threaded_tasks_complete :: proc(t: ^testing.T) {
	pool: Pool
	testing.expect_value(t, pool_init(&pool, Options{worker_count = 4, task_capacity = 256, queue_capacity = 64, deque_capacity = 64}, context.allocator), Submit_Error.None)
	testing.expect_value(t, pool_start(&pool), Submit_Error.None)
	defer pool_destroy(&pool)

	tasks: [128]Task(int)
	for i in 0 ..< len(tasks) {
		task, err := submit_value(&pool, i, inc)
		testing.expect_value(t, err, Submit_Error.None)
		tasks[i] = task
	}
	total := 0
	for task in tasks {
		value, wait_err := wait(task)
		testing.expect_value(t, wait_err, Wait_Error.None)
		total += value
	}
	testing.expect_value(t, total, 8256)
	pool_join(&pool)
}
