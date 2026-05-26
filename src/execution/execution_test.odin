package abap_frontend_execution

import base_runtime "base:runtime"
import "core:sync"
import "core:testing"
import "core:thread"

inc :: proc(v: int) -> int {
	return v + 1
}

square :: proc(v: int) -> int {
	return v * v
}

sum_values :: proc(values: []int) -> int {
	total := 0
	for value in values {
		total += value
	}
	return total
}

worker_is_active :: proc(_: int) -> bool {
	return current_pool() != nil && current_temp_arena() != nil
}

temp_arena_is_active :: proc(_: int) -> bool {
	return current_temp_arena() != nil
}

alloc_temp_value :: proc(v: int) -> int {
	bytes := make([]byte, 16, context.temp_allocator)
	bytes[0] = byte(v)
	return int(bytes[0])
}

nested_wait_work :: proc(v: int) -> int {
	pool := current_pool()
	graph: Graph
	graph_init(&graph, pool, context.allocator)
	defer graph_destroy(&graph)

	task := submit_value(&graph, worker_executor(pool), v + 1, square)
	graph_start(&graph)
	result := wait(task)
	return result + 1
}

Main_Update :: struct {
	value: ^int,
}

Graph_Payload :: struct {
	value: int,
}

Large_Data :: struct {
	values: [512]int,
}

store_main_value :: proc(v: int, update: Main_Update) -> No_Result {
	update.value^ = v
	return {}
}

read_graph_payload :: proc(payload: ^Graph_Payload) -> int {
	return payload.value + 1
}

large_roundtrip :: proc(data: Large_Data) -> Large_Data {
	result := data
	result.values[511] += 1
	return result
}

record_detached_value :: proc(v: int, out: ^int) -> No_Result {
	sync.atomic_store_explicit(out, v, .Release)
	return {}
}

roots_do_not_run_before_graph_start :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 8, edge_capacity = 8}, context.allocator)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	task := submit_value(&graph, worker_executor(&pool), 1, inc)
	_, ready := try_wait(task)
	testing.expect(t, !ready)

	graph_start(&graph)
	value := wait(task)
	testing.expect_value(t, value, 2)
}

empty_graph_start_completes :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 8, edge_capacity = 8}, context.allocator)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	graph_start(&graph)
	graph_wait(&graph)
}

independent_roots_run_and_results_are_waitable :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 2, task_capacity = 16, queue_capacity = 8, deque_capacity = 8, edge_capacity = 8}, context.allocator)
	pool_start(&pool)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	tasks: [4]Task(int)
	for i in 0 ..< len(tasks) {
		tasks[i] = submit_value(&graph, worker_executor(&pool), i, inc)
	}
	graph_start(&graph)
	total := 0
	for task in tasks {
		total += wait(task)
	}
	testing.expect_value(t, total, 10)
	graph_wait(&graph)
	pool_join(&pool)
}

stats_track_graph_work :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 8, edge_capacity = 8}, context.allocator)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	for i in 0 ..< 4 {
		_ = submit_value(&graph, worker_executor(&pool), i, inc)
	}
	graph_start(&graph)
	graph_wait(&graph)
	stats := pool_stats(&pool)
	testing.expect_value(t, stats.submitted, 4)
	testing.expect_value(t, stats.completed, 4)
	testing.expect_value(t, stats.outstanding, 0)
}

capacities_are_normalized :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 3, queue_capacity = 5, deque_capacity = 6, edge_capacity = 7}, context.allocator)
	defer pool_destroy(&pool)

	testing.expect_value(t, pool.options.task_capacity, 4)
	testing.expect_value(t, pool.options.queue_capacity, 8)
	testing.expect_value(t, pool.options.deque_capacity, 8)
	testing.expect_value(t, pool.options.edge_capacity, 8)

	main: Main_Executor
	main_executor_init(&main, 3, context.allocator)
	defer main_executor_destroy(&main)
	testing.expect_value(t, len(main.buffer), 4)
}

task_and_edge_storage_grows_past_initial_capacity :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 1, edge_capacity = 1}, context.allocator)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	parents: [4]Task(int)
	for i in 0 ..< len(parents) {
		parents[i] = submit_value(&graph, worker_executor(&pool), i, inc)
	}
	total_task := then_all(&graph, parents[:], worker_executor(&pool), sum_values)
	graph_start(&graph)
	testing.expect_value(t, wait(total_task), 10)
	graph_wait(&graph)
}

graph_arena_payload_survives_until_destroy :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 8, edge_capacity = 8}, context.allocator)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	payload := new(Graph_Payload, graph.allocator)
	payload.value = 41
	task := submit_value(&graph, worker_executor(&pool), payload, read_graph_payload)
	graph_start(&graph)
	testing.expect_value(t, wait(task), 42)
}

large_payload_and_result_use_graph_arena :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 8, edge_capacity = 8}, context.allocator)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	input: Large_Data
	input.values[0] = 7
	input.values[511] = 35
	task := submit_value(&graph, worker_executor(&pool), input, large_roundtrip)
	graph_start(&graph)
	value := wait(task)
	testing.expect_value(t, value.values[0], 7)
	testing.expect_value(t, value.values[511], 36)
}

then_preserves_sequential_dependency :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 8, edge_capacity = 8}, context.allocator)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	root := submit_value(&graph, worker_executor(&pool), 2, inc)
	next := then(&graph, root, worker_executor(&pool), square)
	graph_start(&graph)
	value := wait(next)
	testing.expect_value(t, value, 9)
}

then_all_runs_after_all_parents_complete :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 2, task_capacity = 16, queue_capacity = 8, deque_capacity = 8, edge_capacity = 16}, context.allocator)
	pool_start(&pool)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	parents: [4]Task(int)
	for i in 0 ..< len(parents) {
		parents[i] = submit_value(&graph, worker_executor(&pool), i, inc)
	}
	total_task := then_all(&graph, parents[:], worker_executor(&pool), sum_values)
	graph_start(&graph)
	value := wait(total_task)
	testing.expect_value(t, value, 10)
	pool_join(&pool)
}

one_parent_can_feed_multiple_children :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 8, edge_capacity = 8}, context.allocator)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	root := submit_value(&graph, worker_executor(&pool), 3, inc)
	a := then(&graph, root, worker_executor(&pool), inc)
	b := then(&graph, root, worker_executor(&pool), square)

	graph_start(&graph)
	value_a := wait(a)
	value_b := wait(b)
	testing.expect_value(t, value_a, 5)
	testing.expect_value(t, value_b, 16)
}

main_executor_runs_only_when_drained :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 1, task_capacity = 8, queue_capacity = 8, deque_capacity = 8, edge_capacity = 8}, context.allocator)
	pool_start(&pool)
	defer pool_destroy(&pool)

	main: Main_Executor
	main_executor_init(&main, 8, context.allocator)
	defer main_executor_destroy(&main)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	updated := 0
	root := submit_value(&graph, worker_executor(&pool), 4, square)
	final := then_with(&graph, root, main_executor(&main), Main_Update{value = &updated}, store_main_value)

	graph_start(&graph)
	drained := 0
	for i := 0; i < 1000 && drained == 0; i += 1 {
		drained = main_executor_drain(&main)
		if drained == 0 {
			thread.yield()
		}
	}
	testing.expect_value(t, drained, 1)
	value := wait(final)
	testing.expect_value(t, value, No_Result{})
	testing.expect_value(t, updated, 16)
	pool_join(&pool)
}

main_executor_temp_allocator_is_active_and_reset :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 0, task_capacity = 8, edge_capacity = 8}, context.allocator)
	defer pool_destroy(&pool)

	main: Main_Executor
	main_executor_init(&main, 8, context.allocator)
	defer main_executor_destroy(&main)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	task := submit_value(&graph, main_executor(&main), 0, temp_arena_is_active)
	value_task := submit_value(&graph, main_executor(&main), 7, alloc_temp_value)
	graph_start(&graph)
	testing.expect_value(t, main_executor_drain(&main), 2)
	testing.expect(t, wait(task))
	testing.expect_value(t, wait(value_task), 7)
	testing.expect_value(t, main.temp_arena.total_used, uint(0))
}

detached_graph_completes_without_coordinator_wait :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 1, task_capacity = 8, queue_capacity = 8, deque_capacity = 8, edge_capacity = 8}, context.allocator)
	pool_start(&pool)
	defer pool_destroy(&pool)

	graph := graph_create(&pool, base_runtime.heap_allocator())
	out := 0
	root := submit_value(graph, worker_executor(&pool), 9, inc)
	_ = then_with(graph, root, worker_executor(&pool), &out, record_detached_value)
	graph_detach(graph)

	for i := 0; i < 100000 && sync.atomic_load_explicit(&out, .Acquire) == 0; i += 1 {
		thread.yield()
	}
	testing.expect_value(t, out, 10)
	pool_join(&pool)
}

worker_temp_allocator_is_active_and_reset :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 1, task_capacity = 8, queue_capacity = 8, deque_capacity = 8, edge_capacity = 8}, context.allocator)
	pool_start(&pool)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	active := submit_value(&graph, worker_executor(&pool), 0, worker_is_active)
	value_task := submit_value(&graph, worker_executor(&pool), 7, alloc_temp_value)
	graph_start(&graph)
	active_value := wait(active)
	value := wait(value_task)
	testing.expect(t, active_value)
	testing.expect_value(t, value, 7)
	testing.expect_value(t, pool.workers[0].temp_arena.total_used, uint(0))
	pool_join(&pool)
}

nested_worker_wait_helps_without_deadlock :: proc(t: ^testing.T) {
	pool: Pool
	pool_init(&pool, Options{worker_count = 1, task_capacity = 16, queue_capacity = 8, deque_capacity = 8, edge_capacity = 8}, context.allocator)
	pool_start(&pool)
	defer pool_destroy(&pool)

	graph: Graph
	graph_init(&graph, &pool, context.allocator)
	defer graph_destroy(&graph)

	task := submit_value(&graph, worker_executor(&pool), 4, nested_wait_work)
	graph_start(&graph)
	value := wait(task)
	testing.expect_value(t, value, 26)
	pool_join(&pool)
}

@(test)
execution_graph_behaviors :: proc(t: ^testing.T) {
	roots_do_not_run_before_graph_start(t)
	empty_graph_start_completes(t)
	independent_roots_run_and_results_are_waitable(t)
	stats_track_graph_work(t)
	capacities_are_normalized(t)
	task_and_edge_storage_grows_past_initial_capacity(t)
	graph_arena_payload_survives_until_destroy(t)
	large_payload_and_result_use_graph_arena(t)
	then_preserves_sequential_dependency(t)
	then_all_runs_after_all_parents_complete(t)
	one_parent_can_feed_multiple_children(t)
	main_executor_runs_only_when_drained(t)
	main_executor_temp_allocator_is_active_and_reset(t)
	detached_graph_completes_without_coordinator_wait(t)
	worker_temp_allocator_is_active_and_reset(t)
	nested_worker_wait_helps_without_deadlock(t)
}
