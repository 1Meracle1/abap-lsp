package main

import "../../src/runtime"

import base_runtime "base:runtime"
import "core:fmt"
import "core:thread"
import "core:time"

bench_inc :: proc(v: int) -> int {
	return v + 1
}

core_task :: proc(task: thread.Task) {
	p := cast(^int)task.data
	p^ += 1
}

run_runtime_noop :: proc(count: int, workers: int) {
	fmt.printfln("starting runtime noop")
	allocator := base_runtime.heap_allocator()
	pool: runtime.Pool
	err := runtime.pool_init(&pool, runtime.Options{worker_count = workers, task_capacity = 32768, queue_capacity = 4096, deque_capacity = 4096}, allocator)
	if err != .None {
		fmt.printfln("runtime init failed: %v", err)
		return
	}
	_ = runtime.pool_start(&pool)
	defer runtime.pool_destroy(&pool)

	start := time.tick_now()
	tasks := make([]runtime.Task(int), count, allocator)
	defer delete(tasks, allocator)
	for i in 0 ..< count {
		task, submit_err := runtime.submit_value(&pool, i, bench_inc)
		if submit_err != .None {
			fmt.printfln("runtime submit failed: %v", submit_err)
			return
		}
		tasks[i] = task
	}
	sum := 0
	for task in tasks {
		value, wait_err := runtime.wait(task)
		if wait_err != .None {
			fmt.printfln("runtime wait failed: %v", wait_err)
			return
		}
		sum += value
	}
	elapsed := time.tick_since(start)
	stats := runtime.pool_stats(&pool)
	fmt.printfln(
		"runtime noop: count=%d sum=%d seconds=%.6f tasks_per_sec=%.0f steals=%d queue_full=%d completed=%d",
		count,
		sum,
		time.duration_seconds(elapsed),
		f64(count) / max(time.duration_seconds(elapsed), 0.000001),
		stats.steals,
		stats.queue_full,
		stats.completed,
	)
	runtime.pool_join(&pool)
}

run_core_thread_pool_noop :: proc(count: int, workers: int) {
	fmt.printfln("starting core thread.Pool noop")
	allocator := base_runtime.heap_allocator()
	pool: thread.Pool
	thread.pool_init(&pool, allocator, workers)
	thread.pool_start(&pool)
	defer thread.pool_destroy(&pool)

	values := make([]int, count, allocator)
	defer delete(values, allocator)
	start := time.tick_now()
	for i in 0 ..< count {
		thread.pool_add_task(&pool, allocator, core_task, &values[i])
	}
	for thread.pool_num_outstanding(&pool) > 0 {
		thread.yield()
	}
	elapsed := time.tick_since(start)
	sum := 0
	for value in values {
		sum += value
	}
	fmt.printfln(
		"core thread.Pool noop: count=%d sum=%d seconds=%.6f tasks_per_sec=%.0f completed=%d",
		count,
		sum,
		time.duration_seconds(elapsed),
		f64(count) / max(time.duration_seconds(elapsed), 0.000001),
		thread.pool_num_done(&pool),
	)
	thread.pool_join(&pool)
}

main :: proc() {
	count := 5000
	workers := 4
	run_runtime_noop(count, workers)
	run_core_thread_pool_noop(count, workers)
}
