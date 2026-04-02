package thread_pool

import "core:mem"
import "core:sync"
import sysinfo "core:sys/info"
import "core:thread"

Work_Proc :: #type proc(data: rawptr)
Completion_Proc :: #type proc(data: rawptr)

Completion_Item :: struct {
	procedure: Completion_Proc,
	data:      rawptr,
}

Thread_Pool :: struct {
	allocator:       mem.Allocator,
	worker_count:    int,
	pool:            thread.Pool,
	completion_lock: sync.Mutex,
	completions:     [dynamic]Completion_Item,
}

Submitted_Work :: struct {
	owner:           ^Thread_Pool,
	work:            Work_Proc,
	work_data:       rawptr,
	completion:      Completion_Proc,
	completion_data: rawptr,
}

recommended_worker_count :: proc() -> int {
	return max(sysinfo.cpu.logical_cores - 1, 1)
}

init :: proc(worker_count: int, allocator := context.allocator) -> ^Thread_Pool {
	pool := new(Thread_Pool, allocator)
	pool.allocator = allocator
	pool.worker_count = max(worker_count, 1)
	pool.completions = make([dynamic]Completion_Item, allocator)
	thread.pool_init(&pool.pool, allocator, pool.worker_count)
	thread.pool_start(&pool.pool)
	return pool
}

deinit :: proc(pool: ^Thread_Pool) {
	thread.pool_join(&pool.pool)
	thread.pool_destroy(&pool.pool)
	delete(pool.completions)
	free(pool, pool.allocator)
}

submit :: proc(
	pool: ^Thread_Pool,
	work: Work_Proc,
	work_data: rawptr,
	completion: Completion_Proc = nil,
	completion_data: rawptr = nil,
) {
	submitted := new(Submitted_Work, pool.allocator)
	submitted.owner = pool
	submitted.work = work
	submitted.work_data = work_data
	submitted.completion = completion
	submitted.completion_data = completion_data
	thread.pool_add_task(&pool.pool, pool.allocator, run_submitted_work, submitted)
}

run_pending_completions :: proc(pool: ^Thread_Pool) -> int {
	pending := make([dynamic]Completion_Item, context.temp_allocator)
	if sync.guard(&pool.completion_lock) {
		for item in pool.completions {
			append(&pending, item)
		}
		clear(&pool.completions)
	}

	for item in pending {
		if item.procedure != nil {
			item.procedure(item.data)
		}
	}

	return len(pending)
}

@(private = "file")
run_submitted_work :: proc(task: thread.Task) {
	submitted := cast(^Submitted_Work)task.data
	defer free(submitted, submitted.owner.allocator)

	submitted.work(submitted.work_data)

	if submitted.completion == nil {
		return
	}

	if sync.guard(&submitted.owner.completion_lock) {
		append(
			&submitted.owner.completions,
			Completion_Item{procedure = submitted.completion, data = submitted.completion_data},
		)
	}
}
