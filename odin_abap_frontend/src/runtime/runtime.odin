package abap_frontend_runtime

import "base:intrinsics"
import "core:mem"
import "core:mem/virtual"
import "core:nbio"
import sysinfo "core:sys/info"
import "core:sync"
import "core:thread"

INLINE_BYTES_MAX :: 256
AUTO_WORKER_COUNT :: -1
DEFAULT_TASK_CAPACITY :: 1024
DEFAULT_QUEUE_CAPACITY :: 1024
DEFAULT_DEQUE_CAPACITY :: 1024

Task_Invoke_Proc :: #type proc(cell: ^Task_Cell)
Raw_Work_Proc :: #type proc(data: rawptr)

Task_State :: enum u32 {
	Free,
	Reserved,
	Queued,
	Running,
	Deferred,
	Completed,
	Consumed,
}

Task_Kind :: enum u8 {
	None,
	Value,
	Raw,
	Continuation,
	Nbio_Post,
	Deferred,
}

// No_Result is the result type used by raw tasks and continuations that only
// perform side effects.
No_Result :: struct {}

// Submit_Error reports immediate submission/backpressure failures. `.None`
// means the task handle is valid.
Submit_Error :: enum u8 {
	None,
	Invalid_Options,
	Pool_Shutting_Down,
	No_Task_Slots,
	Queue_Full,
	Payload_Too_Large,
	Result_Too_Large,
	Continuation_Already_Set,
	Invalid_Task,
}

// Wait_Error reports task-handle or completion failures. `.None` means the
// returned value is valid.
Wait_Error :: enum u8 {
	None,
	Invalid_Task,
	Not_Ready,
	Already_Consumed,
}

// Complete_Error reports deferred-completion failures. `.None` means the
// completer stored the value and woke any continuation.
Complete_Error :: enum u8 {
	None,
	Invalid_Task,
	Already_Completed,
	Result_Too_Large,
}

// Options fixes all runtime storage sizes up front. `worker_count = 0` runs
// inline; `worker_count = AUTO_WORKER_COUNT` uses available hardware parallelism.
// Queue, deque, and task capacities must be powers of two after defaults.
Options :: struct {
	worker_count:   int,
	task_capacity:  int,
	queue_capacity: int,
	deque_capacity: int,
	inline_bytes:   int,
}

// Stats is a snapshot of counters useful for tests and benchmarks.
Stats :: struct {
	submitted:       u64,
	completed:       u64,
	steals:          u64,
	steal_misses:    u64,
	queue_full:      u64,
	outstanding:     u64,
}

// Task is a typed, one-shot handle to a submitted or deferred result.
Task :: struct($T: typeid) {
	pool:       ^Pool,
	index:      u32,
	generation: u32,
}

// Completer is a typed, one-shot producer handle returned by `make_deferred`.
Completer :: struct($T: typeid) {
	pool:       ^Pool,
	index:      u32,
	generation: u32,
}

Task_Cell :: struct #align(CACHE_LINE_SIZE) {
	state:              Task_State,
	generation:         u32,
	kind:               Task_Kind,
	result_size:        u16,
	payload_size:       u16,
	parent_index:       u32,
	parent_generation:  u32,
	continuation_index: u32,
	pool:               ^Pool,
	invoke:             Task_Invoke_Proc,
	user_proc:          rawptr,
	user_data:          rawptr,
	nbio_loop:          ^nbio.Event_Loop,
	nbio_dispatch:      Task_Invoke_Proc,
	payload:            [INLINE_BYTES_MAX]u8,
	result:             [INLINE_BYTES_MAX]u8,
}

Worker :: struct {
	pool:           ^Pool,
	id:             int,
	thread_handle:  ^thread.Thread,
	deque:          Work_Deque,
	deque_buffer:   []u32,
	ingress:        Mpmc_Index_Ring,
	ingress_buffer: []Mpmc_Cell,
	temp_arena:     virtual.Arena,
	temp_allocator: mem.Allocator,
	temp_depth:     int,
}

// Pool owns a fixed task-cell slab, fixed queues, and optional worker threads.
// The address of a running pool must remain stable until `pool_destroy`.
Pool :: struct {
	allocator:        mem.Allocator,
	options:          Options,
	cells:            []Task_Cell,
	free_queue:       Mpmc_Index_Ring,
	free_buffer:      []Mpmc_Cell,
	workers:          []Worker,
	available:        sync.Sema,
	started:          bool,
	shutting_down:    bool,
	next_worker:      u64,
	submitted_count:  u64,
	completed_count:  u64,
	steal_count:      u64,
	steal_miss_count: u64,
	queue_full_count: u64,
	outstanding:      u64,
}

@(private = "file", thread_local)
current_worker: ^Worker

// pool_init allocates all fixed runtime storage with `allocator` and prepares
// the pool. Call `pool_start` before using worker threads.
pool_init :: proc(pool: ^Pool, options: Options, allocator: mem.Allocator) -> Submit_Error {
	opts := normalize_options(options)
	if !validate_options(opts) {
		return .Invalid_Options
	}

	pool^ = {}
	pool.allocator = allocator
	pool.options = opts
	pool.cells = make([]Task_Cell, opts.task_capacity, allocator)
	pool.free_buffer = make([]Mpmc_Cell, opts.task_capacity, allocator)
	if !mpmc_index_ring_init(&pool.free_queue, pool.free_buffer) {
		return .Invalid_Options
	}

	for i in 0 ..< len(pool.cells) {
		cell := &pool.cells[i]
		cell.state = .Free
		cell.generation = 1
		cell.continuation_index = INDEX_NONE
		_ = mpmc_index_ring_enqueue(&pool.free_queue, u32(i))
	}

	if opts.worker_count > 0 {
		pool.workers = make([]Worker, opts.worker_count, allocator)
		for i in 0 ..< len(pool.workers) {
			worker := &pool.workers[i]
			worker.pool = pool
			worker.id = i
			worker.deque_buffer = make([]u32, opts.deque_capacity, allocator)
			worker.ingress_buffer = make([]Mpmc_Cell, opts.queue_capacity, allocator)
			if virtual.arena_init_growing(&worker.temp_arena) != .None {
				return .Invalid_Options
			}
			worker.temp_allocator = virtual.arena_allocator(&worker.temp_arena)
			if !work_deque_init(&worker.deque, worker.deque_buffer) ||
			   !mpmc_index_ring_init(&worker.ingress, worker.ingress_buffer) {
				return .Invalid_Options
			}
		}
	}

	return .None
}

// current_pool returns the pool currently executing on this worker thread, or
// nil when called outside this runtime.
current_pool :: proc() -> ^Pool {
	if current_worker == nil {
		return nil
	}
	return current_worker.pool
}

current_temp_arena :: proc() -> ^virtual.Arena {
	if current_worker == nil {
		return nil
	}
	return &current_worker.temp_arena
}

// available_parallelism returns the logical processor count reported by the OS,
// clamped to at least one.
available_parallelism :: proc() -> int {
	_, logical, ok := sysinfo.cpu_core_count()
	if !ok || logical < 1 {
		return 1
	}
	return logical
}

// recommended_worker_count leaves the caller thread available and falls back to
// the inline executor when no spare logical core is available.
recommended_worker_count :: proc() -> int {
	count := available_parallelism()
	if count <= 1 {
		return 0
	}
	return count - 1
}

// pool_start launches worker threads. It is a no-op for inline pools where
// `Options.worker_count == 0`.
pool_start :: proc(pool: ^Pool) -> Submit_Error {
	if pool.started {
		return .None
	}
	pool.started = true
	for i in 0 ..< len(pool.workers) {
		t := thread.create(worker_runner)
		if t == nil {
			return .Invalid_Options
		}
		t.data = &pool.workers[i]
		t.user_index = i
		pool.workers[i].thread_handle = t
		thread.start(t)
	}
	return .None
}

// pool_join stops accepting new work and waits for queued/running work to drain
// before joining worker threads.
pool_join :: proc(pool: ^Pool) {
	sync.atomic_store_explicit(&pool.shutting_down, true, .Release)
	if len(pool.workers) > 0 {
		sync.sema_post(&pool.available, len(pool.workers))
	}
	for i in 0 ..< len(pool.workers) {
		t := pool.workers[i].thread_handle
		if t != nil {
			thread.join(t)
			thread.destroy(t)
			pool.workers[i].thread_handle = nil
		}
	}
	pool.started = false
}

// pool_destroy releases fixed pool storage. Call after `pool_join` for threaded
// pools.
pool_destroy :: proc(pool: ^Pool) {
	if pool.started {
		pool_join(pool)
	}
	for i in 0 ..< len(pool.workers) {
		delete(pool.workers[i].deque_buffer, pool.allocator)
		delete(pool.workers[i].ingress_buffer, pool.allocator)
		virtual.arena_destroy(&pool.workers[i].temp_arena)
	}
	delete(pool.workers, pool.allocator)
	delete(pool.free_buffer, pool.allocator)
	delete(pool.cells, pool.allocator)
	pool^ = {}
}

// pool_stats returns a relaxed snapshot of pool counters.
pool_stats :: proc(pool: ^Pool) -> Stats {
	return Stats {
		submitted    = sync.atomic_load_explicit(&pool.submitted_count, .Relaxed),
		completed    = sync.atomic_load_explicit(&pool.completed_count, .Relaxed),
		steals       = sync.atomic_load_explicit(&pool.steal_count, .Relaxed),
		steal_misses = sync.atomic_load_explicit(&pool.steal_miss_count, .Relaxed),
		queue_full   = sync.atomic_load_explicit(&pool.queue_full_count, .Relaxed),
		outstanding  = sync.atomic_load_explicit(&pool.outstanding, .Relaxed),
	}
}

// try_submit_value copies `payload` into a fixed task cell and schedules
// `work(payload) -> R` without waiting for capacity.
try_submit_value :: proc(pool: ^Pool, payload: $T, work: proc(T) -> $R) -> (Task(R), Submit_Error)
	where size_of(T) <= INLINE_BYTES_MAX,
	      size_of(R) <= INLINE_BYTES_MAX {
	task: Task(R)
	if size_of(T) > pool.options.inline_bytes {
		return task, .Payload_Too_Large
	}
	if size_of(R) > pool.options.inline_bytes {
		return task, .Result_Too_Large
	}

	cell, index, err := reserve_cell(pool)
	if err != .None {
		return task, err
	}

	value_invoke :: proc(cell: ^Task_Cell) {
		work := cast(proc(T) -> R)cell.user_proc
		arg := (^T)(raw_data(cell.payload[:]))^
		result := work(arg)
		if size_of(R) > 0 {
			intrinsics.mem_copy_non_overlapping(raw_data(cell.result[:]), &result, size_of(R))
		}
	}

	cell.kind = .Value
	cell.invoke = value_invoke
	cell.user_proc = rawptr(work)
	cell.payload_size = u16(size_of(T))
	cell.result_size = u16(size_of(R))
	if size_of(T) > 0 {
		payload_copy := payload
		intrinsics.mem_copy_non_overlapping(raw_data(cell.payload[:]), &payload_copy, size_of(T))
	}

	task = Task(R){pool = pool, index = index, generation = cell.generation}
	if err = schedule_cell(pool, index, false); err != .None {
		release_cell(cell)
		return Task(R){}, err
	}
	return task, .None
}

// submit_value is the blocking form of `try_submit_value`; it yields or helps
// pool work until storage and queue capacity are available.
submit_value :: proc(pool: ^Pool, payload: $T, work: proc(T) -> $R) -> (Task(R), Submit_Error)
	where size_of(T) <= INLINE_BYTES_MAX,
	      size_of(R) <= INLINE_BYTES_MAX {
	for {
		task, err := try_submit_value(pool, payload, work)
		if err != .No_Task_Slots && err != .Queue_Full {
			return task, err
		}
		if sync.atomic_load_explicit(&pool.shutting_down, .Acquire) {
			return Task(R){}, .Pool_Shutting_Down
		}
		help_or_yield(pool)
	}
}

// try_submit_raw schedules `work(data)` without copying payload data. The
// caller owns `data` lifetime until the task completes.
try_submit_raw :: proc(pool: ^Pool, data: rawptr, work: Raw_Work_Proc) -> (Task(No_Result), Submit_Error) {
	task: Task(No_Result)
	cell, index, err := reserve_cell(pool)
	if err != .None {
		return task, err
	}

	raw_invoke :: proc(cell: ^Task_Cell) {
		work := cast(Raw_Work_Proc)cell.user_proc
		work(cell.user_data)
	}

	cell.kind = .Raw
	cell.invoke = raw_invoke
	cell.user_proc = rawptr(work)
	cell.user_data = data
	cell.result_size = 0
	task = Task(No_Result){pool = pool, index = index, generation = cell.generation}
	if err = schedule_cell(pool, index, false); err != .None {
		release_cell(cell)
		return Task(No_Result){}, err
	}
	return task, .None
}

// submit_raw is the blocking form of `try_submit_raw`.
submit_raw :: proc(pool: ^Pool, data: rawptr, work: Raw_Work_Proc) -> (Task(No_Result), Submit_Error) {
	for {
		task, err := try_submit_raw(pool, data, work)
		if err != .No_Task_Slots && err != .Queue_Full {
			return task, err
		}
		if sync.atomic_load_explicit(&pool.shutting_down, .Acquire) {
			return Task(No_Result){}, .Pool_Shutting_Down
		}
		help_or_yield(pool)
	}
}

// then consumes `task` and schedules `work(parent_result) -> R` after it
// completes. Only one continuation may be attached to a task.
then :: proc(task: Task($T), work: proc(T) -> $R) -> (Task(R), Submit_Error)
	where size_of(T) <= INLINE_BYTES_MAX,
	      size_of(R) <= INLINE_BYTES_MAX {
	child: Task(R)
	pool := task.pool
	parent := cell_from_task(task)
	if parent == nil {
		return child, .Invalid_Task
	}
	if size_of(R) > pool.options.inline_bytes {
		return child, .Result_Too_Large
	}

	cell, index, err := reserve_cell(pool)
	if err != .None {
		return child, err
	}

	continuation_invoke :: proc(cell: ^Task_Cell) {
		parent := parent_cell(cell)
		if parent == nil {
			return
		}
		work := cast(proc(T) -> R)cell.user_proc
		arg := (^T)(raw_data(parent.result[:]))^
		result := work(arg)
		if size_of(R) > 0 {
			intrinsics.mem_copy_non_overlapping(raw_data(cell.result[:]), &result, size_of(R))
		}
		release_cell(parent)
	}

	cell.kind = .Continuation
	cell.invoke = continuation_invoke
	cell.user_proc = rawptr(work)
	cell.parent_index = task.index
	cell.parent_generation = task.generation
	cell.result_size = u16(size_of(R))

	if !attach_continuation(parent, index) {
		release_cell(cell)
		return child, .Continuation_Already_Set
	}

	child = Task(R){pool = pool, index = index, generation = cell.generation}
	schedule_if_parent_completed(parent, pool, index)
	return child, .None
}

// then_raw consumes `task` and schedules `work(data)` after it completes. The
// parent result is ignored, but parent completion still gates the raw task.
then_raw :: proc(task: Task($T), data: rawptr, work: Raw_Work_Proc) -> (Task(No_Result), Submit_Error) {
	child: Task(No_Result)
	pool := task.pool
	parent := cell_from_task(task)
	if parent == nil {
		return child, .Invalid_Task
	}

	cell, index, err := reserve_cell(pool)
	if err != .None {
		return child, err
	}

	raw_continuation_invoke :: proc(cell: ^Task_Cell) {
		parent := parent_cell(cell)
		if parent != nil {
			release_cell(parent)
		}
		work := cast(Raw_Work_Proc)cell.user_proc
		work(cell.user_data)
	}

	cell.kind = .Continuation
	cell.invoke = raw_continuation_invoke
	cell.user_proc = rawptr(work)
	cell.user_data = data
	cell.parent_index = task.index
	cell.parent_generation = task.generation

	if !attach_continuation(parent, index) {
		release_cell(cell)
		return child, .Continuation_Already_Set
	}

	child = Task(No_Result){pool = pool, index = index, generation = cell.generation}
	schedule_if_parent_completed(parent, pool, index)
	return child, .None
}

// then_on_nbio consumes `task` and posts `cb(parent_result, data)` onto `loop`
// after completion. The caller must keep the nbio event loop alive until the
// callback runs.
then_on_nbio :: proc(task: Task($T), loop: ^nbio.Event_Loop, cb: proc(T, rawptr), data: rawptr) -> Submit_Error
	where size_of(T) <= INLINE_BYTES_MAX {
	pool := task.pool
	parent := cell_from_task(task)
	if parent == nil || loop == nil {
		return .Invalid_Task
	}

	cell, index, err := reserve_cell(pool)
	if err != .None {
		return err
	}

	nbio_post_invoke :: proc(cell: ^Task_Cell) {
		nbio.next_tick_poly(rawptr(cell), nbio_cell_callback, cell.nbio_loop)
	}

	nbio_dispatch :: proc(cell: ^Task_Cell) {
		parent := parent_cell(cell)
		if parent != nil {
			cb := cast(proc(T, rawptr))cell.user_proc
			value := (^T)(raw_data(parent.result[:]))^
			cb(value, cell.user_data)
			release_cell(parent)
		}
		release_cell(cell)
	}

	cell.kind = .Nbio_Post
	cell.invoke = nbio_post_invoke
	cell.user_proc = rawptr(cb)
	cell.user_data = data
	cell.nbio_loop = loop
	cell.nbio_dispatch = nbio_dispatch
	cell.parent_index = task.index
	cell.parent_generation = task.generation

	if !attach_continuation(parent, index) {
		release_cell(cell)
		return .Continuation_Already_Set
	}
	schedule_if_parent_completed(parent, pool, index)
	return .None
}

// then_all consumes every task in `tasks` and schedules `work(results) -> R`
// after they all complete. The `results` slice is temporary and is only valid
// during the call to `work`.
then_all :: proc(pool: ^Pool, tasks: []Task($T), work: proc([]T) -> $R) -> (Task(R), Submit_Error)
	where size_of(T) <= INLINE_BYTES_MAX,
	      size_of(R) <= INLINE_BYTES_MAX {
	child: Task(R)
	if pool == nil {
		return child, .Invalid_Task
	}
	if size_of(R) > pool.options.inline_bytes {
		return child, .Result_Too_Large
	}

	cell, index, err := reserve_cell(pool)
	if err != .None {
		return child, err
	}

	Then_All_Data :: struct {
		tasks:     []Task(T),
		results:   []T,
		work:      proc([]T) -> R,
		allocator: mem.Allocator,
	}

	data := new(Then_All_Data, pool.allocator)
	data.tasks = make([]Task(T), len(tasks), pool.allocator)
	data.results = make([]T, len(tasks), pool.allocator)
	data.work = work
	data.allocator = pool.allocator
	copy(data.tasks, tasks)

	then_all_invoke :: proc(cell: ^Task_Cell) {
		data := cast(^Then_All_Data)cell.user_data
		ok := true
		for task, i in data.tasks {
			value, wait_err := wait(task)
			if wait_err != .None {
				ok = false
			}
			data.results[i] = value
		}
		result: R
		if ok {
			result = data.work(data.results)
		}
		if size_of(R) > 0 {
			intrinsics.mem_copy_non_overlapping(raw_data(cell.result[:]), &result, size_of(R))
		}
		delete(data.tasks, data.allocator)
		delete(data.results, data.allocator)
		free(data, data.allocator)
	}

	cell.kind = .Continuation
	cell.invoke = then_all_invoke
	cell.user_data = data
	cell.result_size = u16(size_of(R))

	for task in tasks {
		parent := cell_from_task(task)
		if parent == nil || parent.pool != pool {
			delete(data.tasks, data.allocator)
			delete(data.results, data.allocator)
			free(data, data.allocator)
			release_cell(cell)
			return child, .Invalid_Task
		}
		if !attach_continuation(parent, index) {
			delete(data.tasks, data.allocator)
			delete(data.results, data.allocator)
			free(data, data.allocator)
			release_cell(cell)
			return child, .Continuation_Already_Set
		}
	}

	child = Task(R){pool = pool, index = index, generation = cell.generation}
	if len(tasks) == 0 {
		_ = schedule_cell(pool, index, true)
	} else {
		for task in tasks {
			parent := cell_from_task(task)
			if parent != nil {
				schedule_if_parent_completed(parent, pool, index)
			}
		}
	}
	return child, .None
}

// make_deferred creates a task that is completed externally by its paired
// completer.
make_deferred :: proc(pool: ^Pool, $T: typeid) -> (Task(T), Completer(T), Submit_Error)
	where size_of(T) <= INLINE_BYTES_MAX {
	task: Task(T)
	completer: Completer(T)
	if size_of(T) > pool.options.inline_bytes {
		return task, completer, .Result_Too_Large
	}
	cell, index, err := reserve_cell(pool)
	if err != .None {
		return task, completer, err
	}
	cell.kind = .Deferred
	cell.result_size = u16(size_of(T))
	sync.atomic_store_explicit(&cell.state, Task_State.Deferred, .Release)
	task = Task(T){pool = pool, index = index, generation = cell.generation}
	completer = Completer(T){pool = pool, index = index, generation = cell.generation}
	return task, completer, .None
}

// complete stores a deferred value, wakes waiters, and schedules an attached
// continuation if present.
complete :: proc(completer: Completer($T), value: T) -> Complete_Error
	where size_of(T) <= INLINE_BYTES_MAX {
	pool := completer.pool
	if pool == nil || completer.index >= u32(len(pool.cells)) {
		return .Invalid_Task
	}
	cell := &pool.cells[completer.index]
	if cell.generation != completer.generation {
		return .Invalid_Task
	}
	if size_of(T) > pool.options.inline_bytes {
		return .Result_Too_Large
	}
	if _, ok := sync.atomic_compare_exchange_strong_explicit(&cell.state, Task_State.Deferred, Task_State.Running, .Acquire, .Relaxed); !ok {
		return .Already_Completed
	}
	if size_of(T) > 0 {
		value_copy := value
		intrinsics.mem_copy_non_overlapping(raw_data(cell.result[:]), &value_copy, size_of(T))
	}
	finish_cell(cell)
	return .None
}

// wait blocks until `task` completes. Pool workers help execute work while
// waiting so nested submit/wait paths do not deadlock.
wait :: proc(task: Task($T)) -> (T, Wait_Error)
	where size_of(T) <= INLINE_BYTES_MAX {
	for {
		if value, ready, err := try_wait(task); ready || err != .Not_Ready {
			return value, err
		}
		help_or_yield(task.pool)
	}
}

// try_wait returns immediately with `ready == false` when the task has not
// completed yet.
try_wait :: proc(task: Task($T)) -> (T, bool, Wait_Error)
	where size_of(T) <= INLINE_BYTES_MAX {
	value: T
	cell := cell_from_task(task)
	if cell == nil {
		return value, false, .Invalid_Task
	}
	state := sync.atomic_load_explicit(&cell.state, .Acquire)
	if state == .Consumed || state == .Free {
		return value, false, .Already_Consumed
	}
	if state != .Completed {
		return value, false, .Not_Ready
	}
	if size_of(T) > 0 {
		intrinsics.mem_copy_non_overlapping(&value, raw_data(cell.result[:]), size_of(T))
	}
	release_cell(cell)
	return value, true, .None
}

normalize_options :: proc(options: Options) -> Options {
	result := options
	limit := recommended_worker_count()
	if result.worker_count < 0 {
		result.worker_count = limit
	} else if result.worker_count > limit {
		result.worker_count = limit
	}
	if result.task_capacity == 0 {
		result.task_capacity = DEFAULT_TASK_CAPACITY
	}
	if result.queue_capacity == 0 {
		result.queue_capacity = DEFAULT_QUEUE_CAPACITY
	}
	if result.deque_capacity == 0 {
		result.deque_capacity = DEFAULT_DEQUE_CAPACITY
	}
	if result.inline_bytes == 0 {
		result.inline_bytes = 128
	}
	return result
}

validate_options :: proc(options: Options) -> bool {
	return options.worker_count >= 0 &&
	       options.inline_bytes > 0 &&
	       options.inline_bytes <= INLINE_BYTES_MAX &&
	       is_power_of_two(options.task_capacity) &&
	       is_power_of_two(options.queue_capacity) &&
	       is_power_of_two(options.deque_capacity)
}

reserve_cell :: proc(pool: ^Pool) -> (^Task_Cell, u32, Submit_Error) {
	if pool == nil || sync.atomic_load_explicit(&pool.shutting_down, .Acquire) {
		return nil, INDEX_NONE, .Pool_Shutting_Down
	}
	index, ok := mpmc_index_ring_dequeue(&pool.free_queue)
	if !ok {
		sync.atomic_add_explicit(&pool.queue_full_count, 1, .Relaxed)
		return nil, INDEX_NONE, .No_Task_Slots
	}
	cell := &pool.cells[index]
	cell.state = .Reserved
	cell.kind = .None
	cell.result_size = 0
	cell.payload_size = 0
	cell.parent_index = INDEX_NONE
	cell.parent_generation = 0
	cell.continuation_index = INDEX_NONE
	cell.pool = pool
	cell.invoke = nil
	cell.user_proc = nil
	cell.user_data = nil
	cell.nbio_loop = nil
	cell.nbio_dispatch = nil
	intrinsics.mem_zero(raw_data(cell.payload[:]), len(cell.payload))
	intrinsics.mem_zero(raw_data(cell.result[:]), len(cell.result))
	sync.atomic_add_explicit(&pool.outstanding, 1, .Relaxed)
	sync.atomic_add_explicit(&pool.submitted_count, 1, .Relaxed)
	return cell, index, .None
}

release_cell :: proc(cell: ^Task_Cell) {
	pool := cell.pool
	if pool == nil {
		return
	}
	cell.state = .Consumed
	cell.generation += 1
	cell.kind = .None
	cell.invoke = nil
	cell.user_proc = nil
	cell.user_data = nil
	cell.parent_index = INDEX_NONE
	cell.parent_generation = 0
	cell.continuation_index = INDEX_NONE
	cell.nbio_loop = nil
	cell.nbio_dispatch = nil
	index := u32(uintptr(cell) - uintptr(raw_data(pool.cells))) / u32(size_of(Task_Cell))
	sync.atomic_store_explicit(&cell.state, Task_State.Free, .Release)
	_ = mpmc_index_ring_enqueue(&pool.free_queue, index)
	sync.atomic_sub_explicit(&pool.outstanding, 1, .Relaxed)
}

cell_from_task :: proc(task: Task($T)) -> ^Task_Cell {
	if task.pool == nil || task.index >= u32(len(task.pool.cells)) {
		return nil
	}
	cell := &task.pool.cells[task.index]
	if cell.generation != task.generation {
		return nil
	}
	return cell
}

parent_cell :: proc(cell: ^Task_Cell) -> ^Task_Cell {
	pool := cell.pool
	if pool == nil || cell.parent_index >= u32(len(pool.cells)) {
		return nil
	}
	parent := &pool.cells[cell.parent_index]
	if parent.generation != cell.parent_generation {
		return nil
	}
	return parent
}

attach_continuation :: proc(parent: ^Task_Cell, child_index: u32) -> bool {
	if parent.continuation_index != INDEX_NONE {
		return false
	}
	parent.continuation_index = child_index
	return true
}

schedule_if_parent_completed :: proc(parent: ^Task_Cell, pool: ^Pool, child_index: u32) {
	if sync.atomic_load_explicit(&parent.state, .Acquire) == .Completed {
		_ = schedule_cell(pool, child_index, true)
	}
}

schedule_cell :: proc(pool: ^Pool, index: u32, must_schedule: bool) -> Submit_Error {
	if pool.options.worker_count == 0 {
		cell := &pool.cells[index]
		if _, ok := sync.atomic_compare_exchange_strong_explicit(&cell.state, Task_State.Reserved, Task_State.Running, .Acquire, .Relaxed); ok {
			execute_cell(cell)
		}
		return .None
	}

	cell := &pool.cells[index]
	if _, ok := sync.atomic_compare_exchange_strong_explicit(&cell.state, Task_State.Reserved, Task_State.Queued, .Release, .Relaxed); !ok {
		return .Invalid_Task
	}

	for {
		worker := current_worker
		if worker != nil && worker.pool == pool && work_deque_push(&worker.deque, index) {
			sync.sema_post(&pool.available)
			return .None
		}

		start := sync.atomic_add_explicit(&pool.next_worker, 1, .Relaxed)
		for offset in 0 ..< len(pool.workers) {
			i := int((start + u64(offset)) % u64(len(pool.workers)))
			if mpmc_index_ring_enqueue(&pool.workers[i].ingress, index) {
				sync.sema_post(&pool.available)
				return .None
			}
		}

		sync.atomic_add_explicit(&pool.queue_full_count, 1, .Relaxed)
		if !must_schedule {
			sync.atomic_store_explicit(&cell.state, Task_State.Reserved, .Release)
			return .Queue_Full
		}
		thread.yield()
	}
}

finish_cell :: proc(cell: ^Task_Cell) {
	pool := cell.pool
	sync.atomic_store_explicit(&cell.state, Task_State.Completed, .Release)
	sync.atomic_add_explicit(&pool.completed_count, 1, .Relaxed)
	child := cell.continuation_index
	if child != INDEX_NONE {
		_ = schedule_cell(pool, child, true)
	}
}

execute_cell :: proc(cell: ^Task_Cell) {
	worker := current_worker
	if worker != nil {
		worker_execute_cell(worker, cell)
		return
	}
	if cell.kind == .Nbio_Post {
		if cell.invoke != nil {
			cell.invoke(cell)
		}
		return
	}
	if cell.invoke != nil {
		cell.invoke(cell)
	}
	finish_cell(cell)
}

worker_execute_cell :: proc(worker: ^Worker, cell: ^Task_Cell) {
	previous_temp_allocator := context.temp_allocator
	context.temp_allocator = worker.temp_allocator
	worker.temp_depth += 1
	temp := virtual.arena_temp_begin(&worker.temp_arena)

	nbio_post := cell.kind == .Nbio_Post
	if cell.invoke != nil {
		cell.invoke(cell)
	}

	virtual.arena_temp_end(temp)
	worker.temp_depth -= 1
	if worker.temp_depth == 0 {
		virtual.arena_free_all(&worker.temp_arena)
	}
	context.temp_allocator = previous_temp_allocator
	if !nbio_post {
		finish_cell(cell)
	}
}

worker_runner :: proc(t: ^thread.Thread) {
	worker := cast(^Worker)t.data
	current_worker = worker
	pool := worker.pool
	for {
		if execute_next(worker) {
			continue
		}
		if sync.atomic_load_explicit(&pool.shutting_down, .Acquire) &&
		   sync.atomic_load_explicit(&pool.outstanding, .Acquire) == 0 {
			break
		}
		sync.sema_wait(&pool.available)
	}
	current_worker = nil
}

execute_next :: proc(worker: ^Worker) -> bool {
	if index, ok := work_deque_pop(&worker.deque); ok {
		execute_index(worker.pool, index)
		return true
	}
	if index, ok := mpmc_index_ring_dequeue(&worker.ingress); ok {
		execute_index(worker.pool, index)
		return true
	}
	for i in 0 ..< len(worker.pool.workers) {
		if i == worker.id {
			continue
		}
		if index, ok := mpmc_index_ring_dequeue(&worker.pool.workers[i].ingress); ok {
			sync.atomic_add_explicit(&worker.pool.steal_count, 1, .Relaxed)
			execute_index(worker.pool, index)
			return true
		}
	}
	sync.atomic_add_explicit(&worker.pool.steal_miss_count, 1, .Relaxed)
	return false
}

execute_index :: proc(pool: ^Pool, index: u32) {
	if index >= u32(len(pool.cells)) {
		return
	}
	cell := &pool.cells[index]
	if _, ok := sync.atomic_compare_exchange_strong_explicit(&cell.state, Task_State.Queued, Task_State.Running, .Acquire, .Relaxed); ok {
		execute_cell(cell)
	}
}

help_or_yield :: proc(pool: ^Pool) {
	worker := current_worker
	if worker != nil && worker.pool == pool {
		if execute_next(worker) {
			return
		}
	}
	thread.yield()
}

nbio_cell_callback :: proc(op: ^nbio.Operation, p: rawptr) {
	cell := cast(^Task_Cell)p
	if cell.nbio_dispatch != nil {
		cell.nbio_dispatch(cell)
	} else {
		if parent := parent_cell(cell); parent != nil {
			release_cell(parent)
		}
		release_cell(cell)
	}
	_ = op
}
