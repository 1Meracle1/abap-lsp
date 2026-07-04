package abap_frontend_execution

import "base:intrinsics"
import "core:container/xar"
import "core:mem"
import "core:mem/virtual"
import "core:sync"
import sysinfo "core:sys/info"
import "core:thread"

AUTO_WORKER_COUNT :: int(-1)

No_Result :: struct {}

Options :: struct {
	worker_count:        int,
	task_capacity:       int,
	queue_capacity:      int,
	deque_capacity:      int,
	edge_capacity:       int,
	main_queue_capacity: int,
}

Stats :: struct {
	submitted:   u64,
	completed:   u64,
	steals:      u64,
	queue_full:  u64,
	outstanding: u64,
}

Executor_Kind :: enum u8 {
	Worker,
	Main,
}

Executor :: struct {
	kind: Executor_Kind,
	pool: ^Pool,
	main: ^Main_Executor,
}

Task :: struct($T: typeid) {
	graph:      ^Graph,
	index:      u32,
	generation: u32,
}

Task_State :: enum u32 {
	Free,
	Reserved,
	Queued,
	Running,
	Completed,
}

Task_Kind :: enum u8 {
	None,
	Value,
	Then,
	Then_With,
	Then_All,
}

Task_Invoke_Proc :: #type proc(cell: ^Task_Cell)

Task_Cell :: struct #align (CACHE_LINE_SIZE) {
	state:          Task_State,
	generation:     u32,
	index:          u32,
	kind:           Task_Kind,
	pending_count:  u32,
	parent_index:   u32,
	first_child:    u32,
	next_sibling:   u32,
	first_edge:     u32,
	graph:          ^Graph,
	executor:       Executor,
	invoke:         Task_Invoke_Proc,
	user_proc:      rawptr,
	user_data:      rawptr,
	available:      sync.Sema,
	payload:        rawptr,
	result:         rawptr,
	completed_once: bool,
}

Task_Edge :: struct {
	parent: u32,
	child:  u32,
	next:   u32,
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

Pool :: struct {
	allocator:        mem.Allocator,
	options:          Options,
	cells:            xar.Array(Task_Cell, 4),
	free_cells:       [dynamic]u32,
	cell_count:       u32,
	cell_lock:        sync.Mutex,
	edges:            xar.Array(Task_Edge, 4),
	free_edges:       [dynamic]u32,
	edge_count:       u32,
	edge_lock:        sync.Mutex,
	workers:          []Worker,
	available:        sync.Sema,
	started:          bool,
	shutting_down:    bool,
	next_worker:      u64,
	submitted_count:  u64,
	completed_count:  u64,
	steal_count:      u64,
	queue_full_count: u64,
	outstanding:      u64,
}

Graph :: struct {
	pool:             ^Pool,
	allocator:        mem.Allocator,
	object_allocator: mem.Allocator,
	arena:            virtual.Arena,
	cell_indices:     [dynamic]u32,
	edge_indices:     [dynamic]u32,
	started:          bool,
	detached:         bool,
	owned:            bool,
	completed:        bool,
	remaining:        u64,
	available:        sync.Sema,
}

Main_Executor :: struct {
	allocator:      mem.Allocator,
	queue:          Main_Queue,
	buffer:         []Main_Queue_Cell,
	temp_arena:     virtual.Arena,
	temp_allocator: mem.Allocator,
	temp_depth:     int,
}

@(private = "file", thread_local)
current_worker: ^Worker

@(private = "file", thread_local)
current_main_executor: ^Main_Executor

pool_cell :: proc(pool: ^Pool, index: u32) -> ^Task_Cell {
	assert(index < sync.atomic_load_explicit(&pool.cell_count, .Acquire))
	return xar.get_ptr_unsafe(&pool.cells, int(index))
}

pool_edge :: proc(pool: ^Pool, index: u32) -> ^Task_Edge {
	assert(index < sync.atomic_load_explicit(&pool.edge_count, .Acquire))
	return xar.get_ptr_unsafe(&pool.edges, int(index))
}

add_pool_cell_locked :: proc(pool: ^Pool) -> u32 {
	assert(u64(xar.len(pool.cells)) < u64(INDEX_NONE))
	index := u32(xar.len(pool.cells))
	cell, err := xar.push_back_elem_and_get_ptr(&pool.cells, Task_Cell{})
	assert(err == .None && cell != nil)
	cell.index = index
	cell.generation = 1
	cell.state = .Free
	sync.atomic_store_explicit(&pool.cell_count, index + 1, .Release)
	return index
}

add_pool_edge_locked :: proc(pool: ^Pool) -> u32 {
	assert(u64(xar.len(pool.edges)) < u64(INDEX_NONE))
	index := u32(xar.len(pool.edges))
	edge, err := xar.push_back_elem_and_get_ptr(&pool.edges, Task_Edge{next = INDEX_NONE})
	assert(err == .None && edge != nil)
	sync.atomic_store_explicit(&pool.edge_count, index + 1, .Release)
	return index
}

reserve_cell_index :: proc(pool: ^Pool) -> u32 {
	sync.mutex_lock(&pool.cell_lock)
	defer sync.mutex_unlock(&pool.cell_lock)
	if len(pool.free_cells) > 0 {
		return pop(&pool.free_cells)
	}
	return add_pool_cell_locked(pool)
}

release_cell_index :: proc(pool: ^Pool, index: u32) {
	sync.mutex_lock(&pool.cell_lock)
	append(&pool.free_cells, index)
	sync.mutex_unlock(&pool.cell_lock)
}

reserve_edge_index :: proc(pool: ^Pool) -> u32 {
	sync.mutex_lock(&pool.edge_lock)
	defer sync.mutex_unlock(&pool.edge_lock)
	if len(pool.free_edges) > 0 {
		return pop(&pool.free_edges)
	}
	return add_pool_edge_locked(pool)
}

release_edge_index :: proc(pool: ^Pool, index: u32) {
	sync.mutex_lock(&pool.edge_lock)
	append(&pool.free_edges, index)
	sync.mutex_unlock(&pool.edge_lock)
}

pool_init :: proc(pool: ^Pool, options: Options, allocator: mem.Allocator) {
	opts := normalize_options(options)

	pool^ = {}
	pool.allocator = allocator
	pool.options = opts
	xar.init(&pool.cells, allocator)
	xar.init(&pool.edges, allocator)
	pool.free_cells = make([dynamic]u32, 0, opts.task_capacity, allocator)
	pool.free_edges = make([dynamic]u32, 0, opts.edge_capacity, allocator)
	for _ in 0 ..< opts.task_capacity {
		append(&pool.free_cells, add_pool_cell_locked(pool))
	}
	for _ in 0 ..< opts.edge_capacity {
		append(&pool.free_edges, add_pool_edge_locked(pool))
	}

	if opts.worker_count > 0 {
		pool.workers = make([]Worker, opts.worker_count, allocator)
		for i in 0 ..< len(pool.workers) {
			worker := &pool.workers[i]
			worker.pool = pool
			worker.id = i
			worker.deque_buffer = make([]u32, opts.deque_capacity, allocator)
			worker.ingress_buffer = make([]Mpmc_Cell, opts.queue_capacity, allocator)
			arena_err := virtual.arena_init_growing(&worker.temp_arena)
			assert(arena_err == .None)
			worker.temp_allocator = virtual.arena_allocator(&worker.temp_arena)
			work_deque_init(&worker.deque, worker.deque_buffer)
			mpmc_index_ring_init(&worker.ingress, worker.ingress_buffer)
		}
	}
}

pool_start :: proc(pool: ^Pool) {
	if pool.started {
		return
	}
	assert(!sync.atomic_load_explicit(&pool.shutting_down, .Acquire))
	pool.started = true
	for i in 0 ..< len(pool.workers) {
		t := thread.create(worker_runner)
		assert(t != nil)
		t.data = &pool.workers[i]
		t.user_index = i
		pool.workers[i].thread_handle = t
		thread.start(t)
	}
}

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
	delete(pool.free_edges)
	xar.destroy(&pool.edges)
	delete(pool.free_cells)
	xar.destroy(&pool.cells)
	pool^ = {}
}

pool_stats :: proc(pool: ^Pool) -> Stats {
	return Stats {
		submitted = sync.atomic_load_explicit(&pool.submitted_count, .Relaxed),
		completed = sync.atomic_load_explicit(&pool.completed_count, .Relaxed),
		steals = sync.atomic_load_explicit(&pool.steal_count, .Relaxed),
		queue_full = sync.atomic_load_explicit(&pool.queue_full_count, .Relaxed),
		outstanding = sync.atomic_load_explicit(&pool.outstanding, .Relaxed),
	}
}

worker_executor :: proc(pool: ^Pool) -> Executor {
	return Executor{kind = .Worker, pool = pool}
}

main_executor :: proc(main: ^Main_Executor) -> Executor {
	return Executor{kind = .Main, main = main}
}

main_executor_init :: proc(main: ^Main_Executor, capacity: int, allocator: mem.Allocator) {
	cap := normalize_capacity(capacity, 1024, 2)
	main^ = {}
	main.allocator = allocator
	main.buffer = make([]Main_Queue_Cell, cap, allocator)
	arena_err := virtual.arena_init_growing(&main.temp_arena)
	assert(arena_err == .None)
	main.temp_allocator = virtual.arena_allocator(&main.temp_arena)
	main_queue_init(&main.queue, main.buffer)
}

main_executor_drain :: proc(main: ^Main_Executor, max_tasks := -1) -> int {
	count := 0
	for max_tasks < 0 || count < max_tasks {
		cell, ok := main_queue_dequeue(&main.queue)
		if !ok {
			break
		}
		main_executor_execute_cell(main, cell)
		count += 1
	}
	return count
}

main_executor_destroy :: proc(main: ^Main_Executor) {
	virtual.arena_destroy(&main.temp_arena)
	delete(main.buffer, main.allocator)
	main^ = {}
}

current_pool :: proc() -> ^Pool {
	if current_worker == nil {
		return nil
	}
	return current_worker.pool
}

current_temp_arena :: proc "contextless" () -> ^virtual.Arena {
	if current_main_executor != nil {
		return &current_main_executor.temp_arena
	}
	if current_worker != nil {
		return &current_worker.temp_arena
	}
	unreachable()
}

recommended_worker_count :: proc "contextless" () -> int {
	_, logical, _ := sysinfo.cpu_core_count()
	return max(logical - 1, 0)
}

graph_init :: proc(graph: ^Graph, pool: ^Pool, allocator: mem.Allocator) {
	graph^ = {}
	graph.pool = pool
	arena_err := virtual.arena_init_growing(&graph.arena)
	assert(arena_err == .None)
	graph.allocator = virtual.arena_allocator(&graph.arena)
	graph.object_allocator = allocator
	graph.cell_indices = make([dynamic]u32, 0, pool.options.task_capacity, graph.allocator)
	graph.edge_indices = make([dynamic]u32, 0, pool.options.edge_capacity, graph.allocator)
}

graph_create :: proc(pool: ^Pool, allocator: mem.Allocator) -> ^Graph {
	graph := new(Graph, allocator)
	graph_init(graph, pool, allocator)
	graph.owned = true
	return graph
}

graph_start :: proc(graph: ^Graph) {
	if graph.started {
		return
	}
	assert(!sync.atomic_load_explicit(&graph.pool.shutting_down, .Acquire))
	graph.started = true
	if len(graph.cell_indices) == 0 {
		complete_empty_graph(graph)
		return
	}
	for index in graph.cell_indices {
		cell := pool_cell(graph.pool, index)
		if sync.atomic_load_explicit(&cell.state, .Acquire) == .Reserved &&
		   sync.atomic_load_explicit(&cell.pending_count, .Acquire) == 0 {
			schedule_cell(cell)
		}
	}
}

graph_detach :: proc(graph: ^Graph) {
	assert(graph.owned)
	graph.detached = true
	graph_start(graph)
}

graph_wait :: proc(graph: ^Graph) {
	for !sync.atomic_load_explicit(&graph.completed, .Acquire) {
		worker := current_worker
		if worker != nil && worker.pool == graph.pool && execute_next(worker) {
			continue
		}
		sync.sema_wait(&graph.available)
	}
}

graph_completed :: proc(graph: ^Graph) -> bool {
	return graph != nil && sync.atomic_load_explicit(&graph.completed, .Acquire)
}

graph_reset :: proc(graph: ^Graph) {
	assert(!graph.detached)
	assert(!graph.started || sync.atomic_load_explicit(&graph.completed, .Acquire))
	pool := graph.pool
	object_allocator := graph.object_allocator
	owned := graph.owned
	release_graph_storage(graph)
	virtual.arena_free_all(&graph.arena)
	graph.pool = pool
	graph.allocator = virtual.arena_allocator(&graph.arena)
	graph.object_allocator = object_allocator
	graph.cell_indices = make([dynamic]u32, 0, pool.options.task_capacity, graph.allocator)
	graph.edge_indices = make([dynamic]u32, 0, pool.options.edge_capacity, graph.allocator)
	graph.started = false
	graph.detached = false
	graph.owned = owned
	graph.completed = false
	graph.remaining = 0
	graph.available = {}
}

graph_destroy :: proc(graph: ^Graph) {
	release_graph_storage(graph)
	virtual.arena_destroy(&graph.arena)
	graph^ = {}
}

submit_value :: proc(
	graph: ^Graph,
	exec: Executor,
	payload: $P,
	work: proc(_: P) -> $R,
) -> Task(R) {
	cell := reserve_cell(graph, exec, .Value)
	cell.user_proc = rawptr(work)
	if size_of(P) > 0 && size_of(R) > 0 {
		Storage :: struct {
			payload: P,
			result:  R,
		}
		storage := new(Storage, graph.allocator)
		storage.payload = payload
		cell.payload = &storage.payload
		cell.result = &storage.result
	} else {
		cell.result = alloc_task_storage(graph, R)
		if size_of(P) > 0 {
			payload_copy := payload
			cell.payload = alloc_task_storage(graph, P)
			intrinsics.mem_copy_non_overlapping(cell.payload, &payload_copy, size_of(P))
		}
	}

	invoke :: proc(cell: ^Task_Cell) {
		work := cast(proc(_: P) -> R)cell.user_proc
		arg: P
		if size_of(P) > 0 {
			arg = (^P)(cell.payload)^
		}
		result := work(arg)
		if size_of(R) > 0 {
			intrinsics.mem_copy_non_overlapping(cell.result, &result, size_of(R))
		}
	}
	cell.invoke = invoke
	return Task(R){graph = graph, index = cell.index, generation = cell.generation}
}

then :: proc(graph: ^Graph, parent: Task($T), exec: Executor, work: proc(_: T) -> $R) -> Task(R) {
	assert(cell_from_task(parent) != nil && parent.graph == graph)
	cell := reserve_cell(graph, exec, .Then)
	cell.user_proc = rawptr(work)
	cell.result = alloc_task_storage(graph, R)
	cell.parent_index = parent.index
	cell.pending_count = 1
	add_child_edge(graph, parent.index, cell.index)

	invoke :: proc(cell: ^Task_Cell) {
		work := cast(proc(_: T) -> R)cell.user_proc
		parent := pool_cell(cell.graph.pool, cell.parent_index)
		arg: T
		if size_of(T) > 0 {
			arg = (^T)(parent.result)^
		}
		result := work(arg)
		if size_of(R) > 0 {
			intrinsics.mem_copy_non_overlapping(cell.result, &result, size_of(R))
		}
	}
	cell.invoke = invoke
	return Task(R){graph = graph, index = cell.index, generation = cell.generation}
}

then_with :: proc(
	graph: ^Graph,
	parent: Task($T),
	exec: Executor,
	payload: $P,
	work: proc(_: T, _: P) -> $R,
) -> Task(R) {
	assert(cell_from_task(parent) != nil && parent.graph == graph)
	cell := reserve_cell(graph, exec, .Then_With)
	cell.user_proc = rawptr(work)
	if size_of(P) > 0 && size_of(R) > 0 {
		Storage :: struct {
			payload: P,
			result:  R,
		}
		storage := new(Storage, graph.allocator)
		storage.payload = payload
		cell.payload = &storage.payload
		cell.result = &storage.result
	} else {
		cell.result = alloc_task_storage(graph, R)
		if size_of(P) > 0 {
			payload_copy := payload
			cell.payload = alloc_task_storage(graph, P)
			intrinsics.mem_copy_non_overlapping(cell.payload, &payload_copy, size_of(P))
		}
	}
	cell.parent_index = parent.index
	cell.pending_count = 1
	add_child_edge(graph, parent.index, cell.index)

	invoke :: proc(cell: ^Task_Cell) {
		work := cast(proc(_: T, _: P) -> R)cell.user_proc
		parent := pool_cell(cell.graph.pool, cell.parent_index)
		arg: T
		if size_of(T) > 0 {
			arg = (^T)(parent.result)^
		}
		payload: P
		if size_of(P) > 0 {
			payload = (^P)(cell.payload)^
		}
		result := work(arg, payload)
		if size_of(R) > 0 {
			intrinsics.mem_copy_non_overlapping(cell.result, &result, size_of(R))
		}
	}
	cell.invoke = invoke
	return Task(R){graph = graph, index = cell.index, generation = cell.generation}
}

then_all :: proc(
	graph: ^Graph,
	parents: []Task($T),
	exec: Executor,
	work: proc(_: []T) -> $R,
) -> Task(R) {
	for parent in parents {
		assert(cell_from_task(parent) != nil && parent.graph == graph)
	}
	cell := reserve_cell(graph, exec, .Then_All)
	cell.result = alloc_task_storage(graph, R)

	Then_All_Data :: struct {
		parent_indices: []u32,
		work:           proc(_: []T) -> R,
	}

	data := new(Then_All_Data, graph.allocator)
	data.parent_indices = make([]u32, len(parents), graph.allocator)
	data.work = work
	for parent, i in parents {
		data.parent_indices[i] = parent.index
	}
	cell.user_data = data
	cell.pending_count = u32(len(parents))

	for parent in parents {
		add_edge(graph, parent.index, cell.index)
	}

	invoke :: proc(cell: ^Task_Cell) {
		data := cast(^Then_All_Data)cell.user_data
		results := make([]T, len(data.parent_indices), context.temp_allocator)
		for parent_index, i in data.parent_indices {
			parent := pool_cell(cell.graph.pool, parent_index)
			if size_of(T) > 0 {
				intrinsics.mem_copy_non_overlapping(&results[i], parent.result, size_of(T))
			}
		}
		result := data.work(results)
		if size_of(R) > 0 {
			intrinsics.mem_copy_non_overlapping(cell.result, &result, size_of(R))
		}
	}
	cell.invoke = invoke
	return Task(R){graph = graph, index = cell.index, generation = cell.generation}
}

wait :: proc(task: Task($T)) -> T {
	for {
		if value, ready := try_wait(task); ready {
			return value
		}
		graph := task.graph
		worker := current_worker
		if worker != nil && worker.pool == graph.pool && execute_next(worker) {
			continue
		}
		cell := cell_from_task_allow_completed(task)
		assert(cell != nil)
		sync.sema_wait(&cell.available)
	}
}

try_wait :: proc(task: Task($T)) -> (T, bool) {
	value: T
	cell := cell_from_task_allow_completed(task)
	assert(cell != nil)
	state := sync.atomic_load_explicit(&cell.state, .Acquire)
	if state == .Completed {
		if size_of(T) > 0 {
			intrinsics.mem_copy_non_overlapping(&value, cell.result, size_of(T))
		}
		return value, true
	}
	return value, false
}

alloc_task_storage :: proc(graph: ^Graph, $T: typeid) -> rawptr {
	if size_of(T) == 0 {
		return nil
	}
	ptr, err := mem.alloc(size_of(T), align_of(T), graph.allocator)
	assert(err == .None && ptr != nil)
	return ptr
}

reserve_cell :: proc(graph: ^Graph, exec: Executor, kind: Task_Kind) -> ^Task_Cell {
	assert(!graph.started)
	pool := graph.pool
	assert(!sync.atomic_load_explicit(&pool.shutting_down, .Acquire))
	executor := exec
	if executor.kind == .Worker {
		if executor.pool == nil {
			executor.pool = pool
		}
		assert(executor.pool == pool)
	} else {
		assert(executor.main != nil)
	}
	index := reserve_cell_index(pool)
	cell := pool_cell(pool, index)
	cell.index = index
	cell.state = .Reserved
	cell.kind = kind
	cell.graph = graph
	cell.executor = executor
	cell.parent_index = INDEX_NONE
	cell.first_child = INDEX_NONE
	cell.next_sibling = INDEX_NONE
	cell.first_edge = INDEX_NONE
	cell.pending_count = 0
	cell.invoke = nil
	cell.user_proc = nil
	cell.user_data = nil
	cell.available = {}
	cell.completed_once = false
	append(&graph.cell_indices, index)
	graph.remaining += 1
	sync.atomic_add_explicit(&pool.submitted_count, 1, .Relaxed)
	sync.atomic_add_explicit(&pool.outstanding, 1, .Relaxed)
	return cell
}

add_edge :: proc(graph: ^Graph, parent_index, child_index: u32) {
	pool := graph.pool
	edge_index := reserve_edge_index(pool)
	edge := pool_edge(pool, edge_index)
	edge.parent = parent_index
	edge.child = child_index
	parent := pool_cell(pool, parent_index)
	edge.next = parent.first_edge
	parent.first_edge = edge_index
	append(&graph.edge_indices, edge_index)
}

add_child_edge :: proc(graph: ^Graph, parent_index, child_index: u32) {
	parent := pool_cell(graph.pool, parent_index)
	child := pool_cell(graph.pool, child_index)
	child.next_sibling = parent.first_child
	parent.first_child = child_index
}

cell_from_task :: proc(task: Task($T)) -> ^Task_Cell {
	cell := cell_from_task_allow_completed(task)
	if cell == nil {
		return nil
	}
	if cell.state == .Free {
		return nil
	}
	return cell
}

cell_from_task_allow_completed :: proc(task: Task($T)) -> ^Task_Cell {
	graph := task.graph
	if graph == nil ||
	   graph.pool == nil ||
	   task.index >= sync.atomic_load_explicit(&graph.pool.cell_count, .Acquire) {
		return nil
	}
	cell := pool_cell(graph.pool, task.index)
	if cell.generation != task.generation || cell.graph != graph {
		return nil
	}
	return cell
}

schedule_cell :: proc(cell: ^Task_Cell) {
	pool := cell.graph.pool
	if _, ok := sync.atomic_compare_exchange_strong_explicit(
		&cell.state,
		Task_State.Reserved,
		Task_State.Queued,
		.Release,
		.Relaxed,
	); !ok {
		assert(false)
		return
	}

	if cell.executor.kind == .Main {
		ok := main_queue_enqueue(&cell.executor.main.queue, cell)
		assert(ok)
		return
	}

	if pool.options.worker_count == 0 {
		execute_cell(cell)
		return
	}

	for {
		worker := current_worker
		if worker != nil && worker.pool == pool && work_deque_push(&worker.deque, cell.index) {
			if pool.started {
				sync.sema_post(&pool.available)
			}
			return
		}

		start := sync.atomic_add_explicit(&pool.next_worker, 1, .Relaxed)
		for offset in 0 ..< len(pool.workers) {
			i := int((start + u64(offset)) % u64(len(pool.workers)))
			if mpmc_index_ring_enqueue(&pool.workers[i].ingress, cell.index) {
				if pool.started {
					sync.sema_post(&pool.available)
				}
				return
			}
		}

		sync.atomic_add_explicit(&pool.queue_full_count, 1, .Relaxed)
		help_or_yield(pool)
	}
}

execute_cell :: proc(cell: ^Task_Cell) {
	if !begin_execute_cell(cell) {
		return
	}
	invoke_cell(cell)
	finish_cell(cell)
}

begin_execute_cell :: proc(cell: ^Task_Cell) -> bool {
	_, ok := sync.atomic_compare_exchange_strong_explicit(
		&cell.state,
		Task_State.Queued,
		Task_State.Running,
		.Acquire,
		.Relaxed,
	)
	return ok
}

invoke_cell :: proc(cell: ^Task_Cell) {
	cell.invoke(cell)
}

finish_cell :: proc(cell: ^Task_Cell) {
	graph := cell.graph
	pool := graph.pool
	sync.atomic_store_explicit(&cell.state, Task_State.Completed, .Release)
	cell.completed_once = true
	sync.atomic_add_explicit(&pool.completed_count, 1, .Relaxed)
	sync.atomic_sub_explicit(&pool.outstanding, 1, .Relaxed)
	sync.sema_post(&cell.available)

	edge_index := cell.first_edge
	for edge_index != INDEX_NONE {
		edge := pool_edge(pool, edge_index)
		child := pool_cell(pool, edge.child)
		if sync.atomic_sub_explicit(&child.pending_count, 1, .Release) == 1 {
			schedule_cell(child)
		}
		edge_index = edge.next
	}
	child_index := cell.first_child
	for child_index != INDEX_NONE {
		child := pool_cell(pool, child_index)
		next_child := child.next_sibling
		if sync.atomic_sub_explicit(&child.pending_count, 1, .Release) == 1 {
			schedule_cell(child)
		}
		child_index = next_child
	}

	if sync.atomic_sub_explicit(&graph.remaining, 1, .Release) == 1 {
		if graph.detached && graph.owned {
			allocator := graph.object_allocator
			sync.atomic_store_explicit(&graph.completed, true, .Release)
			graph_destroy(graph)
			free(graph, allocator)
		} else {
			sync.atomic_store_explicit(&graph.completed, true, .Release)
			sync.sema_post(&graph.available)
		}
	}
}

release_cell :: proc(cell: ^Task_Cell) {
	pool := cell.graph.pool
	if !cell.completed_once {
		sync.atomic_sub_explicit(&pool.outstanding, 1, .Relaxed)
	}
	index := cell.index
	generation := cell.generation + 1
	if generation == 0 {
		generation = 1
	}
	cell^ = {}
	cell.index = index
	cell.generation = generation
	cell.state = .Free
	release_cell_index(pool, index)
}

release_graph_storage :: proc(graph: ^Graph) {
	pool := graph.pool
	for index in graph.cell_indices {
		release_cell(pool_cell(pool, index))
	}
	for index in graph.edge_indices {
		pool_edge(pool, index)^ = Task_Edge {
			next = INDEX_NONE,
		}
		release_edge_index(pool, index)
	}
}

complete_empty_graph :: proc(graph: ^Graph) {
	sync.atomic_store_explicit(&graph.completed, true, .Release)
	sync.sema_post(&graph.available)
	if graph.detached && graph.owned {
		allocator := graph.object_allocator
		graph_destroy(graph)
		free(graph, allocator)
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
		if sync.atomic_load_explicit(&pool.shutting_down, .Acquire) {
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
		other := &worker.pool.workers[i]
		if index, ok := mpmc_index_ring_dequeue(&other.ingress); ok {
			sync.atomic_add_explicit(&worker.pool.steal_count, 1, .Relaxed)
			execute_index(worker.pool, index)
			return true
		}
		if index, ok := work_deque_steal(&other.deque); ok {
			sync.atomic_add_explicit(&worker.pool.steal_count, 1, .Relaxed)
			execute_index(worker.pool, index)
			return true
		}
	}
	return false
}

execute_index :: proc(pool: ^Pool, index: u32) {
	cell := pool_cell(pool, index)
	worker := current_worker
	if worker != nil {
		worker_execute_cell(worker, cell)
		return
	}
	execute_cell(cell)
}

worker_execute_cell :: proc(worker: ^Worker, cell: ^Task_Cell) {
	if !begin_execute_cell(cell) {
		return
	}

	previous_temp_allocator := context.temp_allocator
	context.temp_allocator = worker.temp_allocator
	worker.temp_depth += 1
	temp := virtual.arena_temp_begin(&worker.temp_arena)

	invoke_cell(cell)

	virtual.arena_temp_end(temp)
	worker.temp_depth -= 1
	if worker.temp_depth == 0 {
		virtual.arena_free_all(&worker.temp_arena)
	}
	context.temp_allocator = previous_temp_allocator
	finish_cell(cell)
}

main_executor_execute_cell :: proc(main: ^Main_Executor, cell: ^Task_Cell) {
	if !begin_execute_cell(cell) {
		return
	}

	previous_main_executor := current_main_executor
	previous_temp_allocator := context.temp_allocator
	current_main_executor = main
	context.temp_allocator = main.temp_allocator

	main.temp_depth += 1
	temp := virtual.arena_temp_begin(&main.temp_arena)

	invoke_cell(cell)

	virtual.arena_temp_end(temp)
	main.temp_depth -= 1
	if main.temp_depth == 0 {
		virtual.arena_free_all(&main.temp_arena)
	}
	current_main_executor = previous_main_executor
	context.temp_allocator = previous_temp_allocator
	finish_cell(cell)
}

help_or_yield :: proc(pool: ^Pool) {
	worker := current_worker
	if worker != nil && worker.pool == pool && execute_next(worker) {
		return
	}
	thread.yield()
}

normalize_options :: proc(options: Options) -> Options {
	result := options
	limit := recommended_worker_count()
	if result.worker_count < 0 {
		result.worker_count = limit
	} else if result.worker_count > limit {
		result.worker_count = limit
	}
	result.task_capacity = normalize_capacity(result.task_capacity, 1024, 1)
	result.queue_capacity = normalize_capacity(result.queue_capacity, 1024, 1)
	result.deque_capacity = normalize_capacity(result.deque_capacity, 1024, 1)
	result.edge_capacity = normalize_capacity(result.edge_capacity, 1024, 1)
	result.main_queue_capacity = normalize_capacity(result.main_queue_capacity, 1024, 2)
	return result
}

normalize_capacity :: proc(value, default, minimum: int) -> int {
	needed := value
	if value <= 0 {
		needed = default
	}
	needed = max(needed, minimum)
	result := 1
	for result < needed {
		result *= 2
	}
	return result
}
