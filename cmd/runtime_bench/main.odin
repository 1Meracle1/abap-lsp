package main

import execution "../../src/execution"
import runtime "../../src/runtime"

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:time"

Bench_Config :: struct {
	count:          int,
	workers:        int,
	depth:          int,
	repeat:         int,
	queue_capacity: int,
	task_capacity:  int,
	scenario:       string,
}

Bench_Result :: struct {
	name:           string,
	count:          int,
	depth:          int,
	node_count:     int,
	sum:            int,
	expected:       int,
	errors:         int,
	total_seconds:  f64,
	submit_seconds: f64,
	wait_seconds:   f64,
	submitted:      u64,
	completed:      u64,
	queue_full:     u64,
	steals:         u64,
}

Bench_Summary :: struct {
	name:   string,
	last:   Bench_Result,
	best:   f64,
	median: f64,
}

Bench_Run_Proc :: #type proc(Bench_Config) -> Bench_Result

bench_inc :: proc(v: int) -> int {
	return v + 1
}

sum_values :: proc(values: []int) -> int {
	total := 0
	for value in values {
		total += value
	}
	return total
}

run_runtime_flat :: proc(cfg: Bench_Config) -> Bench_Result {
	pool: runtime.Pool
	err := runtime.pool_init(
		&pool,
		runtime.Options {
			worker_count   = cfg.workers,
			task_capacity  = cfg.task_capacity,
			queue_capacity = cfg.queue_capacity,
			deque_capacity = cfg.queue_capacity,
		},
		context.allocator,
	)
	if err != .None {
		return Bench_Result{name = "runtime flat", errors = 1}
	}
	defer runtime.pool_destroy(&pool)

	tasks := make([]runtime.Task(int), cfg.count, context.allocator)
	defer delete(tasks, context.allocator)

	total_start := time.tick_now()
	submit_start := total_start
	for i in 0 ..< cfg.count {
		task, submit_err := runtime.submit_value(&pool, i, bench_inc)
		if submit_err != .None {
			return Bench_Result{name = "runtime flat", errors = 1}
		}
		tasks[i] = task
	}
	if start_err := runtime.pool_start(&pool); start_err != .None {
		return Bench_Result{name = "runtime flat", errors = 1}
	}
	submit_elapsed := time.tick_since(submit_start)

	wait_start := time.tick_now()
	sum := 0
	errors := 0
	for task in tasks {
		value, wait_err := runtime.wait(task)
		if wait_err != .None {
			errors += 1
		}
		sum += value
	}
	wait_elapsed := time.tick_since(wait_start)
	total_elapsed := time.tick_since(total_start)
	stats := runtime.pool_stats(&pool)
	runtime.pool_join(&pool)

	return Bench_Result {
		name           = "runtime flat",
		count          = cfg.count,
		depth          = 1,
		node_count     = cfg.count,
		sum            = sum,
		expected       = expected_sum(cfg.count, 1),
		errors         = errors,
		total_seconds  = time.duration_seconds(total_elapsed),
		submit_seconds = time.duration_seconds(submit_elapsed),
		wait_seconds   = time.duration_seconds(wait_elapsed),
		submitted      = stats.submitted,
		completed      = stats.completed,
		queue_full     = stats.queue_full,
		steals         = stats.steals,
	}
}

run_runtime_chain :: proc(cfg: Bench_Config) -> Bench_Result {
	pool: runtime.Pool
	err := runtime.pool_init(
		&pool,
		runtime.Options {
			worker_count   = cfg.workers,
			task_capacity  = cfg.task_capacity,
			queue_capacity = cfg.queue_capacity,
			deque_capacity = cfg.queue_capacity,
		},
		context.allocator,
	)
	if err != .None {
		return Bench_Result{name = "runtime chain", errors = 1}
	}
	defer runtime.pool_destroy(&pool)

	tasks := make([]runtime.Task(int), cfg.count, context.allocator)
	defer delete(tasks, context.allocator)

	total_start := time.tick_now()
	submit_start := total_start
	for i in 0 ..< cfg.count {
		task, submit_err := runtime.submit_value(&pool, i, bench_inc)
		if submit_err != .None {
			return Bench_Result{name = "runtime chain", errors = 1}
		}
		for _ in 1 ..< cfg.depth {
			task, submit_err = runtime.then(task, bench_inc)
			if submit_err != .None {
				return Bench_Result{name = "runtime chain", errors = 1}
			}
		}
		tasks[i] = task
	}
	if start_err := runtime.pool_start(&pool); start_err != .None {
		return Bench_Result{name = "runtime chain", errors = 1}
	}
	submit_elapsed := time.tick_since(submit_start)

	wait_start := time.tick_now()
	sum := 0
	errors := 0
	for task in tasks {
		value, wait_err := runtime.wait(task)
		if wait_err != .None {
			errors += 1
		}
		sum += value
	}
	wait_elapsed := time.tick_since(wait_start)
	total_elapsed := time.tick_since(total_start)
	stats := runtime.pool_stats(&pool)
	runtime.pool_join(&pool)

	return Bench_Result {
		name           = "runtime chain",
		count          = cfg.count,
		depth          = cfg.depth,
		node_count     = cfg.count * cfg.depth,
		sum            = sum,
		expected       = expected_sum(cfg.count, cfg.depth),
		errors         = errors,
		total_seconds  = time.duration_seconds(total_elapsed),
		submit_seconds = time.duration_seconds(submit_elapsed),
		wait_seconds   = time.duration_seconds(wait_elapsed),
		submitted      = stats.submitted,
		completed      = stats.completed,
		queue_full     = stats.queue_full,
		steals         = stats.steals,
	}
}

run_runtime_fan_in :: proc(cfg: Bench_Config) -> Bench_Result {
	pool: runtime.Pool
	err := runtime.pool_init(
		&pool,
		runtime.Options {
			worker_count   = cfg.workers,
			task_capacity  = cfg.task_capacity,
			queue_capacity = cfg.queue_capacity,
			deque_capacity = cfg.queue_capacity,
		},
		context.allocator,
	)
	if err != .None {
		return Bench_Result{name = "runtime fan-in", errors = 1}
	}
	defer runtime.pool_destroy(&pool)

	tasks := make([]runtime.Task(int), cfg.count, context.allocator)
	defer delete(tasks, context.allocator)

	total_start := time.tick_now()
	submit_start := total_start
	for i in 0 ..< cfg.count {
		task, submit_err := runtime.submit_value(&pool, i, bench_inc)
		if submit_err != .None {
			return Bench_Result{name = "runtime fan-in", errors = 1}
		}
		tasks[i] = task
	}
	all_task, all_err := runtime.then_all(&pool, tasks, sum_values)
	if all_err != .None {
		return Bench_Result{name = "runtime fan-in", errors = 1}
	}
	if start_err := runtime.pool_start(&pool); start_err != .None {
		return Bench_Result{name = "runtime fan-in", errors = 1}
	}
	submit_elapsed := time.tick_since(submit_start)

	wait_start := time.tick_now()
	sum, wait_err := runtime.wait(all_task)
	errors := 0
	if wait_err != .None {
		errors = 1
	}
	wait_elapsed := time.tick_since(wait_start)
	total_elapsed := time.tick_since(total_start)
	stats := runtime.pool_stats(&pool)
	runtime.pool_join(&pool)

	return Bench_Result {
		name           = "runtime fan-in",
		count          = cfg.count,
		depth          = 1,
		node_count     = cfg.count + 1,
		sum            = sum,
		expected       = expected_sum(cfg.count, 1),
		errors         = errors,
		total_seconds  = time.duration_seconds(total_elapsed),
		submit_seconds = time.duration_seconds(submit_elapsed),
		wait_seconds   = time.duration_seconds(wait_elapsed),
		submitted      = stats.submitted,
		completed      = stats.completed,
		queue_full     = stats.queue_full,
		steals         = stats.steals,
	}
}

run_execution_flat :: proc(cfg: Bench_Config) -> Bench_Result {
	pool: execution.Pool
	execution.pool_init(
		&pool,
		execution.Options {
			worker_count   = cfg.workers,
			task_capacity  = cfg.task_capacity,
			queue_capacity = cfg.queue_capacity,
			deque_capacity = cfg.queue_capacity,
			edge_capacity  = cfg.task_capacity,
		},
		context.allocator,
	)
	defer execution.pool_destroy(&pool)

	graph: execution.Graph
	execution.graph_init(&graph, &pool, context.allocator)
	defer execution.graph_destroy(&graph)

	tasks := make([]execution.Task(int), cfg.count, context.allocator)
	defer delete(tasks, context.allocator)

	total_start := time.tick_now()
	submit_start := total_start
	exec := execution.worker_executor(&pool)
	for i in 0 ..< cfg.count {
		tasks[i] = execution.submit_value(&graph, exec, i, bench_inc)
	}
	execution.graph_start(&graph)
	execution.pool_start(&pool)
	submit_elapsed := time.tick_since(submit_start)

	wait_start := time.tick_now()
	sum := 0
	errors := 0
	for task in tasks {
		sum += execution.wait(task)
	}
	execution.graph_wait(&graph)
	wait_elapsed := time.tick_since(wait_start)
	total_elapsed := time.tick_since(total_start)
	stats := execution.pool_stats(&pool)
	execution.pool_join(&pool)

	return Bench_Result {
		name           = "execution flat",
		count          = cfg.count,
		depth          = 1,
		node_count     = cfg.count,
		sum            = sum,
		expected       = expected_sum(cfg.count, 1),
		errors         = errors,
		total_seconds  = time.duration_seconds(total_elapsed),
		submit_seconds = time.duration_seconds(submit_elapsed),
		wait_seconds   = time.duration_seconds(wait_elapsed),
		submitted      = stats.submitted,
		completed      = stats.completed,
		queue_full     = stats.queue_full,
		steals         = stats.steals,
	}
}

run_execution_chain :: proc(cfg: Bench_Config) -> Bench_Result {
	pool: execution.Pool
	execution.pool_init(
		&pool,
		execution.Options {
			worker_count   = cfg.workers,
			task_capacity  = cfg.task_capacity,
			queue_capacity = cfg.queue_capacity,
			deque_capacity = cfg.queue_capacity,
			edge_capacity  = cfg.task_capacity,
		},
		context.allocator,
	)
	defer execution.pool_destroy(&pool)

	graph: execution.Graph
	execution.graph_init(&graph, &pool, context.allocator)
	defer execution.graph_destroy(&graph)

	tasks := make([]execution.Task(int), cfg.count, context.allocator)
	defer delete(tasks, context.allocator)

	total_start := time.tick_now()
	submit_start := total_start
	exec := execution.worker_executor(&pool)
	for i in 0 ..< cfg.count {
		task := execution.submit_value(&graph, exec, i, bench_inc)
		for _ in 1 ..< cfg.depth {
			task = execution.then(&graph, task, exec, bench_inc)
		}
		tasks[i] = task
	}
	execution.graph_start(&graph)
	execution.pool_start(&pool)
	submit_elapsed := time.tick_since(submit_start)

	wait_start := time.tick_now()
	sum := 0
	errors := 0
	for task in tasks {
		sum += execution.wait(task)
	}
	execution.graph_wait(&graph)
	wait_elapsed := time.tick_since(wait_start)
	total_elapsed := time.tick_since(total_start)
	stats := execution.pool_stats(&pool)
	execution.pool_join(&pool)

	return Bench_Result {
		name           = "execution chain",
		count          = cfg.count,
		depth          = cfg.depth,
		node_count     = cfg.count * cfg.depth,
		sum            = sum,
		expected       = expected_sum(cfg.count, cfg.depth),
		errors         = errors,
		total_seconds  = time.duration_seconds(total_elapsed),
		submit_seconds = time.duration_seconds(submit_elapsed),
		wait_seconds   = time.duration_seconds(wait_elapsed),
		submitted      = stats.submitted,
		completed      = stats.completed,
		queue_full     = stats.queue_full,
		steals         = stats.steals,
	}
}

run_execution_fan_in :: proc(cfg: Bench_Config) -> Bench_Result {
	pool: execution.Pool
	execution.pool_init(
		&pool,
		execution.Options {
			worker_count   = cfg.workers,
			task_capacity  = cfg.task_capacity,
			queue_capacity = cfg.queue_capacity,
			deque_capacity = cfg.queue_capacity,
			edge_capacity  = cfg.task_capacity,
		},
		context.allocator,
	)
	defer execution.pool_destroy(&pool)

	graph: execution.Graph
	execution.graph_init(&graph, &pool, context.allocator)
	defer execution.graph_destroy(&graph)

	tasks := make([]execution.Task(int), cfg.count, context.allocator)
	defer delete(tasks, context.allocator)

	total_start := time.tick_now()
	submit_start := total_start
	exec := execution.worker_executor(&pool)
	for i in 0 ..< cfg.count {
		tasks[i] = execution.submit_value(&graph, exec, i, bench_inc)
	}
	all_task := execution.then_all(&graph, tasks, exec, sum_values)
	execution.graph_start(&graph)
	execution.pool_start(&pool)
	submit_elapsed := time.tick_since(submit_start)

	wait_start := time.tick_now()
	sum := execution.wait(all_task)
	errors := 0
	execution.graph_wait(&graph)
	wait_elapsed := time.tick_since(wait_start)
	total_elapsed := time.tick_since(total_start)
	stats := execution.pool_stats(&pool)
	execution.pool_join(&pool)

	return Bench_Result {
		name           = "execution fan-in",
		count          = cfg.count,
		depth          = 1,
		node_count     = cfg.count + 1,
		sum            = sum,
		expected       = expected_sum(cfg.count, 1),
		errors         = errors,
		total_seconds  = time.duration_seconds(total_elapsed),
		submit_seconds = time.duration_seconds(submit_elapsed),
		wait_seconds   = time.duration_seconds(wait_elapsed),
		submitted      = stats.submitted,
		completed      = stats.completed,
		queue_full     = stats.queue_full,
		steals         = stats.steals,
	}
}

run_repeats :: proc(cfg: Bench_Config, run: Bench_Run_Proc) -> Bench_Summary {
	rates := make([]f64, cfg.repeat, context.allocator)
	defer delete(rates, context.allocator)

	last: Bench_Result
	for i in 0 ..< cfg.repeat {
		last = run(cfg)
		rates[i] = rate(last)
	}
	sort_f64(rates)
	return Bench_Summary {
		name   = last.name,
		last   = last,
		best   = rates[len(rates) - 1],
		median = rates[len(rates) / 2],
	}
}

run_pair :: proc(cfg: Bench_Config, name: string, runtime_run, execution_run: Bench_Run_Proc) {
	runtime_summary := run_repeats(cfg, runtime_run)
	execution_summary := run_repeats(cfg, execution_run)
	print_summary(runtime_summary)
	print_summary(execution_summary)
	ratio := execution_summary.median / max(runtime_summary.median, 1)
	status := "ok" if ratio >= 0.90 else "slow"
	fmt.printfln("%-16s comparison status=%s execution/runtime median=%.2f%%", name, status, ratio * 100)
}

rate :: proc(result: Bench_Result) -> f64 {
	return f64(result.node_count) / max(result.total_seconds, 0.000001)
}

sort_f64 :: proc(values: []f64) {
	for i in 1 ..< len(values) {
		value := values[i]
		j := i
		for j > 0 && values[j - 1] > value {
			values[j] = values[j - 1]
			j -= 1
		}
		values[j] = value
	}
}

expected_sum :: proc(count, depth: int) -> int {
	return count * (count - 1) / 2 + count * depth
}

print_summary :: proc(summary: Bench_Summary) {
	result := summary.last
	status := "ok"
	if result.errors != 0 || result.sum != result.expected {
		status = "check"
	}
	fmt.printfln(
		"%-16s status=%s repeat_last_total=%.6fs submit=%.6fs wait=%.6fs best_nodes/sec=%.0f median_nodes/sec=%.0f sum=%d expected=%d errors=%d submitted=%d completed=%d queue_full=%d steals=%d",
		summary.name,
		status,
		result.total_seconds,
		result.submit_seconds,
		result.wait_seconds,
		summary.best,
		summary.median,
		result.sum,
		result.expected,
		result.errors,
		result.submitted,
		result.completed,
		result.queue_full,
		result.steals,
	)
}

next_power_of_two :: proc(value: int) -> int {
	result := 1
	for result < value {
		result *= 2
	}
	return result
}

normalize_config :: proc(cfg: ^Bench_Config) {
	if cfg.count <= 0 {
		cfg.count = 5000
	}
	if cfg.workers < 0 {
		cfg.workers = 4
	}
	if cfg.depth <= 0 {
		cfg.depth = 4
	}
	if cfg.repeat <= 0 {
		cfg.repeat = 5
	}
	node_need := max(cfg.count * cfg.depth + 1, cfg.count + 1)
	if cfg.queue_capacity == 0 {
		cfg.queue_capacity = next_power_of_two(max(cfg.count, 1024))
	}
	if cfg.task_capacity == 0 {
		cfg.task_capacity = next_power_of_two(max(node_need, 1024))
	}
	if cfg.scenario == "" {
		cfg.scenario = "all"
	}
	cfg.queue_capacity = next_power_of_two(max(max(cfg.queue_capacity, cfg.count), 2))
	cfg.task_capacity = next_power_of_two(max(cfg.task_capacity, node_need))
}

parse_args :: proc(args: []string, cfg: ^Bench_Config) -> bool {
	cfg^ = Bench_Config{count = 5000, workers = 4, depth = 4, repeat = 5, scenario = "all"}
	for i := 0; i < len(args); i += 1 {
		arg := args[i]
		switch arg {
		case "--help", "-h":
			print_usage()
			return false
		case "--count":
			if !read_int_arg(args, &i, "--count", &cfg.count) {
				return false
			}
		case "--workers":
			if !read_int_arg(args, &i, "--workers", &cfg.workers) {
				return false
			}
		case "--depth":
			if !read_int_arg(args, &i, "--depth", &cfg.depth) {
				return false
			}
		case "--repeat":
			if !read_int_arg(args, &i, "--repeat", &cfg.repeat) {
				return false
			}
		case "--queue-capacity":
			if !read_int_arg(args, &i, "--queue-capacity", &cfg.queue_capacity) {
				return false
			}
		case "--task-capacity":
			if !read_int_arg(args, &i, "--task-capacity", &cfg.task_capacity) {
				return false
			}
		case "--scenario":
			if !read_string_arg(args, &i, "--scenario", &cfg.scenario) {
				return false
			}
		case:
			fmt.eprintf("unknown argument: %s\n", arg)
			print_usage()
			return false
		}
	}
	normalize_config(cfg)
	return true
}

read_int_arg :: proc(args: []string, index: ^int, flag: string, out: ^int) -> bool {
	next := index^ + 1
	if next >= len(args) {
		fmt.eprintf("expected value after %s\n", flag)
		return false
	}
	value, ok := strconv.parse_int(args[next], 10)
	if !ok {
		fmt.eprintf("invalid value for %s: %s\n", flag, args[next])
		return false
	}
	out^ = value
	index^ = next
	return true
}

read_string_arg :: proc(args: []string, index: ^int, flag: string, out: ^string) -> bool {
	next := index^ + 1
	if next >= len(args) {
		fmt.eprintf("expected value after %s\n", flag)
		return false
	}
	out^ = args[next]
	index^ = next
	return true
}

print_usage :: proc() {
	fmt.println("runtime_bench [--count N] [--workers N] [--depth N] [--repeat N] [--queue-capacity N] [--task-capacity N] [--scenario all|flat|chain|fan-in|runtime-flat|execution-flat|runtime-chain|execution-chain|runtime-fan-in|execution-fan-in]")
}

main :: proc() {
	cfg: Bench_Config
	if !parse_args(os.args[1:], &cfg) {
		return
	}

	fmt.printfln(
		"runtime/execution benchmark: count=%d workers=%d depth=%d repeat=%d queue_capacity=%d task_capacity=%d scenario=%s",
		cfg.count,
		cfg.workers,
		cfg.depth,
		cfg.repeat,
		cfg.queue_capacity,
		cfg.task_capacity,
		cfg.scenario,
	)

	switch cfg.scenario {
	case "", "all":
		run_pair(cfg, "flat", run_runtime_flat, run_execution_flat)
		run_pair(cfg, "chain", run_runtime_chain, run_execution_chain)
		run_pair(cfg, "fan-in", run_runtime_fan_in, run_execution_fan_in)
	case "flat":
		run_pair(cfg, "flat", run_runtime_flat, run_execution_flat)
	case "chain":
		run_pair(cfg, "chain", run_runtime_chain, run_execution_chain)
	case "fan-in":
		run_pair(cfg, "fan-in", run_runtime_fan_in, run_execution_fan_in)
	case "runtime-flat":
		print_summary(run_repeats(cfg, run_runtime_flat))
	case "execution-flat":
		print_summary(run_repeats(cfg, run_execution_flat))
	case "runtime-chain":
		print_summary(run_repeats(cfg, run_runtime_chain))
	case "execution-chain":
		print_summary(run_repeats(cfg, run_execution_chain))
	case "runtime-fan-in":
		print_summary(run_repeats(cfg, run_runtime_fan_in))
	case "execution-fan-in":
		print_summary(run_repeats(cfg, run_execution_fan_in))
	case:
		fmt.eprintf("unknown scenario: %s\n", cfg.scenario)
		print_usage()
	}
}
