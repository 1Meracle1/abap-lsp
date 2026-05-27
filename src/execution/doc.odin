package abap_frontend_execution

// Package execution is a graph-owned task executor.
//
// A Pool owns fixed task/edge capacity and worker threads. A Graph owns the
// task DAG, task payload/result storage, and metadata allocated from
// graph.allocator. Task values returned by submit_value, then, then_with, and
// then_all are handles into that graph and are valid until graph_reset or
// graph_destroy.
//
// Basic worker-backed graph:
//
//   pool: Pool
//   pool_init(&pool, Options{worker_count = 4}, allocator)
//   pool_start(&pool)
//   defer pool_destroy(&pool)
//
//   graph: Graph
//   graph_init(&graph, &pool, allocator)
//   defer graph_destroy(&graph)
//
//   root := submit_value(&graph, worker_executor(&pool), input, work)
//   next := then(&graph, root, worker_executor(&pool), next_work)
//   graph_start(&graph)
//   value := wait(next)
//   graph_wait(&graph)
//
// graph_init initializes a caller-owned Graph value. Use graph_reset after
// graph completion to release the current tasks and submit new work into the
// same Graph, or graph_destroy when finished with the Graph. graph_create
// allocates the Graph object and marks it owned; that form is required for
// graph_detach because detached owned graphs self-destroy and free the Graph
// object when the last task completes.
//
// Main executor:
//
// Main_Executor is for tasks that must run on the owner thread. Scheduling a
// main-executor task only enqueues it. It does not run until the owner thread
// calls main_executor_drain.
//
//   main: Main_Executor
//   main_executor_init(&main, 64, allocator)
//   defer main_executor_destroy(&main)
//
//   root := submit_value(&graph, worker_executor(&pool), input, work)
//   main_task := then_with(&graph, root, main_executor(&main), data, main_work)
//   graph_start(&graph)
//
//   for {
//       main_executor_drain(&main)
//       if value, ok := try_wait(main_task); ok {
//           break
//       }
//       thread.yield()
//   }
//
// main_executor_drain is non-blocking: it runs tasks already queued at the time
// it is called and returns the number run. It does not wait for worker tasks to
// produce future main work. Do not call wait or graph_wait from the owner thread
// while a needed task is still queued on the main executor; drain it first or
// interleave drain with try_wait.
//
// Temporary allocation:
//
// current_temp_arena returns the executing worker temp arena inside worker
// tasks, or the Main_Executor temp arena inside main_executor_drain. That arena
// is reset after the current task boundary. Use graph.allocator for data that
// must survive into later tasks or wait/try_wait results.
