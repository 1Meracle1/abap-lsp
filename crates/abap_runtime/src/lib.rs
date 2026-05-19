use std::cell::Cell;
use std::future::Future;
use std::panic::{AssertUnwindSafe, resume_unwind};
use std::pin::Pin;
use std::sync::OnceLock;
use std::task::{Context, Poll};
use std::thread;

thread_local! {
    static RUNTIME_WORKER_THREAD: Cell<bool> = const { Cell::new(false) };
}

pub struct Executor {
    workers: usize,
    runtime: Option<tokio::runtime::Runtime>,
}

pub struct Task<T> {
    inner: TaskInner<T>,
}

enum TaskInner<T> {
    Ready(Option<thread::Result<T>>),
    Spawned {
        handle: tokio::runtime::Handle,
        task: tokio::task::JoinHandle<T>,
    },
}

impl Executor {
    fn new(workers: usize) -> Self {
        let runtime = (workers > 0).then(|| {
            tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .worker_threads(workers)
                .thread_name("abap-runtime")
                .on_thread_start(|| RUNTIME_WORKER_THREAD.with(|flag| flag.set(true)))
                .on_thread_stop(|| RUNTIME_WORKER_THREAD.with(|flag| flag.set(false)))
                .build()
                .expect("global abap runtime should start")
        });
        Self { workers, runtime }
    }

    fn spawn_cpu<T>(&self, job: impl FnOnce() -> T + Send + 'static) -> Task<T>
    where
        T: Send + 'static,
    {
        let Some(runtime) = &self.runtime else {
            return Task {
                inner: TaskInner::Ready(Some(std::panic::catch_unwind(AssertUnwindSafe(job)))),
            };
        };
        Task {
            inner: TaskInner::Spawned {
                handle: runtime.handle().clone(),
                task: runtime.spawn(async move { job() }),
            },
        }
    }

    fn spawn_async<F>(&self, future: F) -> Task<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        let Some(runtime) = &self.runtime else {
            let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
                tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .expect("inline abap runtime should start")
                    .block_on(future)
            }));
            return Task {
                inner: TaskInner::Ready(Some(result)),
            };
        };
        Task {
            inner: TaskInner::Spawned {
                handle: runtime.handle().clone(),
                task: runtime.spawn(future),
            },
        }
    }

    fn run_cpu_batch<I, O, F>(&'static self, items: Vec<I>, f: F) -> Vec<O>
    where
        I: Send + 'static,
        O: Send + 'static,
        F: Fn(I) -> O + Send + Sync + 'static,
    {
        let len = items.len();
        if len < 2 || self.workers == 0 {
            return items.into_iter().map(f).collect();
        }

        let task_count = len.min(self.workers);
        let chunk_size = len.div_ceil(task_count);
        let f = std::sync::Arc::new(f);
        let mut chunks = Vec::new();
        let mut iter = items.into_iter().enumerate();
        loop {
            let chunk: Vec<_> = iter.by_ref().take(chunk_size).collect();
            if chunk.is_empty() {
                break;
            }
            chunks.push(chunk);
        }

        let tasks: Vec<_> = chunks
            .into_iter()
            .map(|chunk| {
                let f = std::sync::Arc::clone(&f);
                self.spawn_cpu(move || {
                    chunk
                        .into_iter()
                        .map(|(idx, item)| (idx, f(item)))
                        .collect::<Vec<_>>()
                })
            })
            .collect();
        let mut out = (0..len).map(|_| None).collect::<Vec<_>>();
        for task in tasks {
            for (idx, value) in task.join() {
                out[idx] = Some(value);
            }
        }
        out.into_iter()
            .map(|value| value.expect("batch task should fill every result"))
            .collect()
    }
}

impl<T> Future for Task<T>
where
    T: Send + 'static,
{
    type Output = T;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match &mut self.inner {
            TaskInner::Ready(result) => Poll::Ready(resolve_task_result(
                result.take().expect("task polled after finish"),
            )),
            TaskInner::Spawned { task, .. } => match Pin::new(task).poll(cx) {
                Poll::Ready(result) => Poll::Ready(resolve_join_result(result)),
                Poll::Pending => Poll::Pending,
            },
        }
    }
}

impl<T> Unpin for Task<T> {}

impl<T> Task<T>
where
    T: Send + 'static,
{
    pub fn join(self) -> T {
        match self.inner {
            TaskInner::Ready(mut result) => {
                resolve_task_result(result.take().expect("task joined after finish"))
            }
            TaskInner::Spawned { handle, task } => {
                let wait = move || handle.block_on(async move { resolve_join_result(task.await) });
                if in_runtime_worker_thread() {
                    tokio::task::block_in_place(wait)
                } else if tokio::runtime::Handle::try_current().is_ok() {
                    thread::spawn(wait)
                        .join()
                        .unwrap_or_else(|panic| resume_unwind(panic))
                } else {
                    wait()
                }
            }
        }
    }
}

pub fn init_global_executor() -> &'static Executor {
    global_executor()
}

pub fn global_executor() -> &'static Executor {
    static EXECUTOR: OnceLock<Executor> = OnceLock::new();
    EXECUTOR.get_or_init(|| Executor::new(default_worker_thread_count()))
}

pub fn worker_thread_count() -> usize {
    global_executor().workers
}

pub fn spawn_cpu<T>(job: impl FnOnce() -> T + Send + 'static) -> Task<T>
where
    T: Send + 'static,
{
    global_executor().spawn_cpu(job)
}

pub fn spawn_async<F>(future: F) -> Task<F::Output>
where
    F: Future + Send + 'static,
    F::Output: Send + 'static,
{
    global_executor().spawn_async(future)
}

pub fn run_cpu_batch<I, O, F>(items: Vec<I>, f: F) -> Vec<O>
where
    I: Send + 'static,
    O: Send + 'static,
    F: Fn(I) -> O + Send + Sync + 'static,
{
    global_executor().run_cpu_batch(items, f)
}

fn default_worker_thread_count() -> usize {
    std::thread::available_parallelism()
        .map_or(1, |count| count.get())
        .saturating_sub(1)
}

fn in_runtime_worker_thread() -> bool {
    RUNTIME_WORKER_THREAD.with(Cell::get)
}

fn resolve_task_result<T>(result: thread::Result<T>) -> T {
    match result {
        Ok(value) => value,
        Err(panic) => resume_unwind(panic),
    }
}

fn resolve_join_result<T>(result: Result<T, tokio::task::JoinError>) -> T {
    match result {
        Ok(value) => value,
        Err(error) if error.is_panic() => resume_unwind(error.into_panic()),
        Err(error) => panic!("abap runtime task was cancelled: {error}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
    use std::time::Duration;

    #[test]
    fn worker_count_uses_available_parallelism_minus_one() {
        assert_eq!(worker_thread_count(), default_worker_thread_count());
    }

    #[test]
    fn inline_executor_runs_cpu_immediately() {
        let executor = Executor::new(0);
        let ran = Arc::new(AtomicBool::new(false));
        let task = executor.spawn_cpu({
            let ran = Arc::clone(&ran);
            move || {
                ran.store(true, Ordering::SeqCst);
                7
            }
        });
        assert!(ran.load(Ordering::SeqCst));
        assert_eq!(task.join(), 7);
    }

    #[test]
    fn run_cpu_batch_returns_results_in_input_order() {
        let out = run_cpu_batch(vec![3, 1, 2], |value| value * 10);
        assert_eq!(out, vec![30, 10, 20]);
    }

    #[test]
    fn join_propagates_panics() {
        let result = std::panic::catch_unwind(|| {
            spawn_cpu(|| panic!("boom")).join();
        });
        assert!(result.is_err());
    }

    #[test]
    fn task_can_be_awaited() {
        let value = spawn_async(async { spawn_cpu(|| 41).await + 1 }).join();
        assert_eq!(value, 42);
    }

    #[test]
    fn worker_can_spawn_and_join_nested_task() {
        let value = spawn_cpu(|| spawn_cpu(|| 20).join() + 22).join();
        assert_eq!(value, 42);
    }

    #[test]
    fn worker_can_run_nested_batch() {
        let value = spawn_cpu(|| run_cpu_batch(vec![1, 2, 3], |value| value + 1))
            .join()
            .into_iter()
            .sum::<i32>();
        assert_eq!(value, 9);
    }

    #[test]
    fn worker_join_does_not_deadlock_waiting_for_child() {
        let finished = Arc::new(AtomicUsize::new(0));
        let value = spawn_cpu({
            let finished = Arc::clone(&finished);
            move || {
                let child = spawn_cpu({
                    let finished = Arc::clone(&finished);
                    move || {
                        std::thread::sleep(Duration::from_millis(10));
                        finished.fetch_add(1, Ordering::SeqCst);
                        42
                    }
                });
                child.join()
            }
        })
        .join();
        assert_eq!(value, 42);
        assert_eq!(finished.load(Ordering::SeqCst), 1);
    }
}
