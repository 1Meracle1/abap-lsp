package persistence

import "base:runtime"
import "core:fmt"
import "core:mem"
import "core:strings"
import "core:sync/chan"
import "core:thread"
import "sqlite3"

Task_Callback :: #type proc(
	main_allocator: runtime.Allocator,
	conn: rawptr,
	user_ptr: rawptr,
) -> (
	query_result: Query_Result,
	ok: bool,
	err_descr: cstring,
)

Response_Callback :: #type proc(
	user_ptr: rawptr,
	query_result: Query_Result,
	ok: bool,
	err_descr: cstring,
)

@(private)
Response :: struct {
	user_ptr: rawptr,
	callback:     Response_Callback,
	query_result: Query_Result,
	ok:           bool,
	err_descr:    cstring,
}

@(private)
Task :: struct {
	user_ptr: rawptr,
	task_callback: Task_Callback,
	resp_callback: Response_Callback,
}

@(private)
Persistence :: struct {
	main_allocator:         runtime.Allocator,
	conn_str:               string,
	tasks_chan:             chan.Chan(Task),
	results_chan:           chan.Chan(Response),
	requests_worker_thread: ^thread.Thread,
}

@(private)
pers: Persistence

init :: proc(conn_str: string, allocator := context.allocator) -> (ok: bool, err_descr: string) {
	pers.main_allocator = allocator
	pers.conn_str = conn_str

	tasks_chan, task_err := chan.create_buffered(chan.Chan(Task), 10, context.allocator)
	if task_err != nil {
		return false, "failed to create persistence Task channel"
	}
	pers.tasks_chan = tasks_chan

	results_chan, resp_err := chan.create_buffered(chan.Chan(Response), 10, context.allocator)
	if resp_err != nil {
		return false, "failed to create persistence Response channel"
	}
	pers.results_chan = results_chan

	pers.requests_worker_thread = thread.create_and_start_with_poly_data(&pers, requests_handling)
	if pers.requests_worker_thread == nil {
		return false, "failed to create persistence requests handling thread"
	}

	return true, ""
}

deinit :: proc() {
	chan.close(pers.tasks_chan)
	chan.close(pers.results_chan)
	chan.destroy(pers.tasks_chan)
	chan.destroy(pers.results_chan)
	thread.join(pers.requests_worker_thread)
}

@(private)
requests_handling :: proc(pers: ^Persistence) {
	conn: sqlite3.Conn
	conn_str_c := strings.clone_to_cstring(pers.conn_str, context.temp_allocator)
	if rc := sqlite3.open(conn_str_c, &conn); rc != sqlite3.OK {
		fmt.panicf("failed to open sqlite3 db connection: %v, %s", rc, sqlite3.errmsg(conn))
	}
	defer sqlite3.close(conn)

	for {
		task, ok := chan.recv(pers.tasks_chan)
		if !ok {
			break
		}
		query_result, task_ok, task_err_descr := task.task_callback(
			pers.main_allocator,
			conn,
			task.user_ptr,
		)
		ok = chan.send(
			pers.results_chan,
			Response {
				user_ptr = task.user_ptr,
				callback = task.resp_callback,
				query_result = query_result,
				ok = task_ok,
				err_descr = task_err_descr,
			},
		)
		if !ok {
			break
		}
		mem.free_all(context.temp_allocator)
	}
}

try_recv :: proc() {
	for {
		response, ok := chan.try_recv(pers.results_chan)
		if !ok {
			break
		}
		response.callback(
			response.user_ptr,
			response.query_result,
			response.ok,
			response.err_descr,
		)
		query_result_deinit(&response.query_result)
	}
}

send :: proc(
	user_ptr: rawptr,
	request_handler: Task_Callback,
	response_handler: Response_Callback,
) -> bool {
	return chan.send(
		pers.tasks_chan,
		Task{user_ptr, request_handler, response_handler},
	)
}
