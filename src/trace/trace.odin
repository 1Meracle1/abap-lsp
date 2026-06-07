package abap_frontend_trace

import "core:fmt"
import "core:time"

ENABLED :: #config(ABAP_FRONTEND_TRACE, false)

eprintf :: fmt.eprintf

now :: time.now

duration_ms_since :: proc(start: time.Time) -> f64 {
	return time.duration_milliseconds(time.since(start))
}
