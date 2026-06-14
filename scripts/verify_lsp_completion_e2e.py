#!/usr/bin/env python3
"""Verify ABAP language-server completion scenarios over stdio."""

from __future__ import annotations

import argparse
import json
import queue
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable

try:
    import tomllib
except ModuleNotFoundError:  # pragma: no cover - Python < 3.11 fallback
    tomllib = None


@dataclass(frozen=True)
class CompletionCase:
    name: str
    file_name: str
    description: str
    source: str
    marker: str
    expected_label: str
    expected_filter_text: str | None = None
    expected_text_edit_new_text: str | None = None
    require_text_edit_from_marker: str | None = None
    initial_source: str | None = None
    expected_absent_labels: tuple[str, ...] = ()


COMPLETION_CASES: dict[str, CompletionCase] = {
    "method-body-signature": CompletionCase(
        name="method-body-signature",
        file_name="completion_method_body_signature.abap",
        description="incomplete rv_ method body statement returns rv_res",
        source="""CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something
      IMPORTING
        iv_param TYPE string
      RETURNING
        VALUE(rv_res) TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
    rv_
  ENDMETHOD.
ENDCLASS.""",
        marker="rv_",
        expected_label="rv_res",
    ),
    "me-selector": CompletionCase(
        name="me-selector",
        file_name="completion_method_body_me_selector.abap",
        description="incomplete me-> method body selector returns method_name",
        source="""CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something
      IMPORTING
        iv_param TYPE string
      RETURNING
        VALUE(rv_res) TYPE string.

    METHODS method_name
      IMPORTING
        iv_input TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
    me->
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.""",
        marker="me->",
        expected_label="method_name",
        expected_filter_text="me->method_name",
        expected_text_edit_new_text="""me->method_name(
      iv_input = $1
    )$0""",
        require_text_edit_from_marker="me->",
    ),
    "inline-new-selector-incremental": CompletionCase(
        name="inline-new-selector-incremental",
        file_name="completion_inline_new_selector_incremental.abap",
        description="incremental lo_inst-> edit returns method_name",
        source="""CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_param TYPE string
        iv_param1 TYPE i OPTIONAL.

    METHODS method_name
      IMPORTING
        !iv_value TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( 'iv_param11111' ).
DATA lo_inst1 TYPE REF TO lcl_class.
lo_inst1 = NEW #( iv_param = 'hello' ).

lo_inst->method_name( 'hello' ).
lo_inst->""",
        marker="lo_inst->",
        expected_label="method_name",
        expected_filter_text="lo_inst->method_name",
        expected_text_edit_new_text="""lo_inst->method_name(
  iv_value = $1
)$0""",
        require_text_edit_from_marker="lo_inst->",
        expected_absent_labels=("constructor",),
        initial_source="""CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_param TYPE string
        iv_param1 TYPE i OPTIONAL.

    METHODS method_name
      IMPORTING
        !iv_value TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( 'iv_param11111' ).
DATA lo_inst1 TYPE REF TO lcl_class.
lo_inst1 = NEW #( iv_param = 'hello' ).

lo_inst->method_name( 'hello' ).
""",
    ),
    "inline-new-pending-arrow": CompletionCase(
        name="inline-new-pending-arrow",
        file_name="completion_inline_new_pending_arrow.abap",
        description="lo_inst- trigger returns method_name and inserts ->",
        source="""CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_param TYPE string
        iv_param1 TYPE i OPTIONAL.

    METHODS method_name
      IMPORTING
        !iv_value TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( 'iv_param11111' ).
lo_inst-""",
        marker="lo_inst-",
        expected_label="method_name",
        expected_filter_text="lo_inst->method_name",
        expected_text_edit_new_text="""lo_inst->method_name(
  iv_value = $1
)$0""",
        require_text_edit_from_marker="lo_inst-",
        expected_absent_labels=("constructor",),
    ),
}


def parse_args() -> argparse.Namespace:
    repo_root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(
        description=(
            "Start abap_language_server over stdio and verify completion "
            "scenarios used by editors."
        )
    )
    parser.add_argument(
        "--server",
        type=Path,
        default=repo_root / "bin" / "debug" / "abap_language_server.exe",
        help="Path to abap_language_server.exe.",
    )
    parser.add_argument(
        "--skip-build",
        action="store_true",
        help="Do not run build.bat before starting the server.",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=10.0,
        help="Seconds to wait for each expected LSP response.",
    )
    parser.add_argument(
        "--case",
        choices=["all", *COMPLETION_CASES.keys()],
        default="all",
        help="Completion scenario to run. Defaults to all.",
    )
    return parser.parse_args()


def build_debug_binary(repo_root: Path) -> None:
    subprocess.run(["cmd", "/c", str(repo_root / "build.bat")], cwd=repo_root, check=True)


def source_position_after_marker(source: str, marker: str) -> dict[str, int]:
    offset = source_offset_after_marker(source, marker)
    line = source[:offset].count("\n")
    line_start = source.rfind("\n", 0, offset) + 1
    return {"line": line, "character": offset - line_start}


def source_offset_after_marker(source: str, marker: str) -> int:
    return source.rindex(marker) + len(marker)


def source_position_at_marker(source: str, marker: str) -> dict[str, int]:
    offset = source.rindex(marker)
    return source_position_at_offset(source, offset)


def source_position_at_offset(source: str, offset: int) -> dict[str, int]:
    line = source[:offset].count("\n")
    line_start = source.rfind("\n", 0, offset) + 1
    return {"line": line, "character": offset - line_start}


def send_message(proc: subprocess.Popen[bytes], message: dict[str, Any]) -> None:
    payload = json.dumps(message, separators=(",", ":")).encode("utf-8")
    header = f"Content-Length: {len(payload)}\r\n\r\n".encode("ascii")
    assert proc.stdin is not None
    proc.stdin.write(header + payload)
    proc.stdin.flush()


def read_frames(stdout: Any, frames: "queue.Queue[dict[str, Any]]") -> None:
    try:
        while True:
            header = b""
            while b"\r\n\r\n" not in header:
                byte = stdout.read(1)
                if not byte:
                    return
                header += byte

            content_length = None
            for line in header.split(b"\r\n"):
                if line.lower().startswith(b"content-length:"):
                    content_length = int(line.split(b":", 1)[1].strip())
                    break
            if content_length is None:
                frames.put({"_error": "missing Content-Length header"})
                return

            payload = stdout.read(content_length)
            frames.put(json.loads(payload.decode("utf-8")))
    except Exception as exc:  # pragma: no cover - diagnostic path
        frames.put({"_error": repr(exc)})


def read_stderr(stderr: Any, lines: "queue.Queue[str]") -> None:
    try:
        for line in stderr:
            lines.put(line.decode("utf-8", "replace").rstrip())
    except Exception as exc:  # pragma: no cover - diagnostic path
        lines.put(f"stderr reader failed: {exc!r}")


def recv_until(
    frames: "queue.Queue[dict[str, Any]]",
    predicate: Callable[[dict[str, Any]], bool],
    timeout: float,
    seen: list[dict[str, Any]],
) -> dict[str, Any]:
    deadline = time.monotonic() + timeout
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError("timed out waiting for expected LSP frame")
        message = frames.get(timeout=remaining)
        seen.append(message)
        if "_error" in message:
            raise RuntimeError(message["_error"])
        if predicate(message):
            return message


def completion_items(response: dict[str, Any]) -> list[dict[str, Any]]:
    result = response.get("result")
    if isinstance(result, dict):
        items = result.get("items", [])
    elif isinstance(result, list):
        items = result
    else:
        items = []
    return items if isinstance(items, list) else []


def expected_item_errors(case: CompletionCase, item: dict[str, Any] | None) -> list[str]:
    errors: list[str] = []
    if item is None:
        return [f"missing expected label {case.expected_label!r}"]

    if case.expected_filter_text is not None:
        actual_filter_text = item.get("filterText")
        if actual_filter_text != case.expected_filter_text:
            errors.append(
                "expected filterText "
                f"{case.expected_filter_text!r}, got {actual_filter_text!r}"
            )

    if case.expected_text_edit_new_text is not None:
        edit = item.get("textEdit")
        actual_new_text = edit.get("newText") if isinstance(edit, dict) else None
        if actual_new_text != case.expected_text_edit_new_text:
            errors.append(
                "expected textEdit.newText "
                f"{case.expected_text_edit_new_text!r}, got {actual_new_text!r}"
            )

    if case.require_text_edit_from_marker is not None:
        edit = item.get("textEdit")
        if not isinstance(edit, dict):
            errors.append("expected textEdit object")
        else:
            expected_start = source_position_at_marker(
                case.source, case.require_text_edit_from_marker
            )
            expected_end = source_position_after_marker(
                case.source, case.require_text_edit_from_marker
            )
            actual_range = edit.get("range")
            actual_start = (
                actual_range.get("start") if isinstance(actual_range, dict) else None
            )
            actual_end = (
                actual_range.get("end") if isinstance(actual_range, dict) else None
            )
            if actual_start != expected_start or actual_end != expected_end:
                errors.append(
                    "expected textEdit range "
                    f"{expected_start!r}..{expected_end!r}, got "
                    f"{actual_start!r}..{actual_end!r}"
                )

    return errors


def absent_item_errors(case: CompletionCase, items: list[dict[str, Any]]) -> list[str]:
    labels = {item.get("label") for item in items}
    return [
        f"unexpected completion label {label!r}"
        for label in case.expected_absent_labels
        if label in labels
    ]


def zed_completion_query_characters(repo_root: Path) -> set[str]:
    config_path = repo_root / "editors" / "zed" / "languages" / "abap" / "config.toml"
    if not config_path.exists():
        return set()

    if tomllib is not None:
        data = tomllib.loads(config_path.read_text(encoding="utf-8"))
        values = data.get("completion_query_characters", [])
    else:
        values = []
        for line in config_path.read_text(encoding="utf-8").splitlines():
            stripped = line.split("#", 1)[0].strip()
            if stripped.startswith("["):
                break
            if stripped.startswith("completion_query_characters"):
                _, raw_values = stripped.split("=", 1)
                values = json.loads(raw_values)
                break

    return {value for value in values if isinstance(value, str) and len(value) == 1}


def zed_char_kind(char: str, completion_query_characters: set[str]) -> str:
    if char.isalnum() or char == "_" or char in completion_query_characters:
        return "word"
    if char.isspace():
        return "whitespace"
    return "punctuation"


def zed_completion_query(
    source: str,
    offset: int,
    completion_query_characters: set[str],
) -> str | None:
    order = {"whitespace": 0, "punctuation": 1, "word": 2}
    neighboring_kinds: list[str] = []
    if offset > 0:
        neighboring_kinds.append(
            zed_char_kind(source[offset - 1], completion_query_characters)
        )
    if offset < len(source):
        neighboring_kinds.append(zed_char_kind(source[offset], completion_query_characters))
    if not neighboring_kinds:
        return None

    word_kind = max(neighboring_kinds, key=lambda kind: order[kind])
    start = offset
    while (
        start > 0
        and source[start - 1] != "\n"
        and zed_char_kind(source[start - 1], completion_query_characters) == word_kind
    ):
        start -= 1

    end = offset
    while (
        end < len(source)
        and source[end] != "\n"
        and zed_char_kind(source[end], completion_query_characters) == word_kind
    ):
        end += 1

    if offset > start and word_kind == "word":
        return source[start:offset]
    return None


def zed_effective_filter_text(item: dict[str, Any]) -> str:
    label = str(item.get("label", ""))
    detail = item.get("detail")
    label_details = item.get("labelDetails")
    description = (
        label_details.get("description")
        if isinstance(label_details, dict)
        else None
    )
    if isinstance(detail, str) and detail != label:
        text = f"{label} {detail}"
    elif isinstance(description, str) and description != label:
        text = f"{label} {description}"
    else:
        text = label

    filter_text = item.get("filterText")
    if isinstance(filter_text, str):
        start = text.find(filter_text)
        if start >= 0:
            return text[start : start + len(filter_text)]
    return text[: len(label)]


def zed_fuzzy_subsequence_matches(query: str, candidate: str) -> bool:
    candidate_index = 0
    candidate_lower = candidate.lower()
    for query_char in query.lower():
        found = candidate_lower.find(query_char, candidate_index)
        if found < 0:
            return False
        candidate_index = found + 1
    return True


def run_probe(
    server: Path,
    timeout: float,
    case: CompletionCase,
) -> tuple[dict[str, Any], list[dict[str, Any]], list[str]]:
    with tempfile.TemporaryDirectory(prefix="abap_lsp_completion_") as temp_root:
        root = Path(temp_root).resolve()
        uri = (root / case.file_name).as_uri()
        root_uri = root.as_uri()
        position = source_position_after_marker(case.source, case.marker)
        open_source = case.initial_source if case.initial_source is not None else case.source

        proc = subprocess.Popen(
            [str(server), "--disable-adt-dependency-fetch"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            cwd=server.parent,
        )
        assert proc.stdout is not None
        assert proc.stderr is not None

        frames: "queue.Queue[dict[str, Any]]" = queue.Queue()
        stderr_lines: "queue.Queue[str]" = queue.Queue()
        threading.Thread(target=read_frames, args=(proc.stdout, frames), daemon=True).start()
        threading.Thread(target=read_stderr, args=(proc.stderr, stderr_lines), daemon=True).start()

        seen: list[dict[str, Any]] = []
        try:
            send_message(
                proc,
                {
                    "jsonrpc": "2.0",
                    "id": 1,
                    "method": "initialize",
                    "params": {
                        "processId": None,
                        "rootUri": root_uri,
                        "capabilities": {
                            "textDocument": {
                                "completion": {
                                    "completionItem": {"snippetSupport": True},
                                }
                            }
                        },
                    },
                },
            )
            recv_until(frames, lambda message: message.get("id") == 1, timeout, seen)

            send_message(proc, {"jsonrpc": "2.0", "method": "initialized", "params": {}})
            send_message(
                proc,
                {
                    "jsonrpc": "2.0",
                    "method": "textDocument/didOpen",
                    "params": {
                        "textDocument": {
                            "uri": uri,
                            "languageId": "abap",
                            "version": 1,
                            "text": open_source,
                        }
                    },
                },
            )
            recv_until(
                frames,
                lambda message: message.get("method") == "textDocument/publishDiagnostics",
                timeout,
                seen,
            )

            if case.initial_source is not None:
                if not case.source.startswith(case.initial_source):
                    raise ValueError(f"{case.name}: source must extend initial_source")
                edit_text = case.source[len(case.initial_source) :]
                edit_position = source_position_at_offset(
                    case.initial_source,
                    len(case.initial_source),
                )
                send_message(
                    proc,
                    {
                        "jsonrpc": "2.0",
                        "method": "textDocument/didChange",
                        "params": {
                            "textDocument": {"uri": uri, "version": 2},
                            "contentChanges": [
                                {
                                    "range": {
                                        "start": edit_position,
                                        "end": edit_position,
                                    },
                                    "text": edit_text,
                                }
                            ],
                        },
                    },
                )
                recv_until(
                    frames,
                    lambda message: message.get("method") == "textDocument/publishDiagnostics",
                    timeout,
                    seen,
                )

            send_message(
                proc,
                {
                    "jsonrpc": "2.0",
                    "id": 2,
                    "method": "textDocument/completion",
                    "params": {
                        "textDocument": {"uri": uri},
                        "position": position,
                    },
                },
            )
            completion = recv_until(frames, lambda message: message.get("id") == 2, timeout, seen)

            send_message(proc, {"jsonrpc": "2.0", "id": 3, "method": "shutdown", "params": None})
            recv_until(frames, lambda message: message.get("id") == 3, timeout, seen)
            send_message(proc, {"jsonrpc": "2.0", "method": "exit", "params": None})
        finally:
            try:
                if proc.stdin is not None:
                    proc.stdin.close()
            except BrokenPipeError:
                pass
            try:
                proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait(timeout=5)

        return completion, seen, list(stderr_lines.queue)


def run_case(
    server: Path,
    timeout: float,
    case: CompletionCase,
    zed_query_characters: set[str],
) -> dict[str, Any]:
    completion, seen, stderr_lines = run_probe(server, timeout, case)
    items = completion_items(completion)
    expected = next((item for item in items if item.get("label") == case.expected_label), None)
    errors = expected_item_errors(case, expected)
    errors.extend(absent_item_errors(case, items))
    zed_query: str | None = None
    zed_filter_text: str | None = None
    zed_visible: bool | None = None
    if expected is not None:
        offset = source_offset_after_marker(case.source, case.marker)
        zed_query = zed_completion_query(case.source, offset, zed_query_characters)
        zed_filter_text = zed_effective_filter_text(expected)
        zed_visible = zed_fuzzy_subsequence_matches(zed_query or "", zed_filter_text)
        if not zed_visible:
            errors.append(
                "expected item would be hidden by Zed filtering: "
                f"query {zed_query!r} does not match effective filter text "
                f"{zed_filter_text!r}"
            )
    diagnostics = [
        message.get("params", {}).get("diagnostics", [])
        for message in seen
        if message.get("method") == "textDocument/publishDiagnostics"
    ]
    return {
        "case": case.name,
        "passed": not errors,
        "description": case.description,
        "position": source_position_after_marker(case.source, case.marker),
        "expected_label": case.expected_label,
        "item_count": len(items),
        "labels": [item.get("label") for item in items],
        "expected_item": expected,
        "zed_query_characters": sorted(zed_query_characters),
        "zed_query": zed_query,
        "zed_filter_text": zed_filter_text,
        "zed_visible": zed_visible,
        "errors": errors,
        "diagnostics": diagnostics[-1] if diagnostics else [],
        "stderr": stderr_lines,
    }


def main() -> int:
    args = parse_args()
    repo_root = Path(__file__).resolve().parents[1]
    server = args.server
    if not server.is_absolute():
        server = repo_root / server

    if not args.skip_build:
        build_debug_binary(repo_root)

    if not server.exists():
        print(f"server binary not found: {server}", file=sys.stderr)
        return 2

    cases = (
        list(COMPLETION_CASES.values())
        if args.case == "all"
        else [COMPLETION_CASES[args.case]]
    )
    zed_query_characters = zed_completion_query_characters(repo_root)
    case_results: list[dict[str, Any]] = []
    for case in cases:
        try:
            case_results.append(run_case(server, args.timeout, case, zed_query_characters))
        except Exception as exc:
            print(f"completion e2e probe failed for {case.name}: {exc}", file=sys.stderr)
            return 1

    summary = {
        "passed": all(result["passed"] for result in case_results),
        "cases": case_results,
    }
    print(json.dumps(summary, indent=2))
    return 0 if summary["passed"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
