#!/usr/bin/env python3
"""Verify the ABAP language server returns rv_res completion over stdio."""

from __future__ import annotations

import argparse
import json
import queue
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path
from typing import Any, Callable


SOURCE = """CLASS lcl_class DEFINITION.
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
ENDCLASS."""


def parse_args() -> argparse.Namespace:
    repo_root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(
        description=(
            "Start abap_language_server over stdio and verify completion at "
            "the incomplete rv_ method body statement returns rv_res."
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
    return parser.parse_args()


def build_debug_binary(repo_root: Path) -> None:
    subprocess.run(["cmd", "/c", str(repo_root / "build.bat")], cwd=repo_root, check=True)


def source_position_after_rv_prefix() -> dict[str, int]:
    offset = SOURCE.rindex("rv_") + len("rv_")
    line = SOURCE[:offset].count("\n")
    line_start = SOURCE.rfind("\n", 0, offset) + 1
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


def run_probe(server: Path, timeout: float) -> tuple[dict[str, Any], list[dict[str, Any]], list[str]]:
    with tempfile.TemporaryDirectory(prefix="abap_lsp_completion_") as temp_root:
        root = Path(temp_root).resolve()
        uri = (root / "completion_method_body_signature.abap").as_uri()
        root_uri = root.as_uri()
        position = source_position_after_rv_prefix()

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
                            "text": SOURCE,
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

    try:
        completion, seen, stderr_lines = run_probe(server, args.timeout)
    except Exception as exc:
        print(f"completion e2e probe failed: {exc}", file=sys.stderr)
        return 1

    items = completion_items(completion)
    rv_res = next((item for item in items if item.get("label") == "rv_res"), None)
    diagnostics = [
        message.get("params", {}).get("diagnostics", [])
        for message in seen
        if message.get("method") == "textDocument/publishDiagnostics"
    ]

    summary = {
        "passed": rv_res is not None,
        "position": source_position_after_rv_prefix(),
        "item_count": len(items),
        "labels": [item.get("label") for item in items],
        "rv_res_item": rv_res,
        "diagnostics": diagnostics[-1] if diagnostics else [],
        "stderr": stderr_lines,
    }
    print(json.dumps(summary, indent=2))
    return 0 if rv_res is not None else 1


if __name__ == "__main__":
    raise SystemExit(main())
