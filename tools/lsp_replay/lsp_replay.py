#!/usr/bin/env python3
import argparse
import json
import os
from pathlib import Path
import queue
import shlex
import subprocess
import sys
import threading
import time


ANALYSIS_STATUS = "abapls/workspaceAnalysisStatus"
RESOLVE_REMOTE = "abapls/resolveRemoteDependencies"
REMOTE_UPDATED = "abapls/remoteDependenciesUpdated"


def read_frame(stream):
    content_length = None
    while True:
        line = stream.readline()
        if not line:
            return None
        if line in (b"\r\n", b"\n"):
            break
        name, _, value = line.partition(b":")
        if name.strip().lower() == b"content-length":
            content_length = int(value.strip())
    if content_length is None:
        raise RuntimeError("LSP frame is missing Content-Length")
    payload = stream.read(content_length)
    if len(payload) != content_length:
        raise RuntimeError("LSP frame ended early")
    return json.loads(payload.decode("utf-8"))


def write_frame(stream, message):
    payload = json.dumps(message, separators=(",", ":")).encode("utf-8")
    stream.write(b"Content-Length: " + str(len(payload)).encode("ascii") + b"\r\n\r\n")
    stream.write(payload)
    stream.flush()


def as_command(value):
    if isinstance(value, list):
        return [str(part) for part in value]
    if isinstance(value, str):
        return shlex.split(value, posix=os.name != "nt")
    raise ValueError("server command must be a string or array")


def default_server_command(cwd):
    env_command = os.environ.get("ABAP_LSP_SERVER")
    if env_command:
        return as_command(env_command)
    exe = Path(cwd) / "target" / "debug" / (
        "abap_lsp_server.exe" if os.name == "nt" else "abap_lsp_server"
    )
    if exe.exists():
        return [str(exe)]
    return ["cargo", "run", "-q", "-p", "abap_lsp_server", "--"]


def path_uri(base_dir, value):
    path = Path(value)
    if not path.is_absolute():
        path = Path(base_dir) / path
    return normalize_uri(path.resolve().as_uri())


def normalize_uri(uri):
    prefix = "file:///"
    if uri.lower().startswith(prefix):
        rest = uri[len(prefix) :]
        if len(rest) >= 2 and rest[0].isalpha() and rest[1] == ":":
            return prefix + rest[0].lower() + rest[1:]
    return uri


def load_text(base_dir, value):
    path = Path(value)
    if not path.is_absolute():
        path = Path(base_dir) / path
    return path.read_text(encoding="utf-8")


def op_payload(step):
    if "op" in step:
        return str(step["op"]), step
    for name in (
        "initialize",
        "initialized",
        "open",
        "change",
        "notify",
        "request",
        "hover",
        "definition",
        "gotoDefinition",
        "completion",
        "references",
        "semanticTokens",
        "inlayHint",
        "waitAnalysis",
        "sleep",
    ):
        if name in step:
            payload = step[name]
            return name, payload if isinstance(payload, dict) else {"value": payload}
    raise ValueError(f"cannot identify operation in step: {step}")


class Replay:
    def __init__(self, scenario, args):
        self.scenario = scenario
        self.base_dir = Path(scenario.get("baseDir", ".")).resolve()
        cwd = Path(scenario.get("cwd", self.base_dir))
        self.cwd = cwd.resolve() if cwd.is_absolute() else (self.base_dir / cwd).resolve()
        self.timeout = int(scenario.get("timeoutMs", args.timeout_ms))
        self.trace = args.trace or bool(scenario.get("trace", False))
        self.auto_fail_remote = bool(scenario.get("autoFailRemoteDependencies", False))
        self.server_cmd = (
            as_command(args.server)
            if args.server
            else as_command(scenario["server"])
            if "server" in scenario
            else default_server_command(self.cwd)
        )
        self.proc = None
        self.messages = queue.Queue()
        self.next_id = 1
        self.versions = {}
        self.latest_analysis = {}
        self.notification_counts = {}
        self.notifications = []
        self.records = []
        self.saved = {}
        self.stderr = []

    def start(self):
        env = os.environ.copy()
        env.update({str(k): str(v) for k, v in self.scenario.get("env", {}).items()})
        self.proc = subprocess.Popen(
            self.server_cmd,
            cwd=self.cwd,
            env=env,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        threading.Thread(target=self.read_stdout, daemon=True).start()
        threading.Thread(target=self.read_stderr, daemon=True).start()

    def read_stdout(self):
        try:
            while True:
                message = read_frame(self.proc.stdout)
                if message is None:
                    self.messages.put(("eof", None))
                    return
                self.messages.put(("message", message))
        except Exception as error:
            self.messages.put(("error", str(error)))

    def read_stderr(self):
        for raw in self.proc.stderr:
            line = raw.decode("utf-8", errors="replace").rstrip()
            self.stderr.append(line)
            if len(self.stderr) > 200:
                del self.stderr[: len(self.stderr) - 200]

    def send(self, message):
        if self.proc.poll() is not None:
            raise RuntimeError(f"server exited with code {self.proc.returncode}")
        write_frame(self.proc.stdin, message)

    def request(self, method, params=None, timeout_ms=None, allow_error=False):
        request_id = self.next_id
        self.next_id += 1
        message = {"jsonrpc": "2.0", "id": request_id, "method": method}
        if params is not None:
            message["params"] = params
        self.send(message)
        response = self.wait_response(request_id, timeout_ms or self.timeout)
        if response.get("error") and not allow_error:
            raise RuntimeError(f"{method} failed: {response['error']}")
        return response

    def notify(self, method, params=None):
        message = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            message["params"] = params
        self.send(message)

    def next_message(self, timeout_ms):
        try:
            kind, value = self.messages.get(timeout=timeout_ms / 1000)
        except queue.Empty:
            raise TimeoutError("timed out waiting for LSP message")
        if kind == "error":
            raise RuntimeError(value)
        if kind == "eof":
            raise RuntimeError("server closed stdout")
        self.record_message(value)
        return value

    def wait_response(self, request_id, timeout_ms):
        deadline = time.monotonic() + timeout_ms / 1000
        while True:
            remaining_ms = max(1, int((deadline - time.monotonic()) * 1000))
            if remaining_ms <= 1 and time.monotonic() >= deadline:
                raise TimeoutError(f"timed out waiting for response id {request_id}")
            message = self.next_message(remaining_ms)
            if message.get("id") == request_id:
                return message

    def record_message(self, message):
        method = message.get("method")
        if not method:
            return
        self.notification_counts[method] = self.notification_counts.get(method, 0) + 1
        params = message.get("params")
        if method == ANALYSIS_STATUS and isinstance(params, dict):
            self.latest_analysis[params.get("workspaceUri", "")] = params
        if self.trace or method == ANALYSIS_STATUS:
            self.notifications.append(message)
        if method == RESOLVE_REMOTE and self.auto_fail_remote and isinstance(params, dict):
            self.notify(
                REMOTE_UPDATED,
                {
                    "workspaceUri": params.get("workspaceUri", ""),
                    "sourceUri": params.get("sourceUri", ""),
                    "sourceUris": params.get("sourceUris", []),
                    "fetched": [],
                    "failed": params.get("candidates", []),
                    "artifacts": [],
                },
            )

    def document_uri(self, payload):
        if "uri" in payload:
            return normalize_uri(payload["uri"])
        if "path" in payload:
            return path_uri(self.base_dir, payload["path"])
        raise ValueError("step needs uri or path")

    def position_params(self, payload):
        return {
            "textDocument": {"uri": self.document_uri(payload)},
            "position": {
                "line": int(payload["line"]),
                "character": int(payload["character"]),
            },
        }

    def initialize_params(self, payload):
        if "params" in payload:
            return payload["params"]
        workspace = payload.get("workspace", self.scenario.get("workspace"))
        workspace_uri = payload.get("workspaceUri")
        if not workspace_uri and workspace:
            workspace_uri = path_uri(self.base_dir, workspace)
        params = {
            "processId": None,
            "capabilities": {
                "window": {"workDoneProgress": True},
                "textDocument": {
                    "completion": {"completionItem": {"snippetSupport": True}}
                },
            },
            "initializationOptions": payload.get(
                "initializationOptions",
                self.scenario.get("initializationOptions", {}),
            ),
        }
        if workspace_uri:
            params["rootUri"] = workspace_uri
            params["workspaceFolders"] = [
                {
                    "uri": workspace_uri,
                    "name": payload.get("workspaceName", Path(workspace or ".").name),
                }
            ]
        params.update(payload.get("extra", {}))
        return params

    def wait_analysis(self, payload):
        workspace_uri = payload.get("workspaceUri")
        if not workspace_uri:
            workspace = payload.get("workspace", self.scenario.get("workspace"))
            if workspace:
                workspace_uri = path_uri(self.base_dir, workspace)
        require_idle = bool(payload.get("remoteIdle", True))
        accept_complete_progress = bool(payload.get("acceptCompleteProgress", True))
        settle_ms = int(payload.get("settleMs", 100))
        timeout_ms = int(payload.get("timeoutMs", self.timeout))
        deadline = time.monotonic() + timeout_ms / 1000
        while True:
            status = self.latest_analysis.get(workspace_uri or "")
            if self.analysis_ready(status, require_idle):
                return status
            if accept_complete_progress and self.analysis_complete_progress(status, require_idle):
                self.drain_quiet(settle_ms)
                status = self.latest_analysis.get(workspace_uri or "")
                if self.analysis_ready(status, require_idle):
                    return status
                if self.analysis_complete_progress(status, require_idle):
                    assumed = dict(status)
                    assumed["assumedFinished"] = True
                    return assumed
            remaining_ms = int((deadline - time.monotonic()) * 1000)
            if remaining_ms <= 0:
                raise TimeoutError("timed out waiting for workspace analysis to finish")
            self.next_message(max(1, remaining_ms))

    def analysis_ready(self, status, require_idle):
        return bool(
            status
            and status.get("phase") == "finished"
            and (not require_idle or not status.get("remoteResolutionInFlight", False))
        )

    def analysis_complete_progress(self, status, require_idle):
        return bool(
            status
            and status.get("phase") == "progress"
            and status.get("totalDocumentCount", 0) > 0
            and status.get("processedDocumentCount", 0) >= status.get("totalDocumentCount", 0)
            and (not require_idle or not status.get("remoteResolutionInFlight", False))
        )

    def drain_quiet(self, timeout_ms):
        deadline = time.monotonic() + timeout_ms / 1000
        while True:
            remaining_ms = int((deadline - time.monotonic()) * 1000)
            if remaining_ms <= 0:
                return
            try:
                self.next_message(remaining_ms)
            except TimeoutError:
                return

    def run_step(self, index, step):
        op, payload = op_payload(step)
        started = time.monotonic()
        record = {"step": index, "op": op}

        if op == "initialize":
            response = self.request("initialize", self.initialize_params(payload))
            record["response"] = response
        elif op == "initialized":
            self.notify("initialized")
        elif op == "open":
            uri = self.document_uri(payload)
            text = payload.get("text")
            if text is None:
                text = load_text(self.base_dir, payload["path"])
            version = int(payload.get("version", self.versions.get(uri, 0) + 1))
            self.versions[uri] = version
            self.notify(
                "textDocument/didOpen",
                {
                    "textDocument": {
                        "uri": uri,
                        "languageId": payload.get("languageId", "abap"),
                        "version": version,
                        "text": text,
                    }
                },
            )
            record["uri"] = uri
            record["version"] = version
        elif op == "change":
            uri = self.document_uri(payload)
            text = payload.get("text")
            if text is None:
                text = load_text(self.base_dir, payload["path"])
            version = int(payload.get("version", self.versions.get(uri, 0) + 1))
            self.versions[uri] = version
            self.notify(
                "textDocument/didChange",
                {
                    "textDocument": {"uri": uri, "version": version},
                    "contentChanges": [{"text": text}],
                },
            )
            record["uri"] = uri
            record["version"] = version
        elif op == "notify":
            self.notify(payload["method"], payload.get("params"))
        elif op == "request":
            response = self.request(
                payload["method"],
                payload.get("params"),
                payload.get("timeoutMs"),
                bool(payload.get("allowError", False)),
            )
            record["response"] = response
        elif op == "hover":
            record["response"] = self.request("textDocument/hover", self.position_params(payload))
        elif op in ("definition", "gotoDefinition"):
            record["response"] = self.request(
                "textDocument/definition", self.position_params(payload)
            )
        elif op == "completion":
            params = self.position_params(payload)
            if "context" in payload:
                params["context"] = payload["context"]
            record["response"] = self.request("textDocument/completion", params)
        elif op == "references":
            params = self.position_params(payload)
            params["context"] = payload.get("context", {"includeDeclaration": True})
            record["response"] = self.request("textDocument/references", params)
        elif op == "semanticTokens":
            record["response"] = self.request(
                "textDocument/semanticTokens/full",
                {"textDocument": {"uri": self.document_uri(payload)}},
            )
        elif op == "inlayHint":
            record["response"] = self.request(
                "textDocument/inlayHint",
                {
                    "textDocument": {"uri": self.document_uri(payload)},
                    "range": payload["range"],
                },
            )
        elif op == "waitAnalysis":
            record["status"] = self.wait_analysis(payload)
        elif op == "sleep":
            time.sleep(int(payload.get("ms", payload.get("value", 0))) / 1000)
        else:
            raise ValueError(f"unsupported operation: {op}")

        if "response" in record and "expect" in payload:
            self.check_expect(record["response"], payload["expect"])
        if "saveAs" in payload and "response" in record:
            self.saved[payload["saveAs"]] = record["response"].get("result")
        record["elapsedMs"] = int((time.monotonic() - started) * 1000)
        self.records.append(record)

    def check_expect(self, response, expect):
        result = response.get("result")
        if expect.get("resultNotNull") and result is None:
            raise AssertionError("expected non-null result")
        if expect.get("resultNull") and result is not None:
            raise AssertionError("expected null result")
        contains = expect.get("contains")
        if contains:
            text = json.dumps(result, sort_keys=True)
            values = contains if isinstance(contains, list) else [contains]
            for value in values:
                if str(value) not in text:
                    raise AssertionError(f"expected result to contain {value!r}")

    def run(self):
        self.start()
        for index, step in enumerate(self.scenario.get("steps", []), start=1):
            self.run_step(index, step)

    def stop(self):
        if not self.proc:
            return
        if self.proc.poll() is None:
            try:
                self.request("shutdown", {}, 5000)
                self.notify("exit")
            except Exception:
                pass
        try:
            self.proc.wait(timeout=2)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            self.proc.wait(timeout=2)

    def transcript(self, ok=True, error=None):
        out = {
            "ok": ok,
            "server": self.server_cmd,
            "cwd": str(self.cwd),
            "records": self.records,
            "saved": self.saved,
            "notificationCounts": self.notification_counts,
            "analysisStatuses": list(self.latest_analysis.values()),
        }
        if self.trace:
            out["notifications"] = self.notifications
        if error:
            out["error"] = str(error)
            out["serverStderrTail"] = self.stderr[-50:]
        return out


def load_scenario(path):
    if path == "-":
        return json.load(sys.stdin)
    return json.loads(Path(path).read_text(encoding="utf-8"))


def main():
    parser = argparse.ArgumentParser(
        description="Replay a JSON-defined LSP session against abap_lsp_server."
    )
    parser.add_argument("scenario", help="scenario JSON file, or - for stdin")
    parser.add_argument("--timeout-ms", type=int, default=30000)
    parser.add_argument("--trace", action="store_true", help="include all notifications")
    parser.add_argument("--server", nargs=argparse.REMAINDER, help="server command override")
    args = parser.parse_args()

    replay = None
    try:
        scenario = load_scenario(args.scenario)
        replay = Replay(scenario, args)
        replay.run()
        print(json.dumps(replay.transcript(), indent=2, sort_keys=True))
        return 0
    except Exception as error:
        if replay:
            print(json.dumps(replay.transcript(False, error), indent=2, sort_keys=True))
        else:
            print(json.dumps({"ok": False, "error": str(error)}, indent=2, sort_keys=True))
        return 1
    finally:
        if replay:
            replay.stop()


if __name__ == "__main__":
    sys.exit(main())
