#!/usr/bin/env bash
set -eo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ODIN_EXE="${ODIN_EXE:-odin}"
ODIN_FLAGS=(-collection:src="$ROOT/src" -vet -warnings-as-errors)
MODE=""
ODIN_EXTRA_ARGS=()
ODIN_FRONTEND_EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
	arg_lc="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')"
	case "$arg_lc" in
		debug)
			MODE="debug"
			;;
		release)
			MODE="release"
			;;
		trace)
			ODIN_FRONTEND_EXTRA_ARGS+=(-define:ABAP_FRONTEND_TRACE=true)
			;;
		*)
			ODIN_EXTRA_ARGS+=("$1")
			;;
	esac
	shift
done

if [[ -z "$MODE" ]]; then
	MODE="debug"
fi

OUT_DIR="$ROOT/bin/$MODE"
mkdir -p "$OUT_DIR"

if [[ "$MODE" == "release" ]]; then
	echo "[release mode]"
	MODE_FLAGS=(-o:speed)
else
	echo "[debug mode]"
	MODE_FLAGS=(-debug)
fi

"$ODIN_EXE" build "$ROOT/cmd/abap_frontend" \
	-out:"$OUT_DIR/abap_frontend" \
	"${ODIN_FLAGS[@]}" \
	"${MODE_FLAGS[@]}" \
	"${ODIN_EXTRA_ARGS[@]}" \
	"${ODIN_FRONTEND_EXTRA_ARGS[@]}"

"$ODIN_EXE" build "$ROOT/cmd/adt_cli" \
	-out:"$OUT_DIR/adt_cli" \
	"${ODIN_FLAGS[@]}" \
	"${MODE_FLAGS[@]}" \
	"${ODIN_EXTRA_ARGS[@]}"

"$ODIN_EXE" build "$ROOT/cmd/abap_language_server" \
	-out:"$OUT_DIR/abap_language_server" \
	"${ODIN_FLAGS[@]}" \
	"${MODE_FLAGS[@]}" \
	"${ODIN_EXTRA_ARGS[@]}"
