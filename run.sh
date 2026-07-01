#!/usr/bin/env bash
set -eo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODE="debug"
APP="abap_frontend"
BUILD_ARGS=()

while [[ $# -gt 0 ]]; do
	arg_lc="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')"
	case "$arg_lc" in
		debug)
			MODE="debug"
			;;
		release)
			MODE="release"
			;;
		abap_frontend)
			APP="abap_frontend"
			;;
		adt_cli | adt)
			APP="adt_cli"
			;;
		abap_language_server | lsp)
			APP="abap_language_server"
			;;
		trace)
			BUILD_ARGS+=(trace)
			;;
		*)
			break
			;;
	esac
	shift
done

"$ROOT/build.sh" "$MODE" "${BUILD_ARGS[@]}"
"$ROOT/bin/$MODE/$APP" "$@"
