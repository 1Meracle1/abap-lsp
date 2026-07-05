#!/usr/bin/env bash
set -eo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ODIN_EXE="${ODIN_EXE:-odin}"
ODIN_FLAGS=(-collection:src="$ROOT/src" -vet -warnings-as-errors)
TEST_FLAGS=()

if [[ $# -eq 0 ]]; then
	TEST_FLAGS=(-define:ODIN_TEST_LOG_LEVEL=error)
fi

while [[ $# -gt 0 ]]; do
	arg_lc="$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')"
	case "$arg_lc" in
		--no-leak-warnings)
			TEST_FLAGS+=(-define:ODIN_TEST_LOG_LEVEL=error)
			;;
		*)
			TEST_FLAGS+=("$1")
			;;
	esac
	shift
done

mkdir -p "$ROOT/bin"

CHECK_PACKAGES=(
	tokenizer
	string_interner
	encoding/toml
	ast
	parser
	ir
	ir/bytecode
	runtime
	vm
	execution
	trace
	http
	adt
	ddic_xml
	semantic
	remote_dependencies
	lsp
	workspace
	lints
	persistence/sqlite3
	dependency_store
)

TEST_PACKAGES=(
	tokenizer
	string_interner
	encoding/toml
	ast
	parser
	ir
	ir/bytecode
	runtime
	vm
	execution
	http
	adt
	ddic_xml
	semantic
	remote_dependencies
	lsp
	lints
	persistence/sqlite3
	dependency_store
	workspace
)

for package in "${CHECK_PACKAGES[@]}"; do
	"$ODIN_EXE" check "$ROOT/src/$package" -no-entry-point "${ODIN_FLAGS[@]}"
done

for package in "${TEST_PACKAGES[@]}"; do
	"$ODIN_EXE" test "$ROOT/src/$package" "${ODIN_FLAGS[@]}" "${TEST_FLAGS[@]}"
done
