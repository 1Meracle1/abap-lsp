#!/bin/zsh
#
# Clears Zed's cached ABAP tree-sitter grammar when the cached clone is stale or dirty.
#
# Zed installs dev extensions as local builds and keeps grammar build output under
# grammars/<grammar>. If that cached clone has local changes or points at an old
# remote, Zed cannot checkout the rev from extension.toml and keeps using the old
# WASM parser. This script removes only the grammar cache directory and WASM file
# so Zed can rebuild them.

set -e
set -u
set -o pipefail

extension_id="abap"
grammar="abap"
zed_root="${HOME}/Library/Application Support/Zed"
restart_zed=0
restart_wait_seconds=15
rebuild_wait_seconds=60
force=0
dry_run=0

usage() {
  cat <<'EOF'
Usage: ./reset-grammar-cache.sh [options]

Clears a stale or dirty ABAP grammar cache and asks you to reload/rebuild the
dev extension.

Options:
  --extension-id ID          Zed extension id. Default: abap
  --grammar NAME             Grammar name. Default: abap
  --zed-root PATH            Zed data root. Default: ~/Library/Application Support/Zed
  --restart-zed              Quit Zed, clear the cache, restart Zed, and wait for the parser
  --restart-wait-seconds N   Seconds to wait for Zed to quit. Default: 15
  --rebuild-wait-seconds N   Seconds to wait for NAME.wasm after restart. Default: 60
  --force                    Reset even if the cache appears clean
  -n, --dry-run              Print what would be removed or restarted
  -h, --help                 Show this help

Examples:
  ./reset-grammar-cache.sh
  ./reset-grammar-cache.sh --restart-zed
  ./reset-grammar-cache.sh --force --dry-run
EOF
}

die() {
  print -u2 -- "error: $*"
  exit 1
}

warn() {
  print -u2 -- "warning: $*"
}

full_path() {
  print -r -- "${1:A}"
}

assert_child_path() {
  local path_full parent_full path_lower parent_lower

  path_full="$(full_path "$1")"
  parent_full="$(full_path "$2")"
  path_lower="${path_full:l}"
  parent_lower="${parent_full:l}"

  if [[ "$path_lower" != "$parent_lower" && "$path_lower" != "$parent_lower/"* ]]; then
    die "$3 path '$path_full' is outside expected root '$parent_full'."
  fi
}

grammar_rev_from_manifest() {
  local manifest_path="$1"
  local grammar_name="$2"

  awk -v target="[grammars.${grammar_name}]" '
    function trim(value) {
      sub(/^[ \t\r\n]+/, "", value)
      sub(/[ \t\r\n]+$/, "", value)
      return value
    }

    {
      line = trim($0)
      if (line ~ /^\[.*\]$/) {
        in_section = (line == target)
        next
      }

      if (in_section && line ~ /^rev[ \t]*=/) {
        sub(/^rev[ \t]*=[ \t]*"/, "", line)
        sub(/"[ \t]*$/, "", line)
        print line
        found = 1
        exit
      }
    }

    END {
      if (!found) {
        exit 1
      }
    }
  ' "$manifest_path"
}

git_output() {
  local repository="$1"
  shift

  if ! command -v git >/dev/null 2>&1; then
    return 1
  fi

  local output
  if output="$(git -C "$repository" "$@" 2>/dev/null)"; then
    print -r -- "$output"
    return 0
  fi

  return 1
}

zed_pids() {
  pgrep -x Zed 2>/dev/null || true
}

join_lines() {
  paste -sd ", " - 2>/dev/null || cat
}

remove_cache_target() {
  local target="$1"
  local cache_root_full="$2"

  if [[ ! -e "$target" ]]; then
    return
  fi

  assert_child_path "$target" "$cache_root_full" "Cache target"

  if (( dry_run )); then
    print -- "Would remove $target"
    return
  fi

  rm -rf -- "$target"
  print -- "Removed $target"
}

while (( $# > 0 )); do
  case "$1" in
    --extension-id)
      (( $# >= 2 )) || die "--extension-id requires a value"
      extension_id="$2"
      shift 2
      ;;
    --grammar)
      (( $# >= 2 )) || die "--grammar requires a value"
      grammar="$2"
      shift 2
      ;;
    --zed-root)
      (( $# >= 2 )) || die "--zed-root requires a value"
      zed_root="$2"
      shift 2
      ;;
    --restart-zed)
      restart_zed=1
      shift
      ;;
    --restart-wait-seconds)
      (( $# >= 2 )) || die "--restart-wait-seconds requires a value"
      restart_wait_seconds="$2"
      shift 2
      ;;
    --rebuild-wait-seconds)
      (( $# >= 2 )) || die "--rebuild-wait-seconds requires a value"
      rebuild_wait_seconds="$2"
      shift 2
      ;;
    --force)
      force=1
      shift
      ;;
    -n|--dry-run)
      dry_run=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      die "unknown option '$1'"
      ;;
  esac
done

[[ "$restart_wait_seconds" == <-> ]] || die "--restart-wait-seconds must be a non-negative integer"
[[ "$rebuild_wait_seconds" == <-> ]] || die "--rebuild-wait-seconds must be a non-negative integer"

script_dir="${0:A:h}"
manifest_path="${script_dir}/extension.toml"
[[ -f "$manifest_path" ]] || die "expected Zed extension manifest at '$manifest_path'"

manifest_rev="$(grammar_rev_from_manifest "$manifest_path" "$grammar")" \
  || die "could not find rev in [grammars.${grammar}] in '$manifest_path'"

installed_extension="${zed_root}/extensions/installed/${extension_id}"
cache_root="${installed_extension}/grammars"

if [[ ! -d "$installed_extension" ]]; then
  warn "Zed dev extension '${extension_id}' is not installed at '${installed_extension}'. Falling back to extension-local cache."
  cache_root="${script_dir}/grammars"
fi

cache_root_full="$(full_path "$cache_root")"
repo_cache="${cache_root_full}/${grammar}"
wasm_cache="${cache_root_full}/${grammar}.wasm"

assert_child_path "$repo_cache" "$cache_root_full" "Grammar repository cache"
assert_child_path "$wasm_cache" "$cache_root_full" "Grammar WASM cache"

cached_rev=""
dirty=0
if [[ -d "${repo_cache}/.git" ]]; then
  cached_rev="$(git_output "$repo_cache" rev-parse HEAD || true)"
  git_status="$(git_output "$repo_cache" status --porcelain || true)"
  if [[ -n "$git_status" ]]; then
    dirty=1
  fi
fi

wasm_exists=0
if [[ -e "$wasm_cache" ]]; then
  wasm_exists=1
fi

stale=0
if [[ -n "$cached_rev" && "$cached_rev" != "$manifest_rev" ]]; then
  stale=1
fi

missing_repo=0
if [[ ! -e "$repo_cache" ]]; then
  missing_repo=1
fi

should_reset=0
if (( force || stale || dirty || missing_repo || ! wasm_exists )); then
  should_reset=1
fi

print -- "Manifest rev: $manifest_rev"
if [[ -n "$cached_rev" ]]; then
  print -- "Cached rev:   $cached_rev"
else
  print -- "Cached rev:   <none>"
fi
print -- "Cache root:   $cache_root_full"

if (( ! should_reset )); then
  print -- "Zed grammar cache already matches the manifest and is clean."
  exit 0
fi

pids="$(zed_pids)"
if (( restart_zed )) && [[ -n "$pids" ]]; then
  process_ids="$(print -r -- "$pids" | join_lines)"
  print -- "Closing Zed before resetting grammar cache (PID $process_ids)."

  if (( dry_run )); then
    print -- "Would quit Zed."
  else
    osascript -e 'tell application "Zed" to quit' >/dev/null 2>&1 || true

    deadline=$(( SECONDS + restart_wait_seconds ))
    while (( SECONDS < deadline )); do
      if [[ -z "$(zed_pids)" ]]; then
        break
      fi
      sleep 0.25
    done

    remaining="$(zed_pids)"
    if [[ -n "$remaining" ]]; then
      remaining_ids="$(print -r -- "$remaining" | join_lines)"
      die "Zed did not close within ${restart_wait_seconds} seconds (PID ${remaining_ids}). Close it manually, then rerun this script."
    fi
  fi
elif [[ -n "$pids" ]]; then
  process_ids="$(print -r -- "$pids" | join_lines)"
  warn "Zed is currently running (PID $process_ids). Close/reopen Zed or run the in-app dev-extension rebuild after this reset."
fi

if (( stale )); then
  print -- "Reason: cached grammar revision is stale."
fi
if (( dirty )); then
  print -- "Reason: cached grammar repository has local changes."
fi
if (( missing_repo )); then
  print -- "Reason: cached grammar repository is missing."
fi
if (( ! wasm_exists )); then
  print -- "Reason: cached WASM parser is missing."
fi
if (( force )); then
  print -- "Reason: --force was supplied."
fi

remove_cache_target "$repo_cache" "$cache_root_full"
remove_cache_target "$wasm_cache" "$cache_root_full"

if (( restart_zed )); then
  if (( dry_run )); then
    print -- "Would start Zed and wait up to ${rebuild_wait_seconds} seconds for ${grammar}.wasm."
    exit 0
  fi

  if command -v zed >/dev/null 2>&1; then
    zed >/dev/null 2>&1 &
  else
    open -a Zed
  fi

  print -- "Started Zed. Waiting up to ${rebuild_wait_seconds} seconds for ${grammar}.wasm."
  deadline=$(( SECONDS + rebuild_wait_seconds ))
  while (( SECONDS < deadline )); do
    if [[ -e "$wasm_cache" ]]; then
      print -- "Detected rebuilt parser at $wasm_cache."
      exit 0
    fi
    sleep 1
  done

  warn "Zed was restarted, but ${grammar}.wasm was not recreated within ${rebuild_wait_seconds} seconds. Check Zed logs or run the in-app dev-extension rebuild."
  exit 1
fi

print -- "Reload or rebuild the ABAP dev extension in Zed. Zed should now checkout $manifest_rev and rebuild ${grammar}.wasm."
