# Shared helpers for the release scripts. Source it, do not run it.
# Environment:
#   GH_REPO     owner/name of the public repository (required)
#   GH_TOKEN    credential for the GitHub API (required)
#   GH_API      API base URL (default https://api.github.com). Tests point it
#               at a fake GitHub.
# Needs curl and jq.

GH_API="${GH_API:-https://api.github.com}"

die() { printf '%s: %s\n' "$(basename "$0")" "$*" >&2; exit 1; }

need_env() {
  for v in "$@"; do
    eval "val=\${$v:-}"
    [ -n "$val" ] || die "$v is not set"
  done
}

# gh_api METHOD PATH [JSON-BODY]
# Writes the response body to $GH_BODY and the HTTP status to $GH_STATUS.
gh_api() {
  GH_BODY="$(mktemp)"
  _path="$2"; _json="${3:-}"
  set -- -sS -o "$GH_BODY" -w '%{http_code}' -X "$1" \
    -H "Authorization: Bearer $GH_TOKEN" -H 'Accept: application/vnd.github+json'
  [ -z "$_json" ] || set -- "$@" -H 'Content-Type: application/json' -d "$_json"
  GH_STATUS="$(curl "$@" "$GH_API$_path")" || die "no answer from $GH_API"
}

# gh_ok METHOD PATH [BODY]: like gh_api, but stops on any status but 2xx.
gh_ok() {
  gh_api "$@"
  case "$GH_STATUS" in
    2??) ;;
    *) die "$1 $2 answered $GH_STATUS: $(head -c 300 "$GH_BODY")" ;;
  esac
}

# gh_list PATH: every item of a paged list, as one JSON array on stdout.
gh_list() {
  _page=1; _all='[]'
  while :; do
    case "$1" in *\?*) _sep='&' ;; *) _sep='?' ;; esac
    gh_ok GET "$1${_sep}per_page=100&page=$_page"
    _n="$(jq 'length' "$GH_BODY")"
    _all="$(printf '%s' "$_all" | jq -c --slurpfile p "$GH_BODY" '. + $p[0]')"
    [ "$_n" -lt 100 ] && break
    _page=$((_page + 1))
  done
  printf '%s' "$_all"
}

# gh_delete_each LIST-JSON JQ-FILTER DELETE-PATH-PREFIX LABEL TAG
# Deletes "$DELETE-PATH-PREFIX<value>" for each value that JQ-FILTER prints for
# LIST-JSON (a JSON array from gh_list). The filter sees TAG as $t. A non-empty
# LABEL prints "LABEL <value>" for each deletion.
gh_delete_each() {
  for _v in $(printf '%s' "$1" | jq -r --arg t "$5" "$2"); do
    gh_ok DELETE "$3$_v"
    [ -z "$4" ] || echo "$4 $_v"
  done
}

# version_gt A B: success when Release Version A is larger than B. Each part
# compares as a number, so 26.11.10000 is larger than 26.11.9999.
version_gt() {
  printf '%s\n%s\n' "$1" "$2" | awk -F. '
    NR == 1 { for (i = 1; i <= 3; i++) a[i] = $i + 0 }
    NR == 2 { for (i = 1; i <= 3; i++) b[i] = $i + 0 }
    END { for (i = 1; i <= 3; i++) { if (a[i] > b[i]) exit 0; if (a[i] < b[i]) exit 1 } exit 1 }'
}
