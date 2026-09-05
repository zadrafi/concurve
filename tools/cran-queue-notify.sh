#!/bin/sh
# Wrapper around cran-queue.sh for unattended use by launchd.
#
# Notifies only when something CHANGES. A daily "still queued" alert would
# train you to dismiss it unread, which defeats the point -- the whole
# reason to watch is to catch the one day it moves.
#
# Install/remove with:
#     sh tools/cran-queue-agent.sh install
#     sh tools/cran-queue-agent.sh uninstall
#
# State and logs live under ~/.local/state/concurve-cran/, outside the
# repository, so nothing here is committed or shipped.

set -u

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." 2>/dev/null && pwd) || ROOT=.
STATE_DIR="$HOME/.local/state/concurve-cran"
LAST="$STATE_DIR/last-status"
LOG="$STATE_DIR/queue.log"

mkdir -p "$STATE_DIR"

out=$(sh "$ROOT/tools/cran-queue.sh" 2>&1)
rc=$?

printf '===== %s (exit %s) =====\n%s\n\n' \
  "$(date -u '+%Y-%m-%d %H:%M UTC')" "$rc" "$out" >>"$LOG"

notify() {
  # $1 title, $2 message
  osascript -e "display notification \"$2\" with title \"$1\"" >/dev/null 2>&1
}

# Exit 2 means at least one request failed, so the listing is untrustworthy.
# Report it, but do not overwrite the last known good status with it.
if [ "$rc" -eq 2 ]; then
  notify "concurve: CRAN check failed" \
         "Could not reach CRAN. Status unknown -- see the log."
  exit 2
fi

# The status line is the one-line summary the script prints after the rule.
status=$(printf '%s\n' "$out" | grep -E "^(concurve [0-9]|Nothing of concurve)" | head -1)
[ -z "$status" ] && status="(could not parse status)"

prev=""
[ -f "$LAST" ] && prev=$(cat "$LAST")

if [ "$status" != "$prev" ]; then
  printf '%s\n' "$status" >"$LAST"
  if [ -n "$prev" ]; then
    notify "concurve: CRAN status CHANGED" "$status"
  else
    notify "concurve: CRAN watch started" "$status"
  fi
fi

exit 0
