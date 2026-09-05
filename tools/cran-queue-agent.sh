#!/bin/sh
# Install or remove the launchd agent that watches concurve's CRAN queue
# status once a day.
#
#     sh tools/cran-queue-agent.sh install
#     sh tools/cran-queue-agent.sh uninstall
#     sh tools/cran-queue-agent.sh status
#
# The plist is written to ~/Library/LaunchAgents/, outside the repository,
# so it is never committed or shipped. Nothing about the package build
# depends on it, and uninstall removes it completely.
#
# It runs tools/cran-queue-notify.sh, which notifies only when the status
# changes -- see that file.

set -u

LABEL=com.zad.concurve-cran-queue
PLIST="$HOME/Library/LaunchAgents/$LABEL.plist"
STATE_DIR="$HOME/.local/state/concurve-cran"
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." 2>/dev/null && pwd) || ROOT=.
RUNNER="$ROOT/tools/cran-queue-notify.sh"

# Runs daily at this local time.
HOUR=10
MINUTE=0

usage() {
  echo "usage: sh tools/cran-queue-agent.sh [install|uninstall|status]" >&2
  exit 64
}

[ $# -eq 1 ] || usage

case "$1" in
  install)
    mkdir -p "$HOME/Library/LaunchAgents" "$STATE_DIR"
    cat >"$PLIST" <<PLIST_END
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>$LABEL</string>
    <key>ProgramArguments</key>
    <array>
        <string>/bin/sh</string>
        <string>$RUNNER</string>
    </array>
    <key>StartCalendarInterval</key>
    <dict>
        <key>Hour</key><integer>$HOUR</integer>
        <key>Minute</key><integer>$MINUTE</integer>
    </dict>
    <key>RunAtLoad</key>
    <false/>
    <key>StandardOutPath</key>
    <string>$STATE_DIR/launchd.out</string>
    <key>StandardErrorPath</key>
    <string>$STATE_DIR/launchd.err</string>
</dict>
</plist>
PLIST_END
    launchctl unload "$PLIST" 2>/dev/null
    launchctl load "$PLIST" || { echo "load failed" >&2; exit 1; }
    printf 'installed %s\n' "$LABEL"
    printf '  runs:   %s daily at %02d:%02d local\n' "$RUNNER" "$HOUR" "$MINUTE"
    printf '  plist:  %s\n' "$PLIST"
    printf '  state:  %s\n' "$STATE_DIR"
    echo "  notifies only when the status changes; remove with: uninstall"
    ;;
  uninstall)
    launchctl unload "$PLIST" 2>/dev/null
    rm -f "$PLIST"
    printf 'removed %s\n' "$LABEL"
    printf '  plist deleted. State kept at %s -- delete by hand if unwanted.\n' "$STATE_DIR"
    ;;
  status)
    if [ -f "$PLIST" ]; then
      echo "plist present: $PLIST"
      launchctl list 2>/dev/null | grep "$LABEL" \
        && echo "  loaded" || echo "  NOT loaded"
      [ -f "$STATE_DIR/last-status" ] \
        && printf '  last seen: %s\n' "$(cat "$STATE_DIR/last-status")"
    else
      echo "not installed"
    fi
    ;;
  *) usage ;;
esac
