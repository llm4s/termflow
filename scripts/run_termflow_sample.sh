#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

APP_ARG="${1:-hub}"

CP="$(
  sbt -no-colors -batch "show termflowSample / Runtime / fullClasspath" |
    sed -n 's/^\[info\] \* Attributed(//p' |
    sed 's/)$//' |
    paste -sd ':' -
)"

if [[ -z "$CP" ]]; then
  echo "Failed to resolve runtime classpath" >&2
  exit 1
fi

exec java -cp "$CP" termflow.run.TermFlowMain "$APP_ARG"
