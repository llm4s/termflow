#!/usr/bin/env bash
#
# Verify that every widget under termflow-widgets is mentioned in the
# user-facing widgets guide. Prevents the catalogue page from rotting
# silently when a new widget is added.
#
# Run from the repo root; exits non-zero with a clear diff if any
# widget file isn't named in docs/guide/widgets.md.

set -euo pipefail

WIDGET_DIR="modules/termflow-widgets/src/main/scala/termflow/tui/widgets"
GUIDE="docs/guide/widgets.md"

if [[ ! -d "$WIDGET_DIR" ]]; then
  echo "$0: widget directory not found at $WIDGET_DIR" >&2
  exit 1
fi

if [[ ! -f "$GUIDE" ]]; then
  echo "$0: widgets guide not found at $GUIDE" >&2
  exit 1
fi

missing=()
for src in "$WIDGET_DIR"/*.scala; do
  name="$(basename "$src" .scala)"
  if ! grep -qE "(^|[^A-Za-z])${name}([^A-Za-z]|$)" "$GUIDE"; then
    missing+=("$name")
  fi
done

if (( ${#missing[@]} > 0 )); then
  echo "ERROR: the following widgets are missing from $GUIDE:" >&2
  printf '  - %s\n' "${missing[@]}" >&2
  echo "" >&2
  echo "Add a section for each new widget, then re-run this script." >&2
  exit 1
fi

echo "OK: all $(ls -1 "$WIDGET_DIR"/*.scala | wc -l | tr -d ' ') widgets covered in $GUIDE"
