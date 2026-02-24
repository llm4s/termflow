#!/usr/bin/env bash
set -euo pipefail

echo "Running Scala 3 style syntax gate..."

# Block common Scala 2 brace-style syntax forms so we do not drift back.
# This intentionally targets high-signal patterns used in this repo.
patterns=(
  '^[[:space:]]*(object|class|trait|enum)[[:space:]].*\{[[:space:]]*$'
  '^[[:space:]]*match[[:space:]]*\{[[:space:]]*$'
  '^[[:space:]]*(if|while|for)[[:space:]]*\(.*\)[[:space:]]*\{[[:space:]]*$'
  '^[[:space:]]*try[[:space:]]*\{[[:space:]]*$'
)

status=0

for pattern in "${patterns[@]}"; do
  if rg -n --glob 'modules/**/src/**/*.scala' --pcre2 "$pattern" modules >/tmp/scala3-style-gate.out; then
    echo "Found Scala 2 brace-style syntax matching pattern: $pattern"
    cat /tmp/scala3-style-gate.out
    status=1
  fi
done

if [[ "$status" -ne 0 ]]; then
  echo
  echo "Scala 3 style gate failed. Use significant indentation / optional braces style."
  exit 1
fi

echo "Scala 3 style syntax gate passed."
