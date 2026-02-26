#!/usr/bin/env bash
set -euo pipefail

if command -v timeout >/dev/null 2>&1; then
  timeout 180s sbt --batch "termflowSample/runMain termflow.run.SampleSmoke" || {
    code=$?
    if [ "$code" -eq 124 ]; then
      echo "Sample smoke timed out after 180s" >&2
    fi
    exit "$code"
  }
else
  sbt --batch "termflowSample/runMain termflow.run.SampleSmoke"
fi
