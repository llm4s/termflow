#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

LOG_PATH="target/termflow-sample/logging-metrics-demo.log"

rm -f "$LOG_PATH"

echo "Running logging/metrics demo outside sbt..."
echo "The app will render a short progress view, exit automatically, and write metrics to:"
echo "  $LOG_PATH"
echo

./scripts/run_termflow_sample.sh metrics-demo

echo
echo "Demo completed."
echo "Log file:"
echo "  $LOG_PATH"
echo
echo "Metrics log contents:"
cat "$LOG_PATH"
