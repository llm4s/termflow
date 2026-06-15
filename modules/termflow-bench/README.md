# termflow-bench

JMH microbenchmarks for TermFlow (Scala 3 only).

## Not part of `ciCheck`

This module is **deliberately excluded** from `root.aggregate(...)` in
`build.sbt`. Because sbt's `scalafmtCheckAll`, `scalafixAll --check`, and `test`
all follow project aggregation, that means `ciCheck`
(`;scalafmtCheckAll;scalafixAll --check;test`) does **not** compile, test,
format-check, or scalafix-lint anything here.

The module is built only by:

- explicit `sbt "bench/..."` invocations, and
- the dedicated bench CI workflow.

Keep the code consistent with the repo's `.scalafmt.conf` anyway — run
`sbt "bench/scalafmt"` locally before committing.

## Running

```bash
# Quick smoke run (1 iteration, 1 warmup, 1 fork)
sbt "bench/Jmh/run -i 1 -wi 1 -f1"

# A specific benchmark
sbt "bench/Jmh/run -i 1 -wi 1 -f1 NoOpBench"
```

JMH benchmarks are **main** sources (`src/main/scala/termflow/bench/`), not test
sources.
