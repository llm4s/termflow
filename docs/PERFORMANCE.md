# TermFlow benchmark baselines

JMH microbenchmark baselines for the render + layout hot paths (module `termflow-bench`, see its README). Generated from a single local run on the machine below.

> **These numbers are indicative on the recording machine — not a CI pass/fail threshold.** JMH results vary with hardware, JVM, thermal state, and background load. Use them as a rough baseline for spotting large regressions locally, and regenerate on your own machine for comparison. The bench module is excluded from `ciCheck`; CI only smoke-runs it (`bench.yml`).

## Recording environment

| | |
|---|---|
| Date | 2026-06-15 |
| JDK | Amazon Corretto 21.0.7 (build 21.0.7+6-LTS) |
| CPU | Apple M4 Max (14 cores) |
| OS | macOS 26.5.1 (arm64) |
| JMH | 1.37 |

Command (full sweep, JSON export):

```bash
sbt "bench/Jmh/run -f1 -wi 3 -w 1 -i 5 -r 1 -rf json -rff jmh-result.json"
```

Modest iteration counts (1 fork, 3×1s warmup, 5×1s measurement) keep a full ~44-variant sweep to ~6 minutes; raise `-wi`/`-i`/`-f` for tighter error bars when investigating a specific regression.

## NoOpBench

Harness sanity check — sums ints into a Blackhole; not TermFlow work.

| Method | Mode | Score | Error | Units |
|---|---|---|---|---|
| `sumInts` | avgt | 261.091 | ± 9.779 | ns/op |

## LayoutMeasureBench

`Layout.measure` over the shared fixture trees.

| Method | shape | size | Mode | Score | Error | Units |
|---|---|---|---|---|---|---|
| `measure` | fillRow | large | avgt | 1533.921 | ± 128.975 | ns/op |
| `measure` | fillRow | medium | avgt | 354.332 | ± 9.008 | ns/op |
| `measure` | fillRow | small | avgt | 87.277 | ± 1.496 | ns/op |
| `measure` | flatRow | large | avgt | 1469.828 | ± 41.956 | ns/op |
| `measure` | flatRow | medium | avgt | 331.340 | ± 6.102 | ns/op |
| `measure` | flatRow | small | avgt | 78.525 | ± 2.776 | ns/op |
| `measure` | grid | large | avgt | 3213.448 | ± 46.789 | ns/op |
| `measure` | grid | medium | avgt | 767.555 | ± 7.275 | ns/op |
| `measure` | grid | small | avgt | 216.752 | ± 3.733 | ns/op |
| `measure` | nestedColumn | large | avgt | 1586.366 | ± 47.705 | ns/op |
| `measure` | nestedColumn | medium | avgt | 377.956 | ± 49.906 | ns/op |
| `measure` | nestedColumn | small | avgt | 91.075 | ± 1.641 | ns/op |

## LayoutResolveBench

`Layout.resolve` (natural) and `Layout.resolveTo` (budgeted) over the fixtures; `fillRow` is the shape that drives the Fill even-split path under a budget.

| Method | shape | size | Mode | Score | Error | Units |
|---|---|---|---|---|---|---|
| `resolveBudgeted` | fillRow | large | avgt | 5.076 | ± 0.3362 | us/op |
| `resolveBudgeted` | fillRow | medium | avgt | 0.8027 | ± 0.0466 | us/op |
| `resolveBudgeted` | fillRow | small | avgt | 0.2257 | ± 0.0264 | us/op |
| `resolveBudgeted` | flatRow | large | avgt | 3.883 | ± 0.0867 | us/op |
| `resolveBudgeted` | flatRow | medium | avgt | 0.4851 | ± 0.0107 | us/op |
| `resolveBudgeted` | flatRow | small | avgt | 0.1226 | ± 0.0061 | us/op |
| `resolveBudgeted` | grid | large | avgt | 4.994 | ± 0.2416 | us/op |
| `resolveBudgeted` | grid | medium | avgt | 1.460 | ± 0.0332 | us/op |
| `resolveBudgeted` | grid | small | avgt | 0.3384 | ± 0.0136 | us/op |
| `resolveBudgeted` | nestedColumn | large | avgt | 3.339 | ± 0.1884 | us/op |
| `resolveBudgeted` | nestedColumn | medium | avgt | 0.9636 | ± 0.0094 | us/op |
| `resolveBudgeted` | nestedColumn | small | avgt | 0.3368 | ± 0.0192 | us/op |
| `resolveNatural` | fillRow | large | avgt | 4.679 | ± 0.0894 | us/op |
| `resolveNatural` | fillRow | medium | avgt | 0.6532 | ± 0.0586 | us/op |
| `resolveNatural` | fillRow | small | avgt | 0.1659 | ± 0.0089 | us/op |
| `resolveNatural` | flatRow | large | avgt | 4.642 | ± 0.4090 | us/op |
| `resolveNatural` | flatRow | medium | avgt | 0.6632 | ± 0.0160 | us/op |
| `resolveNatural` | flatRow | small | avgt | 0.1600 | ± 0.0083 | us/op |
| `resolveNatural` | grid | large | avgt | 6.156 | ± 0.1595 | us/op |
| `resolveNatural` | grid | medium | avgt | 1.552 | ± 0.0472 | us/op |
| `resolveNatural` | grid | small | avgt | 0.4230 | ± 0.0791 | us/op |
| `resolveNatural` | nestedColumn | large | avgt | 4.648 | ± 0.1263 | us/op |
| `resolveNatural` | nestedColumn | medium | avgt | 1.272 | ± 0.0347 | us/op |
| `resolveNatural` | nestedColumn | small | avgt | 0.3976 | ± 0.0453 | us/op |

## BuildFrameBench

`AnsiRenderer.buildFrame` on a representative `RootNode`.

| Method | Mode | Score | Error | Units |
|---|---|---|---|---|
| `buildFrame` | avgt | 39.012 | ± 1.269 | us/op |

## AnsiDiffBench

`AnsiRenderer.diff(Some(prev), cur)` across controlled before/after frame pairs.

| Method | scenario | Mode | Score | Error | Units |
|---|---|---|---|---|---|
| `diff` | fullScreen | avgt | 88.689 | ± 1.388 | us/op |
| `diff` | identical | avgt | 25.977 | ± 0.6807 | us/op |
| `diff` | singleCell | avgt | 29.550 | ± 1.001 | us/op |
| `diff` | singleRow | avgt | 28.862 | ± 8.039 | us/op |

## SineWaveFrameBench

End-to-end per animation tick: advance phase → `view` → `buildFrame` → `diff`, via `TuiTestDriver`.

| Method | size | Mode | Score | Error | Units |
|---|---|---|---|---|---|
| `frame` | 200x50 | avgt | 200.413 | ± 5.011 | us/op |
| `frame` | 80x24 | avgt | 38.426 | ± 0.4645 | us/op |

