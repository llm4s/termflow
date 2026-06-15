package termflow.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import termflow.tui.AnsiRenderer
import termflow.tui.AnsiRenderer.{ DiffResult, RenderFrame }

/**
 * Microbenchmark for [[termflow.tui.AnsiRenderer.diff]] across a `@Param`
 * scenario axis. For each scenario [[setup]] pre-builds the `before`/`after`
 * [[RenderFrame]]s via `buildFrame` from the shared [[Fixtures.diffRoots]]
 * pairs, so the timed `@Benchmark` measures only the diff itself.
 *
 * The scenarios cover the distinct diff cost profiles:
 *   - `identical`  — no changed cells (the cheap no-op path).
 *   - `singleCell` — one cell differs (one repainted row).
 *   - `singleRow`  — one full row differs.
 *   - `fullScreen` — every row differs (worst case).
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class AnsiDiffBench:

  @Param(Array("identical", "singleCell", "singleRow", "fullScreen"))
  var scenario: String = ""

  private var prev: RenderFrame = null
  private var cur: RenderFrame  = null

  @Setup
  def setup(): Unit =
    val (before, after) = Fixtures.diffRoots(scenario)
    prev = AnsiRenderer.buildFrame(before)
    cur = AnsiRenderer.buildFrame(after)

  @Benchmark
  def diff: DiffResult =
    AnsiRenderer.diff(Some(prev), cur)
