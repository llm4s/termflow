package termflow.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import termflow.apps.stress.SineWaveApp
import termflow.testkit.TuiTestDriver
import termflow.tui.AnsiRenderer
import termflow.tui.AnsiRenderer.{ DiffResult, RenderFrame }

/**
 * End-to-end per-frame benchmark for [[termflow.apps.stress.SineWaveApp]]:
 * each `@Benchmark` invocation simulates one animation tick the way the
 * runtime would — advance the wave phase, rebuild the `RootNode` via
 * `App.view`, `buildFrame` it, then `diff` against the previous frame.
 *
 * The terminal size is a `@Param` axis so the sweep covers small and large
 * grids. The starting [[SineWaveApp.Model]] carries `Sub` fields and so cannot
 * be hand-constructed; it is obtained from a real `App.init` via
 * [[TuiTestDriver]] in [[setup]].
 *
 * Thread safety is self-enforcing: the mutable per-invocation state (current
 * model + previous frame) lives in a `@State(Scope.Thread)` object and the
 * class is `@Threads(1)`, so each worker thread owns its own copy regardless
 * of any CLI flag.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Threads(1)
@State(Scope.Thread)
class SineWaveFrameBench:

  @Param(Array("80x24", "200x50"))
  var size: String = ""

  private var model: SineWaveApp.Model = null
  private var prev: RenderFrame        = null

  @Setup
  def setup(): Unit =
    val Array(w, h) = size.split("x").map(_.toInt): @unchecked
    val driver      = TuiTestDriver(SineWaveApp.App, w, h)
    driver.init()
    model = driver.model
    prev = AnsiRenderer.buildFrame(SineWaveApp.App.view(model))

  @Benchmark
  def frame: DiffResult =
    val next = model.copy(phase = model.phase + model.step)
    val root = SineWaveApp.App.view(next)
    val cur  = AnsiRenderer.buildFrame(root)
    val result = AnsiRenderer.diff(Some(prev), cur)
    model = next
    prev = cur
    result
