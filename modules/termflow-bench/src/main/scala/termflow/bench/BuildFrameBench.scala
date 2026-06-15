package termflow.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import termflow.tui.AnsiRenderer
import termflow.tui.AnsiRenderer.RenderFrame
import termflow.tui.RootNode

/**
 * Microbenchmark for [[termflow.tui.AnsiRenderer.buildFrame]] over the shared
 * [[Fixtures.screenRoot]] — a full-screen bordered root with dense interior
 * text, so the cell grid is populated the way a real frame would be.
 *
 * The root is built once in [[setup]] and the `@Benchmark` returns the
 * resulting [[RenderFrame]] so dead-code elimination can't drop the work.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class BuildFrameBench:

  private var root: RootNode = null

  @Setup
  def setup(): Unit =
    root = Fixtures.screenRoot

  @Benchmark
  def buildFrame: RenderFrame =
    AnsiRenderer.buildFrame(root)
