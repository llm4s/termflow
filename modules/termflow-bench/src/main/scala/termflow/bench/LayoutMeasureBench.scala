package termflow.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import termflow.tui.Layout

/**
 * Microbenchmark for [[termflow.tui.Layout.measure]] over the shared
 * [[Fixtures]] trees.
 *
 * The `(shape, size)` axes are JMH `@Param`s so a single run sweeps every
 * representative tree. Trees are built once in [[setup]] (so the measured
 * region only times `measure`, not construction) and the result tuple is
 * returned from the `@Benchmark` so the JIT can't eliminate it.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class LayoutMeasureBench:

  @Param(Array("flatRow", "nestedColumn", "grid", "fillRow"))
  var shape: String = ""

  @Param(Array("small", "medium", "large"))
  var size: String = ""

  private var layout: Layout = null

  @Setup
  def setup(): Unit =
    layout = Fixtures.tree(shape, size)

  @Benchmark
  def measure: (Int, Int) =
    Layout.measure(layout)
