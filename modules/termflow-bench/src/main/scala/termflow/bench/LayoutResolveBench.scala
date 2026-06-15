package termflow.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import termflow.tui.{ Layout, VNode }

/**
 * Microbenchmark for [[termflow.tui.Layout.resolve]] / `resolveTo` over the
 * shared [[Fixtures]] trees.
 *
 * Mirrors [[LayoutMeasureBench]]: the `(shape, size)` axes are JMH `@Param`s,
 * trees are built once in [[setup]], and each `@Benchmark` returns the
 * resolved `List[VNode]` so dead-code elimination can't drop the work.
 *
 *   - [[resolveNatural]] resolves at the natural size (the unbounded
 *     `availableWidth = availableHeight = -1` path that `Layout.resolve`
 *     takes).
 *   - [[resolveBudgeted]] resolves against a fixed terminal-sized budget,
 *     exercising the `Fill` / even-split distribution code in `resolveTo`.
 */
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class LayoutResolveBench:

  /** A representative bounded budget for the budgeted resolve path. */
  private val BudgetWidth  = 120
  private val BudgetHeight = 40

  @Param(Array("flatRow", "nestedColumn", "grid", "fillRow"))
  var shape: String = ""

  @Param(Array("small", "medium", "large"))
  var size: String = ""

  private var layout: Layout = null

  @Setup
  def setup(): Unit =
    layout = Fixtures.tree(shape, size)

  @Benchmark
  def resolveNatural: List[VNode] =
    Layout.resolve(layout, Fixtures.Origin)

  @Benchmark
  def resolveBudgeted: List[VNode] =
    Layout.resolveTo(layout, Fixtures.Origin, BudgetWidth, BudgetHeight)
