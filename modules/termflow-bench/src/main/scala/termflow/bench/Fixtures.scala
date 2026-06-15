package termflow.bench

import termflow.tui.*
import termflow.tui.ScreenPrelude.*

/**
 * Deterministic, terminal-free [[termflow.tui.Layout]] trees shared by the
 * Layout microbenchmarks ([[LayoutMeasureBench]], [[LayoutResolveBench]]).
 *
 * Three representative shapes, each available at a few sizes, exercise the
 * distinct code paths in `Layout.measure` / `Layout.resolve`:
 *
 *   - `flatRow`      — a single [[termflow.tui.Layout.Row]] of leaf text
 *     nodes, built with the public `Layout.row` helper (one level deep).
 *   - `nestedColumn` — a [[termflow.tui.Layout.Column]] of
 *     [[termflow.tui.Layout.Row]]s, built from the enum cases directly because
 *     the fluent helpers only accept leaf `VNode` children.
 *   - `grid`         — a fixed-column [[termflow.tui.Layout.Grid]] of
 *     [[termflow.tui.GridCell]]s.
 *
 * All trees are pure data: no terminal, no I/O, no randomness — the same
 * `(shape, size)` always yields the same tree, so benchmark runs are
 * comparable across machines and JVM invocations.
 */
object Fixtures:

  /** Benchmark shape axis — drives [[tree]]'s structure. */
  val Shapes: List[String] = List("flatRow", "nestedColumn", "grid")

  /** Benchmark size axis — drives the child / cell counts in [[tree]]. */
  val Sizes: List[String] = List("small", "medium", "large")

  /** Origin used by the resolve benchmarks (1-based top-left). */
  val Origin: Coord = Coord(XCoord(1), YCoord(1))

  /** A leaf text node authored at the layout-friendly `(1, 1)` origin. */
  private def leaf(label: String): VNode =
    TextNode(1.x, 1.y, List(label.text))

  /** Per-size counts: (number of items along the major axis). */
  private def count(size: String): Int = size match
    case "small"  => 4
    case "medium" => 16
    case "large"  => 64
    case other    => sys.error(s"unknown size: $other")

  /**
   * Build the layout tree for a `(shape, size)` pair.
   *
   * @throws RuntimeException for an unrecognised shape or size.
   */
  def tree(shape: String, size: String): Layout =
    val n = count(size)
    shape match
      case "flatRow"      => flatRow(n)
      case "nestedColumn" => nestedColumn(n)
      case "grid"         => grid(n)
      case other          => sys.error(s"unknown shape: $other")

  /** A single Row of `n` leaf text nodes (built via the public helper). */
  private def flatRow(n: Int): Layout =
    Layout.row(gap = 1)((0 until n).map(i => leaf(s"cell$i"))*)

  /**
   * A Column of `rows` Rows, each holding `cols` leaf cells. Built from the
   * `Layout.Column` / `Layout.Row` case constructors since the fluent helpers
   * can't take Layout children. `rows * cols ≈ n` keeps the node count
   * comparable to the flat shape at the same size.
   */
  private def nestedColumn(n: Int): Layout =
    val cols = math.max(2, math.sqrt(n.toDouble).round.toInt)
    val rows = math.max(1, (n + cols - 1) / cols)
    val rowLayouts = (0 until rows).map { r =>
      val cells = (0 until cols).map(c => Layout.Elem(leaf(s"r${r}c$c"))).toList
      Layout.Row(gap = 1, cells)
    }.toList
    Layout.Column(gap = 1, rowLayouts)

  /**
   * A fixed-column Grid of `n` single-span cells. Built from `Layout.Grid` +
   * [[termflow.tui.GridCell]] directly to exercise the grid placement /
   * sizing path.
   */
  private def grid(n: Int): Layout =
    val cols  = math.max(2, math.sqrt(n.toDouble).round.toInt)
    val cells = (0 until n).map(i => GridCell(Layout.Elem(leaf(s"g$i")))).toList
    Layout.Grid(columns = cols, rowGap = 1, colGap = 2, cells = cells)
