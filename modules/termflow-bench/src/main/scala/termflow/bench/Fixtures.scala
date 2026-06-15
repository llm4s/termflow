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
 *   - `fillRow`      — a [[termflow.tui.Layout.Row]] of multiple
 *     [[termflow.tui.Layout.Fill]] siblings, so the budgeted resolve path
 *     (`resolveTo` with a non-negative budget) exercises the Fill / even-split
 *     distribution branch — natural sizing leaves that branch untouched.
 *
 * All trees are pure data: no terminal, no I/O, no randomness — the same
 * `(shape, size)` always yields the same tree, so benchmark runs are
 * comparable across machines and JVM invocations.
 */
object Fixtures:

  /** Benchmark shape axis — drives [[tree]]'s structure. */
  val Shapes: List[String] = List("flatRow", "nestedColumn", "grid", "fillRow")

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
      case "fillRow"      => fillRow(n)
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

  /**
   * A Row of `n` [[termflow.tui.Layout.Fill]] siblings, each wrapping a leaf.
   * Under a budgeted `resolveTo` the Fills split the available major axis
   * evenly, so this is the shape that actually drives the Fill distribution
   * branch (every other shape leaves it dormant).
   */
  private def fillRow(n: Int): Layout =
    val children = (0 until n).map(i => Layout.Fill(Layout.Elem(leaf(s"fill$i")))).toList
    Layout.Row(gap = 1, children)

  // --- Render fixtures (BuildFrameBench / AnsiDiffBench) ---------------------
  //
  // A representative, terminal-sized [[RootNode]] for the rendering
  // microbenchmarks. It is a full-screen bordered [[BoxNode]] over a stack of
  // text rows that nearly fill the interior — dense enough that
  // `AnsiRenderer.buildFrame` populates most of the cell grid and `diff` has a
  // realistic amount of content to compare.

  /** Representative frame dimensions (a typical wide terminal). */
  val ScreenWidth: Int  = 120
  val ScreenHeight: Int = 40

  /** Diff scenario axis — drives the before/after pair in [[diffRoots]]. */
  val DiffScenarios: List[String] = List("identical", "singleCell", "singleRow", "fullScreen")

  /** Number of interior text rows (everything between the box borders). */
  private def interiorRows: Int = math.max(1, ScreenHeight - 2)

  /** Interior content width (between the left/right box borders). */
  private def interiorWidth: Int = math.max(1, ScreenWidth - 2)

  /** A dense, deterministic row of the given content for interior row `r`. */
  private def baseRow(r: Int): String =
    val label = s"row $r "
    (label * (interiorWidth / label.length + 1)).take(interiorWidth)

  /** A fully different row of the same width, used for changed rows. */
  private def changedRow(r: Int): String =
    val label = s"CHANGED $r "
    (label * (interiorWidth / label.length + 1)).take(interiorWidth)

  /** Flip a single character near the middle of `s` (one-cell change). */
  private def flipOneChar(s: String): String =
    if s.isEmpty then s
    else
      val i = s.length / 2
      s.updated(i, if s(i) == '#' then '*' else '#')

  /**
   * Build a full-screen [[RootNode]] from `rows` interior strings: a bordered
   * box plus one [[TextNode]] per interior row, anchored inside the border.
   */
  private def screenOf(rows: IndexedSeq[String]): RootNode =
    val border = BoxNode(1.x, 1.y, ScreenWidth, ScreenHeight, Nil, Style(border = true))
    val texts = rows.zipWithIndex.map { case (s, i) =>
      TextNode(2.x, (i + 2).y, List(s.take(interiorWidth).text))
    }.toList
    RootNode(ScreenWidth, ScreenHeight, border :: texts, None)

  /** The representative root for [[BuildFrameBench]]. */
  def screenRoot: RootNode =
    screenOf((0 until interiorRows).map(baseRow))

  /**
   * A controlled `(before, after)` [[RootNode]] pair for the diff benchmark.
   *
   *   - `identical`  — same content, so `diff` is a no-op (0 changed cells).
   *   - `singleCell` — one character on one middle row differs.
   *   - `singleRow`  — one whole middle row is replaced.
   *   - `fullScreen` — every interior row is replaced.
   *
   * @throws RuntimeException for an unrecognised scenario.
   */
  def diffRoots(scenario: String): (RootNode, RootNode) =
    val base = (0 until interiorRows).map(baseRow)
    val mid  = interiorRows / 2
    val after = scenario match
      case "identical"  => base
      case "singleCell" => base.updated(mid, flipOneChar(base(mid)))
      case "singleRow"  => base.updated(mid, changedRow(mid))
      case "fullScreen" => base.indices.map(changedRow)
      case other        => sys.error(s"unknown diff scenario: $other")
    (screenOf(base), screenOf(after))
