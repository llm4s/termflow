package termflow.tui.widgets

import termflow.tui.*

/**
 * Generic tree-view widget. Renders a collapsible hierarchy as a flat
 * list of rows, one per visible node.
 *
 * The widget is structure-agnostic: apps describe their own tree by
 * implementing [[Tree.Children]] (or supplying a function), label nodes
 * via a `render` callback, and own the expanded / selected state. The
 * widget walks the structure each frame and produces the visible row
 * list — no internal state, no in-place mutation.
 *
 * ## Conventions
 *
 *   - **Indentation.** Each level adds two cells of left padding so
 *     ancestor relationships are visually obvious. Override with
 *     `indentWidth`.
 *   - **Markers.** Internal nodes (those with children) get a `+`
 *     (collapsed, "click to open") or `-` (expanded, "click to
 *     collapse") glyph, three columns wide (`[+] ` / `[-] `). Leaves
 *     get four spaces of leading padding. The `unicode` flag is kept
 *     for backwards compatibility but no longer changes the glyph
 *     since `+` / `-` are already ASCII.
 *   - **Selection.** The row at `selectedIndex` (an index into the
 *     *visible* row list) is highlighted in the theme's primary slot,
 *     bolded.
 *   - **Identity.** Apps decide what an "expanded" node is via
 *     [[Tree.Children.id]] (a stable key) and a `Set[Id]` of expanded
 *     ids. Re-using the same id across frames is what keeps state
 *     stable as the tree changes shape.
 *
 * ## Example
 *
 * {{{
 * given Theme = Theme.dark
 *
 * sealed trait Node:
 *   def name: String
 * case class Dir(name: String, kids: Vector[Node]) extends Node
 * case class File(name: String) extends Node
 *
 * given Tree.Children[Node, String] with
 *   def id(n: Node): String = n.name
 *   def kids(n: Node): Vector[Node] = n match
 *     case Dir(_, k) => k
 *     case _: File   => Vector.empty
 *
 * Tree(
 *   roots         = Vector(Dir("src", Vector(File("Main.scala")))),
 *   expanded      = Set("src"),
 *   selectedIndex = 0,
 *   render        = _.name
 * )
 * }}}
 */
object Tree:

  /** Row that the widget renders for each visible node. */
  final case class Row[A](
    /**
     * Original node — useful if the app wants the full node back from
     *  a click position.
     */
    node: A,
    /** Stable id (per [[Children.id]]). */
    id: String,
    /** Indent depth — `0` for root entries. */
    depth: Int,
    /** True if the node has any children at all. */
    hasChildren: Boolean,
    /** True if `id` is in the `expanded` set. */
    expanded: Boolean
  )

  /** Type-class describing how to traverse a custom tree structure. */
  trait Children[A, Id]:
    def id(node: A): Id
    def kids(node: A): Vector[A]

  /**
   * Walk the tree honouring the expanded set; produce the flat row list
   *  the widget renders. Public so apps can pre-compute it (e.g. for
   *  click-to-row mapping outside the widget itself).
   */
  def visibleRows[A, Id](
    roots: Vector[A],
    expanded: Set[Id]
  )(using c: Children[A, Id]): Vector[Row[A]] =
    val builder = Vector.newBuilder[Row[A]]
    def walk(node: A, depth: Int): Unit =
      val kids   = c.kids(node)
      val nodeId = c.id(node)
      val isOpen = expanded.contains(nodeId)
      builder += Row(
        node = node,
        id = nodeId.toString,
        depth = depth,
        hasChildren = kids.nonEmpty,
        expanded = isOpen
      )
      if isOpen then kids.foreach(k => walk(k, depth + 1))
    roots.foreach(walk(_, 0))
    builder.result()

  // Marker glyphs. `[+] ` for collapsed, `[-] ` for expanded —
  // unambiguous click targets that survive any font / locale combo.
  // Leaves get four spaces so the label column lines up across rows.
  private val expandedGlyph  = "[-] "
  private val collapsedGlyph = "[+] "
  private val leafGlyph      = "    "

  /**
   * Render the tree as a list of `TextNode`s.
   *
   * @param roots         Root nodes drawn in document order.
   * @param expanded      Set of expanded node ids.
   * @param selectedIndex Visible-row index of the highlighted row, or
   *                      `-1` for no selection.
   * @param render        Label for a node.
   * @param at            Top-left cell of the first row.
   * @param indentWidth   Cells added per depth level (default 2).
   * @param unicode       Whether to emit Unicode chevrons or ASCII
   *                      `v ` / `> ` fallbacks.
   */
  def apply[A, Id](
    roots: Vector[A],
    expanded: Set[Id],
    selectedIndex: Int,
    render: A => String,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    indentWidth: Int = 2,
    unicode: Boolean = true
  )(using c: Children[A, Id], theme: Theme): List[VNode] =
    val rows = visibleRows(roots, expanded)
    rows.zipWithIndex.toList.map { case (row, i) =>
      val isSelected = i == selectedIndex
      val style =
        if isSelected then Style(fg = theme.background, bg = theme.primary, bold = true)
        else Style(fg = theme.foreground)
      val pad = " " * (row.depth * indentWidth)
      val marker =
        if !row.hasChildren then leafFor(unicode)
        else if row.expanded then expandedFor(unicode)
        else collapsedFor(unicode)
      TextNode(at.x, at.y + i, List(Text(s"$pad$marker${render(row.node)}", style)))
    }

  /** Marker glyph for an expanded internal node. */
  def expandedFor(unicode: Boolean = true): String =
    val _ = unicode // glyphs are now ASCII-only; param kept for source compat
    expandedGlyph

  /** Marker glyph for a collapsed internal node. */
  def collapsedFor(unicode: Boolean = true): String =
    val _ = unicode
    collapsedGlyph

  /** Padding used in place of a marker for leaf nodes. */
  def leafFor(unicode: Boolean = true): String =
    val _ = unicode
    leafGlyph

  /**
   * Region of a tree row a click landed in. Apps map a
   * [[HitResult.Chevron]] to "toggle expand" and a
   * [[HitResult.Label]] to "select this row".
   */
  enum HitResult:
    case Chevron(rowIndex: Int)
    case Label(rowIndex: Int)

  /**
   * Map a click `(col, row)` to a [[HitResult]] over the rendered
   * row list.
   *
   * The chevron region is the column span occupied by the `[+] ` /
   * `[-] ` glyph (skipped for leaves since they have no toggle). Apps
   * pre-compute `rows = Tree.visibleRows(...)` once per frame and pass
   * it in alongside the same `at` / `indentWidth` they used to render.
   *
   * Returns `None` for clicks outside any rendered row.
   *
   * @param rows         Row list returned by [[visibleRows]].
   * @param at           Top-left of the first row (mirrors the
   *                     rendering call).
   * @param indentWidth  Cells per depth level (mirrors the rendering call).
   * @param col          Click column (1-based, absolute).
   * @param row          Click row (1-based, absolute).
   * @param labelLength  Maximum label width for the click target.
   *                     Pass `Int.MaxValue` if rows can be any length.
   */
  def hitTest[A](
    rows: Vector[Row[A]],
    at: Coord,
    indentWidth: Int,
    col: Int,
    row: Int,
    labelLength: Int = Int.MaxValue
  ): Option[HitResult] =
    val rowIdx = row - at.y.value
    if rowIdx < 0 || rowIdx >= rows.size then None
    else
      val r           = rows(rowIdx)
      val rowLeft     = at.x.value
      val pad         = r.depth * indentWidth
      val markerLeft  = rowLeft + pad
      val markerLen   = if r.hasChildren then expandedGlyph.length else leafGlyph.length
      val markerRight = markerLeft + markerLen - 1
      val labelLeft   = markerLeft + markerLen
      val labelRight  = labelLeft + math.max(0, labelLength) - 1
      if r.hasChildren && col >= markerLeft && col <= markerRight then Some(HitResult.Chevron(rowIdx))
      else if col >= labelLeft && col <= labelRight then Some(HitResult.Label(rowIdx))
      else None

  /** Cell width of a tree row at `depth` carrying the given label. */
  def rowWidth(label: String, depth: Int, indentWidth: Int = 2, unicode: Boolean = true): Int =
    depth * indentWidth + leafFor(unicode).length + label.length
