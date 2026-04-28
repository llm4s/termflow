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
 *   - **Markers.** Internal nodes (those with children) get a `▾`
 *     (expanded) or `▸` (collapsed) chevron, two columns wide
 *     (chevron + space). Leaves get two spaces. ASCII fallbacks are
 *     `v ` / `> ` / `  ` when `unicode = false`.
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

  // Marker glyphs.
  private val expandedGlyph       = "▾ "
  private val collapsedGlyph      = "▸ "
  private val leafGlyph           = "  "
  private val expandedGlyphAscii  = "v "
  private val collapsedGlyphAscii = "> "
  private val leafGlyphAscii      = "  "

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
    if unicode then expandedGlyph else expandedGlyphAscii

  /** Marker glyph for a collapsed internal node. */
  def collapsedFor(unicode: Boolean = true): String =
    if unicode then collapsedGlyph else collapsedGlyphAscii

  /** Padding used in place of a marker for leaf nodes. */
  def leafFor(unicode: Boolean = true): String =
    if unicode then leafGlyph else leafGlyphAscii

  /** Cell width of a tree row at `depth` carrying the given label. */
  def rowWidth(label: String, depth: Int, indentWidth: Int = 2, unicode: Boolean = true): Int =
    depth * indentWidth + leafFor(unicode).length + label.length
