package termflow.tui

/**
 * Minimal layout DSL for composing TermFlow views without hand-rolling
 * absolute coordinates.
 *
 * The renderer is still absolute-coordinate only — `Layout` is a build-time
 * helper that walks a container tree and emits absolute-positioned [[VNode]]s
 * that the existing `AnsiRenderer` consumes unchanged. No changes to
 * `TuiApp`, `TuiRuntime`, or the render pipeline are required; apps opt in
 * by building a `Layout` inside `view` and calling `.resolve` or
 * `.toRootNode` to feed [[RootNode]].
 *
 * ## Quick example
 *
 * {{{
 * import termflow.tui.*
 * import termflow.tui.TuiPrelude.*
 *
 * override def view(m: Model): RootNode =
 *   Layout.column(gap = 1)(
 *     TextNode(1.x, 1.y, List(Text("Status: ready", Style(fg = Color.Green)))),
 *     Layout.row(gap = 2)(
 *       TextNode(1.x, 1.y, List(Text("A", Style()))),
 *       TextNode(1.x, 1.y, List(Text("B", Style()))),
 *       TextNode(1.x, 1.y, List(Text("C", Style())))
 *     ).asLayout
 *   ).toRootNode(width = m.terminalWidth, height = m.terminalHeight)
 * }}}
 *
 * ## Semantics
 *
 * Each `Layout` value has a natural size returned by [[Layout.measure]]:
 *
 *   - `Elem(v)` — size of the wrapped node (text width, box dimensions, etc.)
 *   - `Row(gap, cs)` — `sum(childWidths) + gap * (n - 1)` wide, `max(childHeights)` tall
 *   - `Column(gap, cs)` — `max(childWidths)` wide, `sum(childHeights) + gap * (n - 1)` tall
 *   - `Spacer(w, h)` — exactly `(w, h)`
 *
 * Empty `Row` / `Column` measure as `(0, 0)` and resolve to the empty list.
 *
 * [[Layout.resolve]] walks the tree, placing children left-to-right (Row)
 * or top-to-bottom (Column) starting from the supplied origin. Each child
 * is positioned at the next free slot using its own measured width/height.
 * Alignment is always start-of-axis; see the "Out of scope" notes below.
 *
 * ## Elem translation rule
 *
 * `Layout.Elem(vnode)` wraps a `VNode` that was authored with its own
 * absolute coordinates (e.g. `TextNode(1.x, 1.y, ...)`). When the layout
 * resolver places the element at `at`, it shifts the wrapped node by
 * `(at.x - vnode.x, at.y - vnode.y)` — effectively repositioning the node
 * to the container's chosen slot while preserving internal relative layout
 * (e.g. child offsets inside a [[BoxNode]]).
 *
 * Common convention: author the wrapped node at `(1.x, 1.y)` so the
 * translation is just "move to `at`".
 *
 * ## Out of scope (v1)
 *
 *   - Alignment / justification (children always at the start of the axis)
 *   - Flex weights / grow / shrink
 *   - Padding wrappers (use [[Layout.Spacer]] between siblings)
 *   - Clipping / overflow handling
 *   - Z-ordering of overlapping children
 */
enum Layout:

  /**
   * Wrap a concrete [[VNode]] as a layout leaf.
   *
   * The wrapped node's absolute coordinates are overridden by the container:
   * when the layout resolver places the element at `at`, the node is
   * translated so its own `(x, y)` becomes `(at.x, at.y)`. See the
   * "Elem translation rule" section on [[Layout]].
   */
  case Elem(vnode: VNode)

  /**
   * Horizontal flow: children are placed left-to-right, top-aligned, with
   * `gap` empty cells between siblings.
   *
   * @param gap Number of cells between adjacent children. Negative values
   *            are treated as `0`.
   * @param children Layout items in render order (first = leftmost).
   */
  case Row(gap: Int, children: List[Layout])

  /**
   * Vertical flow: children are placed top-to-bottom, left-aligned, with
   * `gap` empty rows between siblings.
   *
   * @param gap Number of rows between adjacent children. Negative values
   *            are treated as `0`.
   * @param children Layout items in render order (first = topmost).
   */
  case Column(gap: Int, children: List[Layout])

  /**
   * Empty rectangle used to reserve space without drawing anything.
   *
   * Useful for pushing siblings apart inside a `Row` / `Column` — for
   * example `Row(0, List(Elem(a), Spacer(4, 1), Elem(b)))` places `b`
   * four cells to the right of `a`.
   */
  case Spacer(width: Int, height: Int)

  /** Natural `(width, height)` of this layout, in cells. */
  def measure: (Int, Int) = Layout.measure(this)

  /**
   * Resolve this layout into a flat list of absolute-positioned [[VNode]]s
   * starting at `at`.
   *
   * The returned list is in render order. Callers typically pass it as the
   * `children` argument of a [[RootNode]]; use [[toRootNode]] for a slightly
   * more concise variant.
   */
  def resolve(at: Coord = Coord(XCoord(1), YCoord(1))): List[VNode] =
    Layout.resolve(this, at)

  /**
   * Resolve this layout at `(1, 1)` and wrap the result in a [[RootNode]].
   *
   * @param width Terminal width to advertise on the root.
   * @param height Terminal height to advertise on the root.
   * @param input Optional focused input to attach to the root.
   * @param at Origin for the layout resolver. Defaults to `(1, 1)`.
   */
  def toRootNode(
    width: Int,
    height: Int,
    input: Option[InputNode] = None,
    at: Coord = Coord(XCoord(1), YCoord(1))
  ): RootNode =
    RootNode(width, height, resolve(at), input)

object Layout:

  /**
   * Fluent constructor for a [[Row]] over bare [[VNode]]s. Each vnode is
   * auto-wrapped in an [[Elem]].
   *
   * {{{
   * Layout.row(gap = 1)(
   *   TextNode(1.x, 1.y, List(Text("A", Style()))),
   *   TextNode(1.x, 1.y, List(Text("B", Style())))
   * )
   * }}}
   */
  def row(gap: Int)(children: VNode*): Layout =
    Row(math.max(0, gap), children.toList.map(Elem.apply))

  /**
   * Fluent constructor for a [[Column]] over bare [[VNode]]s. Each vnode is
   * auto-wrapped in an [[Elem]].
   */
  def column(gap: Int)(children: VNode*): Layout =
    Column(math.max(0, gap), children.toList.map(Elem.apply))

  /** Extension lifting a [[VNode]] into a [[Layout.Elem]]. */
  extension (v: VNode) def asLayout: Layout = Elem(v)

  /**
   * Natural size of a layout, ignoring any container it will be placed in.
   *
   * Public for tests and for callers who want to size a surrounding box
   * from measured layout dimensions.
   */
  def measure(layout: Layout): (Int, Int) = layout match
    case Elem(v)      => measureVNode(v)
    case Spacer(w, h) => (math.max(0, w), math.max(0, h))

    case Row(gap, cs) =>
      if cs.isEmpty then (0, 0)
      else
        val g      = math.max(0, gap)
        val sizes  = cs.map(measure)
        val width  = sizes.map(_._1).sum + g * (cs.size - 1)
        val height = sizes.map(_._2).max
        (width, height)

    case Column(gap, cs) =>
      if cs.isEmpty then (0, 0)
      else
        val g      = math.max(0, gap)
        val sizes  = cs.map(measure)
        val width  = sizes.map(_._1).max
        val height = sizes.map(_._2).sum + g * (cs.size - 1)
        (width, height)

  /** Natural `(width, height)` of a concrete [[VNode]]. */
  def measureVNode(v: VNode): (Int, Int) = v match
    case TextNode(_, _, segments) =>
      val width = segments.foldLeft(0)((acc, seg) => acc + seg.txt.length)
      (width, 1)

    case BoxNode(_, _, w, h, _, _) =>
      (math.max(0, w), math.max(0, h))

    case InputNode(_, _, prompt, _, _, lineWidth, _) =>
      val width = if lineWidth > 0 then lineWidth else prompt.length
      (width, 1)

  /**
   * Resolve a layout into absolute-positioned [[VNode]]s starting at `at`.
   *
   * The instance method [[Layout.resolve]] delegates here — prefer calling
   * the method form for readability.
   */
  def resolve(layout: Layout, at: Coord): List[VNode] = layout match
    case Elem(v) =>
      val dx = at.x.value - v.x.value
      val dy = at.y.value - v.y.value
      if dx == 0 && dy == 0 then List(v)
      else List(translate(v, dx, dy))

    case Spacer(_, _) => Nil

    case Row(gap, cs) =>
      val g       = math.max(0, gap)
      var xCursor = at.x.value
      val out     = List.newBuilder[VNode]
      var i       = 0
      val n       = cs.size
      while i < n do
        val child   = cs(i)
        val (cw, _) = measure(child)
        val placed  = resolve(child, Coord(XCoord(xCursor), at.y))
        out ++= placed
        xCursor += cw
        if i < n - 1 then xCursor += g
        i += 1
      out.result()

    case Column(gap, cs) =>
      val g       = math.max(0, gap)
      var yCursor = at.y.value
      val out     = List.newBuilder[VNode]
      var i       = 0
      val n       = cs.size
      while i < n do
        val child   = cs(i)
        val (_, ch) = measure(child)
        val placed  = resolve(child, Coord(at.x, YCoord(yCursor)))
        out ++= placed
        yCursor += ch
        if i < n - 1 then yCursor += g
        i += 1
      out.result()

  /**
   * Translate a [[VNode]] by `(dx, dy)`, including any nested [[BoxNode]]
   * children so that relative offsets are preserved.
   *
   * Exposed for tests; the resolver uses it internally for [[Elem]] nodes.
   */
  def translate(v: VNode, dx: Int, dy: Int): VNode = v match
    case tn: TextNode =>
      tn.copy(x = tn.x + dx, y = tn.y + dy)

    case bn: BoxNode =>
      bn.copy(
        x = bn.x + dx,
        y = bn.y + dy,
        children = bn.children.map(translate(_, dx, dy))
      )

    case in: InputNode =>
      in.copy(x = in.x + dx, y = in.y + dy)
