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

  /**
   * Wrap `content` so that it consumes whatever space is left along the
   * parent container's major axis (width in a [[Row]], height in a
   * [[Column]]) after fixed-size siblings have been allocated.
   *
   * `Fill` only takes effect when the layout is resolved with a target
   * size — i.e. when it appears inside a [[RootNode]] resolved by the
   * renderer at render time, or when [[Layout.resolveTo]] is called
   * directly. Resolving without an axis target (the default
   * [[Layout.resolve]]) treats `Fill` as its inner content's natural size
   * — same shape as today's behaviour.
   *
   * Multiple `Fill` siblings in the same axis split the remaining space
   * evenly. There is no `Weight` yet; add when a real consumer wants
   * uneven distribution.
   *
   * @param content The child whose major-axis size is variable. Its minor
   *                axis (height inside a Row, width inside a Column) is
   *                its natural size.
   */
  case Fill(content: Layout)

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
    case Fill(inner)  => measure(inner)

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

    case BoxNode(_, _, w, h, _, _, _) =>
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
  def resolve(layout: Layout, at: Coord): List[VNode] =
    resolveTo(layout, at, availableWidth = -1, availableHeight = -1)

  /**
   * Resolve a layout into absolute-positioned [[VNode]]s with optional
   * axis-size constraints.
   *
   * Pass `-1` for either dimension to mean "no constraint" — `Fill`
   * children in that axis fall back to their natural size, exactly the
   * old behaviour. Passing a non-negative value enables `Fill` to consume
   * whatever's left after fixed-size siblings; multiple Fill siblings
   * split the remainder evenly (any leftover cells go to the last Fill
   * child so totals add up exactly).
   *
   * @param at              Top-left of the layout's allocated rectangle.
   * @param availableWidth  Width budget for the major axis of a [[Row]]
   *                        (or just the layout, for an outermost call).
   *                        `-1` = unbounded.
   * @param availableHeight Height budget for the major axis of a
   *                        [[Column]]. `-1` = unbounded.
   */
  def resolveTo(layout: Layout, at: Coord, availableWidth: Int, availableHeight: Int): List[VNode] =
    layout match
      case Elem(v) =>
        val dx = at.x.value - v.x.value
        val dy = at.y.value - v.y.value
        if dx == 0 && dy == 0 then List(v)
        else List(translate(v, dx, dy))

      case Spacer(_, _) => Nil

      case Fill(inner) =>
        inner match
          case Elem(v) if availableWidth >= 0 || availableHeight >= 0 =>
            // Actively resize the wrapped vnode to consume the assigned
            // budget. Only BoxNodes can be resized; text/input keep their
            // natural width — Fill around them just reserves space.
            val resized = resizeForFill(v, availableWidth, availableHeight)
            val dx      = at.x.value - resized.x.value
            val dy      = at.y.value - resized.y.value
            if dx == 0 && dy == 0 then List(resized)
            else List(translate(resized, dx, dy))
          case _ =>
            // Forward the budget into nested Row/Column/Fill etc.
            resolveTo(inner, at, availableWidth, availableHeight)

      case Row(gap, cs) =>
        val g         = math.max(0, gap)
        val n         = cs.size
        val widths    = distributeMajor(cs, availableWidth, g, axisIsRow = true)
        val rowHeight = if availableHeight >= 0 then availableHeight else cs.map(measure(_)._2).maxOption.getOrElse(0)
        var xCursor   = at.x.value
        val out       = List.newBuilder[VNode]
        var i         = 0
        while i < n do
          val child = cs(i)
          val cw    = widths(i)
          out ++= resolveTo(child, Coord(XCoord(xCursor), at.y), cw, rowHeight)
          xCursor += cw
          if i < n - 1 then xCursor += g
          i += 1
        out.result()

      case Column(gap, cs) =>
        val g        = math.max(0, gap)
        val n        = cs.size
        val heights  = distributeMajor(cs, availableHeight, g, axisIsRow = false)
        val colWidth = if availableWidth >= 0 then availableWidth else cs.map(measure(_)._1).maxOption.getOrElse(0)
        var yCursor  = at.y.value
        val out      = List.newBuilder[VNode]
        var i        = 0
        while i < n do
          val child = cs(i)
          val ch    = heights(i)
          out ++= resolveTo(child, Coord(at.x, YCoord(yCursor)), colWidth, ch)
          yCursor += ch
          if i < n - 1 then yCursor += g
          i += 1
        out.result()

  /**
   * Compute per-child major-axis sizes for a Row/Column under an optional
   * total budget. Fill children share the budget left over after fixed
   * siblings; without a budget they get their natural major-axis size.
   */
  private def distributeMajor(
    children: List[Layout],
    available: Int,
    gap: Int,
    axisIsRow: Boolean
  ): IndexedSeq[Int] =
    val n = children.size
    if n == 0 then return IndexedSeq.empty[Int]
    val natural = children.map { c =>
      val (w, h) = measure(c)
      if axisIsRow then w else h
    }
    val isFill  = children.map(_.isInstanceOf[Fill]).toIndexedSeq
    val anyFill = isFill.contains(true)

    if !anyFill || available < 0 then
      // No fill children, or no budget — children take their natural size.
      natural.toIndexedSeq
    else
      val totalGap  = gap * math.max(0, n - 1)
      val fixedSize = natural.zip(isFill).collect { case (sz, false) => sz }.sum
      val remaining = math.max(0, available - fixedSize - totalGap)
      val fillCount = isFill.count(identity)
      val per       = if fillCount > 0 then remaining / fillCount else 0
      val leftover  = if fillCount > 0 then remaining - per * fillCount else 0
      var fillSeen  = 0
      val sizes = natural.toIndexedSeq.zip(isFill).zipWithIndex.map { case ((nat, fill), _) =>
        if fill then
          fillSeen += 1
          // Last Fill absorbs any leftover so totals add up exactly.
          if fillSeen == fillCount then per + leftover else per
        else nat
      }
      sizes

  /**
   * Resize a leaf VNode to honour a Fill budget. Only [[BoxNode]] is
   * resizable in the current model; text and input nodes are returned
   * unchanged (Fill around them effectively just reserves the budget).
   *
   * A negative dimension means "no constraint in that axis"; the natural
   * size is kept.
   */
  private def resizeForFill(v: VNode, availableWidth: Int, availableHeight: Int): VNode = v match
    case bn: BoxNode =>
      val newW = if availableWidth >= 0 then availableWidth else bn.width
      val newH = if availableHeight >= 0 then availableHeight else bn.height
      bn.copy(width = newW, height = newH)
    case other => other

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
