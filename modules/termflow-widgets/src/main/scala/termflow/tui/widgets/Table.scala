package termflow.tui.widgets

import termflow.tui.*

/**
 * Tabular view with typed columns, a header row, and a scrollable body
 * of rows with optional selection.
 *
 * Like the other stateful widgets, `Table` is value-based: state lives in
 * the app's model, `handleKey` advances it, and `view` renders a tree of
 * `VNode`s. Internally it delegates body scrolling to [[ListView.State]]
 * so navigation and viewport logic isn't duplicated.
 *
 * ## Columns
 *
 * Columns are declared up-front as `Table.Column[A]` — each knows its
 * header, cell width, alignment, and how to render one data row into a
 * string. The total rendered width is the sum of column widths plus one
 * separator cell between adjacent columns.
 *
 * {{{
 * val cols = Vector(
 *   Table.Column[Task]("Title",  width = 20, align = Table.Align.Left,  render = _.title),
 *   Table.Column[Task]("Status", width = 10, align = Table.Align.Left,  render = _.status.toString),
 *   Table.Column[Task]("Age",    width =  4, align = Table.Align.Right, render = t => t.days.toString)
 * )
 *
 * val state = Table.State.of(cols, rows = myTasks, visibleRows = 8)
 * }}}
 *
 * ## Selection & keys
 *
 * Table supports optional single-row selection. When `selectable = true`,
 * ArrowUp/Down/Home/End move the cursor and Enter dispatches the
 * `onSelect(row)` message — same contract as [[ListView]]. When
 * `selectable = false`, it's a static read-only view.
 *
 * ## Header
 *
 * The header row is always rendered, styled with `theme.primary` bold.
 * A divider row of `─` underneath separates the header from the body.
 * The total occupied height is therefore `2 + visibleRows` cells.
 */
object Table:

  /** Cell alignment within a column. */
  enum Align:
    case Left, Right, Center

  /**
   * Column definition for a [[Table]].
   *
   * @param header How the column's header cell reads.
   * @param width  Column width in cells. Content is truncated / padded to fit.
   * @param align  How to place content within the column.
   * @param render Map a row of type `A` to its display string for this column.
   */
  final case class Column[A](
    header: String,
    width: Int,
    align: Align = Align.Left,
    render: A => String
  )

  /**
   * State for a table.
   *
   * @param columns     Column definitions in display order.
   * @param rows        All data rows (not just the visible slice).
   * @param body        Scroll + selection state for the visible-rows viewport.
   * @param selectable  Whether ArrowUp/Down moves a cursor (`true`) or the
   *                    table renders read-only (`false`).
   */
  final case class State[A](
    columns: Vector[Column[A]],
    rows: Vector[A],
    body: ListView.State[A],
    selectable: Boolean
  ):

    /** `true` when the table has no data rows. */
    def isEmpty: Boolean = rows.isEmpty

    /** Total number of data rows. */
    def size: Int = rows.size

    /** Currently highlighted row, if any. Always `None` when `!selectable`. */
    def selectedRow: Option[A] =
      if selectable then body.selectedItem else None

    /** Currently highlighted row index, or `0` for non-selectable / empty tables. */
    def selectedIndex: Int = body.selected

    /** Replace the data rows, re-clamping selection / scroll. */
    def withRows(newRows: Vector[A]): State[A] =
      copy(rows = newRows, body = body.withItems(newRows))

  object State:

    /** Empty table with the given columns and viewport height. */
    def empty[A](columns: Vector[Column[A]], visibleRows: Int = 8, selectable: Boolean = true): State[A] =
      State(columns, Vector.empty[A], ListView.State.empty[A](visibleRows), selectable)

    /** Table populated with `rows`; selection (if any) parks on the first row. */
    def of[A](
      columns: Vector[Column[A]],
      rows: Vector[A],
      visibleRows: Int = 8,
      selectable: Boolean = true
    ): State[A] =
      State(columns, rows, ListView.State.of(rows, visibleRows), selectable)

  /**
   * Process one keystroke.
   *
   * When `state.selectable` is `false`, every key is a no-op — use the
   * widget purely for display. When `true`, navigation and Enter are
   * delegated to [[ListView.handleKey]], so the contract is identical
   * (ArrowUp/Down/Home/End/Enter/Space).
   */
  def handleKey[A, Msg](
    state: State[A],
    key: KeyDecoder.InputKey
  )(onSelect: A => Option[Msg]): (State[A], Option[Msg]) =
    if !state.selectable then (state, None)
    else
      val (nextBody, msg) = ListView.handleKey(state.body, key)(onSelect)
      (state.copy(body = nextBody), msg)

  /** Pad / truncate `s` to exactly `w` cells using the given alignment. */
  private def align(s: String, w: Int, align: Align): String =
    if w <= 0 then ""
    else if s.length >= w then s.take(w)
    else
      val pad = w - s.length
      align match
        case Align.Left  => s + " " * pad
        case Align.Right => " " * pad + s
        case Align.Center =>
          val left  = pad / 2
          val right = pad - left
          " " * left + s + " " * right

  /** Total visible width = sum of column widths + single-space separators between them. */
  def width[A](columns: Vector[Column[A]]): Int =
    if columns.isEmpty then 0
    else columns.map(c => math.max(0, c.width)).sum + (columns.size - 1)

  /** Total visible height: header + divider + viewport. */
  def height(visibleRows: Int): Int = 2 + math.max(1, visibleRows)

  /**
   * Assemble a single row string from column renderings, aligned and
   * separated by a single space.
   *
   * Exposed for callers who want to re-use the layout for bespoke
   * rendering (e.g. printing a table to a log file).
   */
  def formatRow[A](columns: Vector[Column[A]], row: A): String =
    columns.map(c => align(c.render(row), math.max(0, c.width), c.align)).mkString(" ")

  /** Assemble the header row string. */
  def formatHeader[A](columns: Vector[Column[A]]): String =
    columns.map(c => align(c.header, math.max(0, c.width), c.align)).mkString(" ")

  /**
   * Render the table as a composed [[BoxNode]] of exactly
   * `Table.height(visibleRows)` cells tall and `Table.width(columns)` wide.
   *
   * @param state     Table state.
   * @param at        Top-left cell. Defaults to `(1, 1)` for Layout composition.
   * @param focused   Whether the table currently has focus (affects cursor).
   */
  def view[A](
    state: State[A],
    at: Coord = Coord(XCoord(1), YCoord(1)),
    focused: Boolean = false
  )(using theme: Theme): VNode =
    val cols   = state.columns
    val totalW = math.max(1, width(cols))
    val rowsH  = math.max(1, state.body.visibleRows)

    val headerStyle  = Style(fg = theme.primary, bold = true)
    val dividerStyle = Style(fg = theme.primary)
    val headerText   = formatHeader(cols).take(totalW).padTo(totalW, ' ')
    val dividerText  = "─" * totalW

    val headerRow  = TextNode(at.x, at.y, List(Text(headerText, headerStyle)))
    val dividerRow = TextNode(at.x, at.y + 1, List(Text(dividerText, dividerStyle)))

    // Body rows: viewport starting at at.y + 2, reusing the ListView focus
    // rules (focused + selected => inverse video with ▸ prefix).
    // We hand-render the rows here rather than calling ListView.view because
    // Table has its own multi-column formatting and no "▸ " prefix column —
    // instead the entire row is painted in inverse video when selected.
    val bodyRows = (0 until rowsH).map { r =>
      val idx  = state.body.scrollOffset + r
      val rowY = at.y + 2 + r
      if idx >= state.size then TextNode(at.x, rowY, List(Text(" " * totalW, Style())))
      else
        val rowData    = state.rows(idx)
        val rendered   = formatRow(cols, rowData).take(totalW).padTo(totalW, ' ')
        val isSelected = state.selectable && idx == state.body.selected
        val showCursor = isSelected && focused
        val style =
          if showCursor then Style(fg = theme.background, bg = theme.primary)
          else Style(fg = theme.foreground)
        TextNode(at.x, rowY, List(Text(rendered, style)))
    }.toList

    BoxNode(
      at.x,
      at.y,
      totalW,
      height(rowsH),
      children = List(headerRow, dividerRow) ++ bodyRows,
      style = Style()
    )
