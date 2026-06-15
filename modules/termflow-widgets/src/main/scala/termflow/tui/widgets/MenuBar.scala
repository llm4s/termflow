package termflow.tui.widgets

import termflow.tui.*

/**
 * Horizontal menu bar with optional dropdowns.
 *
 * Renders a single row of menu titles. When a menu is "open" the bar
 * also draws its dropdown rows directly beneath the title. Apps drive
 * navigation by updating [[MenuBar.State]] in their model — Form
 * doesn't own a key dispatcher; instead it ships a [[handleKey]]
 * helper that can be wired into the app's update.
 *
 * ## State model
 *
 *   - `menus: Vector[Menu]`  — the bar itself.
 *   - `openIdx: Option[Int]` — index of the currently-open menu
 *     (`None` = bar is collapsed).
 *   - `cursor: Int`          — highlighted menu (when bar has focus
 *     but nothing is open) OR highlighted item inside the open menu.
 *
 * The same `cursor` field doubles as both meanings depending on
 * `openIdx` — cleanest from the model side, the widget interprets it
 * appropriately when rendering.
 *
 * {{{
 * given Theme = Theme.dark
 * val initial = MenuBar.State(
 *   menus = Vector(
 *     MenuBar.Menu("File", Vector("New", "Open…", "Save", "Quit")),
 *     MenuBar.Menu("Edit", Vector("Cut", "Copy", "Paste"))
 *   )
 * )
 * }}}
 */
object MenuBar:

  /** A single menu — a title plus its dropdown items. */
  final case class Menu(title: String, items: Vector[String])

  /** State for the bar. */
  final case class State(
    menus: Vector[Menu],
    openIdx: Option[Int] = None,
    cursor: Int = 0
  ):
    /** The currently-open menu, if any. */
    def openMenu: Option[Menu] = openIdx.flatMap(menus.lift)

    /** True when no menu is open and `cursor` selects the menu title row. */
    def barFocused: Boolean = openIdx.isEmpty

    /** True when `idx` is the currently-open menu. */
    def isOpen(idx: Int): Boolean = openIdx.contains(idx)

  /**
   * Result of [[handleKey]]: the updated state plus an optional
   *  "the user picked this item" event for the app to act on.
   */
  final case class KeyResult(state: State, picked: Option[(Int, Int)])

  /**
   * Pure key dispatcher. Apps fold their decoded `KeyDecoder.InputKey`
   * through this to advance the bar's state.
   *
   * Bindings:
   *   - `←` / `→`        — move cursor between menu titles (or close
   *                        the open menu and move to the next/prev one)
   *   - `↑` / `↓`        — when a menu is open, move the item cursor;
   *                        when collapsed, opens the cursor menu (↓)
   *                        or the previous menu (↑) — symmetry with
   *                        most modal UIs.
   *   - `Enter` / Space  — open the cursor menu, or pick the cursor
   *                        item when a menu is open.
   *   - `Esc`            — close the open menu (or no-op when bar is
   *                        already collapsed).
   *   - any other key    — leave state alone.
   */
  def handleKey(state: State, key: KeyDecoder.InputKey): KeyResult =
    if state.menus.isEmpty then KeyResult(state, None)
    else
      import KeyDecoder.InputKey.*
      key match
        case ArrowLeft =>
          // Open menu: close + move cursor left + reopen for symmetry.
          val nextIdx = (state.cursor - 1 + state.menus.size) % state.menus.size
          val nextOpen =
            if state.openIdx.isDefined then Some(nextIdx) else None
          KeyResult(state.copy(cursor = nextIdx, openIdx = nextOpen), None)

        case ArrowRight =>
          val nextIdx = (state.cursor + 1) % state.menus.size
          val nextOpen =
            if state.openIdx.isDefined then Some(nextIdx) else None
          KeyResult(state.copy(cursor = nextIdx, openIdx = nextOpen), None)

        case ArrowDown =>
          state.openMenu match
            case Some(menu) if menu.items.nonEmpty =>
              val nextItem = (state.cursor + 1) % menu.items.size
              KeyResult(state.copy(cursor = nextItem), None)
            case _ =>
              // Collapsed: open the menu under cursor at item 0.
              KeyResult(state.copy(openIdx = Some(state.cursor), cursor = 0), None)

        case ArrowUp =>
          state.openMenu match
            case Some(menu) if menu.items.nonEmpty =>
              val nextItem = (state.cursor - 1 + menu.items.size) % menu.items.size
              KeyResult(state.copy(cursor = nextItem), None)
            case _ =>
              KeyResult(state.copy(openIdx = Some(state.cursor), cursor = 0), None)

        case Enter | CharKey(' ') =>
          state.openMenu match
            case Some(menu) if menu.items.nonEmpty =>
              KeyResult(
                state.copy(openIdx = None, cursor = state.openIdx.getOrElse(0)),
                Some((state.openIdx.get, state.cursor))
              )
            case _ =>
              KeyResult(state.copy(openIdx = Some(state.cursor), cursor = 0), None)

        case Escape =>
          if state.openIdx.isDefined then
            KeyResult(state.copy(openIdx = None, cursor = state.openIdx.getOrElse(0)), None)
          else KeyResult(state, None)

        case _ => KeyResult(state, None)

  /**
   * Render the bar (and the open dropdown, if any).
   *
   * @param state   The bar state.
   * @param at      Top-left of the menu-title row.
   * @param focused Whether the bar itself owns focus (controls colour
   *                of the cursor menu when no dropdown is open).
   */
  def apply(
    state: State,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    focused: Boolean = true
  )(using theme: Theme): List[VNode] =
    val titleSegments: List[Text] =
      state.menus.zipWithIndex.toList.flatMap { case (menu, i) =>
        val isCursor  = focused && state.openIdx.isEmpty && i == state.cursor
        val isOpen    = state.isOpen(i)
        val highlight = isCursor || isOpen
        val fg        = if highlight then theme.primary else theme.foreground
        val bold      = highlight
        val opener    = if isOpen then "[ " else "  "
        val closer    = if isOpen then " ]" else "  "
        val cell      = Text(s"$opener${menu.title}$closer", Style(fg = fg, bold = bold))
        if i == state.menus.size - 1 then List(cell)
        else List(cell, Text(" ", Style(fg = theme.border)))
      }

    val barNode = TextNode(at.x, at.y, titleSegments)

    state.openIdx match
      case None => List(barNode)
      case Some(idx) =>
        val menu = state.menus(idx)
        val left = titleStartCol(state, idx)
        val itemNodes = menu.items.zipWithIndex.toList.map { case (item, j) =>
          val sel = j == state.cursor
          val style =
            if sel then Style(fg = theme.background, bg = theme.primary, bold = true)
            else Style(fg = theme.foreground)
          val padded = fitToCells(item, dropdownInnerWidth(menu))
          TextNode(at.x + (left - 1), at.y + (1 + j), List(Text(padded, style)))
        }
        barNode :: itemNodes

  /**
   * Map a click `(col, row)` to a hit on either the menu titles or the
   * open dropdown. Returns:
   *
   *   - `Some(Left(menuIdx))`      — click on a menu title cell.
   *   - `Some(Right(itemIdx))`     — click on a row of the open dropdown.
   *   - `None`                     — click outside.
   */
  def hitTest(
    state: State,
    at: Coord,
    col: Int,
    row: Int
  ): Option[Either[Int, Int]] =
    if row == at.y.value then
      // Title row.
      hitTestTitles(state, at.x.value, col).map(Left.apply)
    else
      state.openIdx match
        case Some(idx) =>
          val menu    = state.menus(idx)
          val left    = at.x.value + titleStartCol(state, idx) - 1
          val width   = dropdownInnerWidth(menu)
          val itemRow = row - (at.y.value + 1)
          if itemRow < 0 || itemRow >= menu.items.size then None
          else if col < left || col >= left + width then None
          else Some(Right(itemRow))
        case None => None

  /**
   * Inner cell width of `menu`'s dropdown: the widest item label (measured in
   * display cells, not UTF-16 length, so CJK/emoji labels align) plus a
   * two-cell padding. `maxOption` guards the empty-items case — an empty
   * menu can still be opened via Enter/Arrow, and `Vector.empty.max` would
   * otherwise throw `UnsupportedOperationException` from both [[apply]] and
   * [[hitTest]].
   */
  private def dropdownInnerWidth(menu: Menu): Int =
    menu.items.map(WCWidth.stringWidth).maxOption.getOrElse(0) + 2

  /**
   * Column offset (1-based, relative to `at.x`) where the title cell
   *  for `menuIdx` begins. Mirrors the rendering layout. Title width is
   *  measured in display cells so a wide-char title doesn't mis-position
   *  every later dropdown.
   */
  private def titleStartCol(state: State, menuIdx: Int): Int =
    var col = 1
    var i   = 0
    while i < menuIdx do
      val cellWidth = WCWidth.stringWidth(state.menus(i).title) + 4 + 1 // "[ X ]" plus separator
      col += cellWidth
      i += 1
    col

  /** Hit-test for the title row. Title width is measured in display cells. */
  private def hitTestTitles(state: State, baseCol: Int, clickCol: Int): Option[Int] =
    var col = baseCol
    var i   = 0
    while i < state.menus.size do
      val cellWidth = WCWidth.stringWidth(state.menus(i).title) + 4
      if clickCol >= col && clickCol < col + cellWidth then return Some(i)
      col += cellWidth + 1 // + separator
      i += 1
    None

  /** Longest prefix of `s` whose display width is at most `maxCells`. */
  private def clipToCells(s: String, maxCells: Int): String =
    if maxCells <= 0 then ""
    else
      val sb = new StringBuilder
      var w  = 0
      var i  = 0
      while i < s.length do
        val cp = s.codePointAt(i)
        val cw = math.max(0, WCWidth.codePointWidth(cp))
        if w + cw > maxCells then return sb.toString
        sb.append(s.substring(i, i + java.lang.Character.charCount(cp)))
        w += cw
        i += java.lang.Character.charCount(cp)
      sb.toString

  /** Clip `s` to `cells` display cells, then right-pad with spaces to fill. */
  private def fitToCells(s: String, cells: Int): String =
    val clipped = clipToCells(s, cells)
    val w       = WCWidth.stringWidth(clipped)
    if w >= cells then clipped else clipped + " " * (cells - w)
