package termflow.tui.widgets

import termflow.tui.*
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.TuiPrelude.*

/**
 * Multi-line text editor widget.
 *
 * Maintains a `Vector[String]` of lines plus a `(row, col)` cursor.
 * Editing keys (`CharKey`, `Enter`, `Backspace`, `Delete`, arrow keys,
 * `Home`/`End`) compose to a small fixed-pitch text editor — enough
 * for a comments box, a description field, or the body of a chat
 * message. For single-line input use [[termflow.tui.Prompt]] instead.
 *
 * Cursor navigation is **grapheme-aware** within a line: arrow keys
 * step over surrogate pairs and combining marks as one unit, mirroring
 * the single-line `Prompt`. The cursor is stored in UTF-16 code-unit
 * offsets to match the underlying `String` model — apps composing with
 * other text APIs can pass it straight through.
 *
 * The widget renders one `TextNode` per visible row. The cursor row
 * uses a reverse-video cell so the caret is visible without having to
 * commandeer the hardware cursor — same approach as
 * [[termflow.tui.widgets.TextField]]. Lines too wide for the viewport
 * are clipped with a trailing `…`; vertical scrolling lifts the top
 * row off-screen when the cursor moves below the visible window.
 *
 * The widget is purely presentational. State lives in the calling app:
 *
 * {{{
 * given Theme = Theme.dark
 * MultiLineInput.render(
 *   state = m.editor,
 *   width = 60,
 *   height = 8,
 *   at = Coord(2.x, 5.y)
 * )
 * }}}
 *
 * Apps wire the keyboard with `MultiLineInput.handleKey(state, key)`
 * which returns `(newState, Option[Cmd[Msg]])` — the optional command
 * is only ever `None` today (no submit semantics are baked in), but
 * the shape matches `Prompt.handleKey` so swapping the two is a small
 * change.
 */
object MultiLineInput:

  /**
   * Editor state.
   *
   * Invariant: `lines` always contains at least one entry — an empty
   * editor is `lines = Vector("")` with cursor `(0, 0)`. The cursor
   * row is clamped to `[0, lines.size - 1]` and the column to
   * `[0, currentLine.length]` after every transition.
   *
   * @param lines      The editor's lines, top-to-bottom. UTF-16
   *                   code-unit strings, no trailing newline.
   * @param cursorRow  Zero-based row of the cursor.
   * @param cursorCol  Zero-based UTF-16 column on the cursor's row.
   * @param scrollTop  First visible row (vertical scroll offset).
   *                   Internal-state-ish; apps generally don't manage
   *                   it directly — [[handleKey]] keeps it consistent
   *                   with the cursor.
   */
  final case class State(
    lines: Vector[String] = Vector(""),
    cursorRow: Int = 0,
    cursorCol: Int = 0,
    scrollTop: Int = 0
  ):

    /** Convenience: the current row's text. */
    def currentLine: String = lines(math.max(0, math.min(lines.size - 1, cursorRow)))

    /** All lines joined with `\n`, no trailing newline. */
    def text: String = lines.mkString("\n")

  object State:

    /** Empty editor at `(0, 0)`. */
    val empty: State = State()

    /** Initial state seeded with `text`, cursor at the end. */
    def of(text: String): State =
      val lines   = if text.isEmpty then Vector("") else text.split("\n", -1).toVector
      val lastRow = lines.size - 1
      val lastCol = lines.last.length
      State(lines, cursorRow = lastRow, cursorCol = lastCol)

  /**
   * Feed a key event into the editor and return the updated state.
   *
   * Returns `(state, None)` for handled keys; `(state, None)` for
   * unhandled keys (no-op) — the `Option[Cmd]` shape is reserved for
   * future submit semantics so callers don't have to refactor when
   * those land.
   */
  def handleKey[Msg](state: State, key: InputKey): (State, Option[Cmd[Msg]]) =
    val s = clamp(state)
    key match
      case InputKey.CharKey(ch) =>
        val line   = s.currentLine
        val newCol = s.cursorCol + 1
        val newLn  = line.substring(0, s.cursorCol) + ch + line.substring(s.cursorCol)
        (clamp(s.copy(lines = s.lines.updated(s.cursorRow, newLn), cursorCol = newCol)), None)

      case InputKey.Supplementary(cp) =>
        val chars = new String(Character.toChars(cp))
        val line  = s.currentLine
        val newLn = line.substring(0, s.cursorCol) + chars + line.substring(s.cursorCol)
        (clamp(s.copy(lines = s.lines.updated(s.cursorRow, newLn), cursorCol = s.cursorCol + chars.length)), None)

      case InputKey.Enter =>
        val line     = s.currentLine
        val before   = line.substring(0, s.cursorCol)
        val after    = line.substring(s.cursorCol)
        val newLines = s.lines.patch(s.cursorRow, Vector(before, after), 1)
        (clamp(s.copy(lines = newLines, cursorRow = s.cursorRow + 1, cursorCol = 0)), None)

      case InputKey.Backspace =>
        if s.cursorCol > 0 then
          val line  = s.currentLine
          val prev  = Grapheme.previousBoundary(line, s.cursorCol)
          val newLn = line.substring(0, prev) + line.substring(s.cursorCol)
          (clamp(s.copy(lines = s.lines.updated(s.cursorRow, newLn), cursorCol = prev)), None)
        else if s.cursorRow > 0 then
          // Join with the previous line; cursor lands at the join point.
          val prevLine = s.lines(s.cursorRow - 1)
          val curLine  = s.currentLine
          val joined   = prevLine + curLine
          val newLines = s.lines.patch(s.cursorRow - 1, Vector(joined), 2)
          (clamp(s.copy(lines = newLines, cursorRow = s.cursorRow - 1, cursorCol = prevLine.length)), None)
        else (s, None)

      case InputKey.Delete =>
        val line = s.currentLine
        if s.cursorCol < line.length then
          val nxt   = Grapheme.nextBoundary(line, s.cursorCol)
          val newLn = line.substring(0, s.cursorCol) + line.substring(nxt)
          (clamp(s.copy(lines = s.lines.updated(s.cursorRow, newLn))), None)
        else if s.cursorRow < s.lines.size - 1 then
          val nextLine = s.lines(s.cursorRow + 1)
          val joined   = line + nextLine
          val newLines = s.lines.patch(s.cursorRow, Vector(joined), 2)
          (clamp(s.copy(lines = newLines)), None)
        else (s, None)

      case InputKey.ArrowLeft =>
        if s.cursorCol > 0 then
          val prev = Grapheme.previousBoundary(s.currentLine, s.cursorCol)
          (clamp(s.copy(cursorCol = prev)), None)
        else if s.cursorRow > 0 then
          val above = s.lines(s.cursorRow - 1)
          (clamp(s.copy(cursorRow = s.cursorRow - 1, cursorCol = above.length)), None)
        else (s, None)

      case InputKey.ArrowRight =>
        val line = s.currentLine
        if s.cursorCol < line.length then
          val nxt = Grapheme.nextBoundary(line, s.cursorCol)
          (clamp(s.copy(cursorCol = nxt)), None)
        else if s.cursorRow < s.lines.size - 1 then (clamp(s.copy(cursorRow = s.cursorRow + 1, cursorCol = 0)), None)
        else (s, None)

      case InputKey.ArrowUp =>
        if s.cursorRow > 0 then
          val above = s.lines(s.cursorRow - 1)
          (clamp(s.copy(cursorRow = s.cursorRow - 1, cursorCol = math.min(s.cursorCol, above.length))), None)
        else (s, None)

      case InputKey.ArrowDown =>
        if s.cursorRow < s.lines.size - 1 then
          val below = s.lines(s.cursorRow + 1)
          (clamp(s.copy(cursorRow = s.cursorRow + 1, cursorCol = math.min(s.cursorCol, below.length))), None)
        else (s, None)

      case InputKey.Home =>
        (clamp(s.copy(cursorCol = 0)), None)

      case InputKey.End =>
        (clamp(s.copy(cursorCol = s.currentLine.length)), None)

      case InputKey.Tab =>
        // Insert a literal `\t` so Tab in a text editor does what users
        // typing into one expect. Apps that want focus traversal across
        // multiple widgets should intercept Tab before feeding the key
        // into [[handleKey]].
        val line   = s.currentLine
        val newCol = s.cursorCol + 1
        val newLn  = line.substring(0, s.cursorCol) + '\t' + line.substring(s.cursorCol)
        (clamp(s.copy(lines = s.lines.updated(s.cursorRow, newLn), cursorCol = newCol)), None)

      case InputKey.Paste(text) =>
        if text.isEmpty then (s, None)
        else
          val pasteLines = text.split("\n", -1).toVector
          val curLine    = s.currentLine
          val before     = curLine.substring(0, s.cursorCol)
          val after      = curLine.substring(s.cursorCol)
          if pasteLines.size == 1 then
            // Single line: insert inline at cursor.
            val newLn  = before + pasteLines.head + after
            val newCol = s.cursorCol + pasteLines.head.length
            (clamp(s.copy(lines = s.lines.updated(s.cursorRow, newLn), cursorCol = newCol)), None)
          else
            // Multiple paste lines: split across rows.
            val firstLine = before + pasteLines.head
            val lastLine  = pasteLines.last + after
            val middle    = pasteLines.drop(1).dropRight(1)
            val newLines  = s.lines.patch(s.cursorRow, firstLine +: middle :+ lastLine, 1)
            val newRow    = s.cursorRow + pasteLines.size - 1
            val newCol    = pasteLines.last.length
            (clamp(s.copy(lines = newLines, cursorRow = newRow, cursorCol = newCol)), None)

      case _ => (s, None)

  /**
   * Feed a key event into the editor with submit semantics.
   *
   * When `key` matches `submitKey`, the full text (`state.text`) is
   * passed to `toMsg` and the resulting [[Cmd]] is returned. The editor
   * resets to [[State.empty]]. For all other keys, editing proceeds
   * identically to the base [[handleKey]].
   *
   * @param state     Current editor state.
   * @param key       The key event to process.
   * @param submitKey The key that triggers text submission.
   * @param toMsg     Callback to convert the submitted text into an
   *                  app message. Return `Left(error)` to surface a
   *                  [[termflow.tui.TermFlowError]] without submitting.
   */
  def handleKey[Msg](state: State, key: InputKey, submitKey: InputKey)(
    toMsg: String => Result[Msg]
  ): (State, Option[Cmd[Msg]]) =
    if key == submitKey then
      val text = state.text
      if text.trim.isEmpty then (State.empty, None)
      else
        val cmd = toMsg(text).fold(err => Cmd.TermFlowErrorCmd(err), g => Cmd.GCmd(g))
        (State.empty, Some(cmd))
    else
      val (s, _) = handleKey(state, key)
      (s, None)

  /**
   * Render the editor at `(at, width, height)`. Returns a list of
   * VNodes — one per visible row plus an [[InputNode]] for the cursor
   * row. Apps typically embed this in a `BoxNode` border for a nicer
   * visual; the widget doesn't draw the border itself.
   *
   * @param state  Current editor state.
   * @param width  Visible width in cells (rows are clipped past this).
   * @param height Visible height in rows.
   * @param at     Top-left of the viewport.
   */
  def render(
    state: State,
    width: Int,
    height: Int,
    at: Coord = Coord(XCoord(1), YCoord(1))
  )(using theme: Theme): List[VNode] =
    if width <= 0 || height <= 0 then return Nil
    val s         = clamp(scrollFor(state, height))
    val viewBegin = s.scrollTop
    val viewEnd   = math.min(s.lines.size, viewBegin + height)
    val style     = Style(fg = theme.foreground, bg = theme.background)

    val cursorStyle = Style(fg = theme.background, bg = theme.primary)
    val rows = (viewBegin until viewEnd).toList.map { rowIdx =>
      val viewRow  = rowIdx - viewBegin
      val rowAt    = Coord(at.x, at.y + viewRow)
      val isCursor = rowIdx == s.cursorRow
      val text     = s.lines(rowIdx)
      if isCursor then
        // Render the cursor row as a TextNode with a reverse-video cell
        // at the cursor column. Same pattern as TextField — the renderer
        // needs no help and the cursor stays visible inside any embedding.
        renderCursorRow(text, s.cursorCol, width, rowAt, style, cursorStyle)
      else
        val clipped =
          if WCWidth.stringWidth(text) <= width then text
          else clipToWidth(text, math.max(1, width - 1)) + "…"
        TextNode(rowAt.x, rowAt.y, List(Text(clipped, style)))
    }
    rows

  /** Adjust `scrollTop` so the cursor row is visible in `height` rows. */
  def scrollFor(state: State, height: Int): State =
    val s = clamp(state)
    if height <= 0 then s
    else if s.cursorRow < s.scrollTop then s.copy(scrollTop = s.cursorRow)
    else if s.cursorRow >= s.scrollTop + height then s.copy(scrollTop = s.cursorRow - height + 1)
    else s

  /** Clamp the cursor and scroll to valid ranges given `lines`. */
  private def clamp(state: State): State =
    val lines     = if state.lines.isEmpty then Vector("") else state.lines
    val maxRow    = lines.size - 1
    val row       = math.max(0, math.min(maxRow, state.cursorRow))
    val maxCol    = lines(row).length
    val col       = math.max(0, math.min(maxCol, state.cursorCol))
    val scrollTop = math.max(0, math.min(maxRow, state.scrollTop))
    state.copy(lines = lines, cursorRow = row, cursorCol = col, scrollTop = scrollTop)

  /**
   * Build the cursor row's TextNode with a reverse-video cell at the
   * cursor's UTF-16 position. Mirrors [[widgets.TextField]]'s caret
   * approach so embedded use stays straightforward.
   */
  private def renderCursorRow(
    text: String,
    cursorCol: Int,
    viewportWidth: Int,
    at: Coord,
    base: Style,
    cursorStyle: Style
  ): VNode =
    val width = math.max(1, viewportWidth)
    val cur   = math.max(0, math.min(text.length, cursorCol))

    // The cursor row, unlike the other rows, used to emit the entire line
    // verbatim — so a line longer than the viewport overflowed `width`
    // (spilling past any surrounding box border). Horizontally scroll the
    // row so it is always exactly `width` cells and the cursor stays visible.
    val cursorCharWidth =
      if cur < text.length then math.max(1, WCWidth.codePointWidth(text.codePointAt(cur))) else 1
    val cursorCell = WCWidth.stringWidth(text.substring(0, cur))
    // Pin the cursor to the right edge once the text to its left overflows,
    // reserving enough columns for the (possibly wide) cursor glyph.
    val startCell = math.max(0, cursorCell - math.max(0, width - cursorCharWidth))
    val startIdx  = charIndexAtCell(text, startCell)

    val before = clipToWidth(text.substring(startIdx, cur), width)
    val rest   = text.substring(cur)
    val (cursorChar, afterRaw) =
      if rest.isEmpty then (" ", "")
      else
        val n = java.lang.Character.charCount(rest.codePointAt(0))
        (rest.substring(0, n), rest.substring(n))

    val usedCells = WCWidth.stringWidth(before) + WCWidth.stringWidth(cursorChar)
    val after     = clipToWidth(afterRaw, math.max(0, width - usedCells))
    // Pad out to the viewport so the reverse-video cell stays visible even
    // when the (clipped) content is shorter than the row.
    val pad    = math.max(0, width - usedCells - WCWidth.stringWidth(after))
    val padded = after + (" " * pad)
    TextNode(
      at.x,
      at.y,
      List(
        Text(before, base),
        Text(cursorChar, cursorStyle),
        Text(padded, base)
      )
    )

  /**
   * Truncate `text` so that the rendered cell width is at most
   * `maxWidth`. Used by the renderer for non-cursor rows that are too
   * wide for the viewport.
   */
  private def clipToWidth(text: String, maxWidth: Int): String =
    if maxWidth <= 0 then ""
    else
      val sb = new StringBuilder
      var w  = 0
      var i  = 0
      while i < text.length do
        val cp  = text.codePointAt(i)
        val cw  = WCWidth.codePointWidth(cp)
        val n   = java.lang.Character.charCount(cp)
        val add = math.max(0, cw)
        if w + add > maxWidth then return sb.toString
        sb.append(text.substring(i, i + n))
        w += add
        i += n
      sb.toString

  /**
   * Smallest UTF-16 index `i` such that `WCWidth.stringWidth(text.take(i))`
   * is at least `targetCells`. Translates a desired horizontal scroll column
   * back to a code-point boundary (never splitting a wide glyph or surrogate
   * pair). For pure-ASCII input this returns `targetCells`.
   */
  private def charIndexAtCell(text: String, targetCells: Int): Int =
    if targetCells <= 0 then 0
    else
      var w = 0
      var i = 0
      while i < text.length && w < targetCells do
        val cp = text.codePointAt(i)
        w += math.max(0, WCWidth.codePointWidth(cp))
        i += java.lang.Character.charCount(cp)
      i
