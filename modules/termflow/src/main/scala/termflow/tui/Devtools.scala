package termflow.tui

/**
 * In-memory recording primitives for time-travel debugging of TermFlow apps.
 *
 * The [[TuiApp]] contract is already pure with respect to `model` and `msg`,
 * which is exactly the property Elm's debugger exploits. This object provides
 * the data structures an app (or a future devtools wrapper) needs to capture
 * a sequence of `(msg, model)` transitions and inspect or rewind them.
 *
 * ## Phase 1 (this object)
 *
 *   - [[Devtools.History]] — bounded immutable ring buffer of [[Devtools.Frame]]s
 *   - [[Devtools.Frame]] — one transition: timestamp, optional `Msg`, resulting `Model`
 *   - Apps record manually in `update`; nothing in the runtime changes
 *
 * ## Phase 2 (partial — see [[Devtools.Panel]])
 *
 * [[Devtools.Panel]] ships a read-only viewer: takes a `History` plus a
 * cursor index and renders a scrollable list of recent frames. Apps
 * integrate by holding a cursor `Int` in their model and forwarding
 * arrow keys / Enter to `Panel.handleKey`. Enter returns the selected
 * frame's index so the app can rewind via [[Devtools.History.rewindTo]].
 *
 * Still deferred (Phase 2b): a `Devtools.wrap` app decorator that
 * records transitions and intercepts keys automatically. Prerequisite
 * is `llm4s/termflow#110` — making `Sub.InputKey` lazy-start (same
 * treatment as `Sub.Every` got in PR #95) so the wrapper can swap
 * the inner app's input source for its own without racing for keys.
 *
 * ## Manual integration today
 *
 * {{{
 * case class Model(
 *   count: Int,
 *   history: Devtools.History[Msg, Model]
 * )
 *
 * def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
 *   val initial = Model(0, Devtools.History.empty(capacity = 200))
 *   initial.copy(history = initial.history.snapshot(initial)).tui
 *
 * def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
 *   val nextModel = msg match
 *     case Msg.Inc => m.copy(count = m.count + 1)
 *     case Msg.Dec => m.copy(count = m.count - 1)
 *   nextModel.copy(history = nextModel.history.record(msg, nextModel)).tui
 *
 * // Later, dump the recording to a file or surface it in a debug overlay:
 * println(model.history.toReport)
 * }}}
 */
object Devtools:

  /** Wall-clock timestamp from `System.currentTimeMillis`. */
  def now(): Long = System.currentTimeMillis()

  /**
   * One entry in a [[History]] — a snapshot of the model after a transition,
   * with the message that caused it.
   *
   * @param index           Monotonically increasing position in the recording. Survives ring-buffer wrap-around.
   * @param timestampMillis Wall-clock time the frame was captured.
   * @param msg             The dispatched message, or `None` for an initial `snapshot`.
   * @param model           The model after the transition (or the initial state).
   */
  final case class Frame[+Msg, +Model](
    index: Int,
    timestampMillis: Long,
    msg: Option[Msg],
    model: Model
  )

  /**
   * Bounded immutable ring buffer of [[Frame]]s.
   *
   * `capacity` is the maximum number of frames retained; once full, recording
   * a new frame discards the oldest. The `index` on each frame is unique
   * across the lifetime of the history (it does **not** reset on wrap), so
   * callers can refer to a specific frame even after older entries have
   * been evicted — `at(index)` returns `None` once a frame has fallen off
   * the back.
   *
   * Every transition method returns a new `History` — the receiver is never
   * mutated.
   *
   * @param capacity  Maximum number of retained frames. Must be positive.
   * @param frames    Frames in chronological order, oldest first.
   * @param nextIndex The index that will be assigned to the next recorded frame.
   */
  final case class History[+Msg, +Model] private[tui] (
    capacity: Int,
    frames: Vector[Frame[Msg, Model]],
    nextIndex: Int
  ):
    require(capacity > 0, s"History capacity must be > 0 (got $capacity)")

    /** Number of frames currently retained. */
    def size: Int = frames.size

    /** `true` when no frames have been recorded yet. */
    def isEmpty: Boolean = frames.isEmpty

    /** `true` when the buffer is at capacity; the next record will evict the oldest. */
    def isFull: Boolean = frames.size == capacity

    /** Most recent frame, if any. */
    def latest: Option[Frame[Msg, Model]] = frames.lastOption

    /** Oldest retained frame, if any. */
    def oldest: Option[Frame[Msg, Model]] = frames.headOption

    /** Look up a frame by its (lifetime-unique) index. Returns `None` if evicted. */
    def at(index: Int): Option[Frame[Msg, Model]] =
      frames.find(_.index == index)

    /**
     * Record an initial-state snapshot (no causing message).
     *
     * Conventionally used once after `init` to capture the starting model.
     * Multiple snapshots are allowed if the app wants to mark waypoints,
     * but the more idiomatic way to log them is `record(msg, model)`.
     */
    def snapshot[M2 >: Model](model: M2, atMillis: Long = now()): History[Msg, M2] =
      append(Frame(nextIndex, atMillis, None, model))

    /** Record a `(msg → model)` transition. */
    def record[Ms2 >: Msg, M2 >: Model](
      msg: Ms2,
      model: M2,
      atMillis: Long = now()
    ): History[Ms2, M2] =
      append(Frame(nextIndex, atMillis, Some(msg), model))

    private def append[Ms2 >: Msg, M2 >: Model](f: Frame[Ms2, M2]): History[Ms2, M2] =
      val nextFrames = if isFull then frames.tail :+ f else frames :+ f
      copy(frames = nextFrames, nextIndex = nextIndex + 1)

    /**
     * Truncate history at the given frame index, preserving frames up to and
     * including `index` and dropping everything after.
     *
     * After a successful rewind, `nextIndex` is set to `index + 1` so that
     * subsequently recorded frames continue with fresh, monotonically
     * increasing indices. Returns `None` if the requested index isn't in the
     * retained window (either never recorded or already evicted).
     */
    def rewindTo(index: Int): Option[History[Msg, Model]] =
      val keepUpto = frames.indexWhere(_.index == index)
      if keepUpto < 0 then None
      else Some(copy(frames = frames.take(keepUpto + 1), nextIndex = index + 1))

    /**
     * Drop every retained frame but keep `capacity` and `nextIndex`.
     *
     * Useful between independent scenarios in long-lived sessions; the
     * preserved `nextIndex` means later frames don't collide with previously
     * exported references.
     */
    def cleared: History[Msg, Model] = copy(frames = Vector.empty)

    /**
     * Render a human-readable text dump of the recording.
     *
     * Format is one line per frame:
     * {{{
     *   #  4 | t=+ 120ms | Increment       | Model(count=3)
     * }}}
     * Timestamps are reported relative to the first retained frame for
     * readability. Useful for printing on shutdown, logging, or pasting
     * into a bug report.
     */
    def toReport: String =
      if frames.isEmpty then "(no recorded frames)"
      else
        val base = frames.head.timestampMillis
        val sb   = new StringBuilder
        sb.append(s"History — ${frames.size}/$capacity frames retained\n")
        frames.foreach { f =>
          val rel = f.timestampMillis - base
          val msg = f.msg.fold("(initial)")(_.toString)
          sb.append(
            f"  #${f.index}%4d | t=+${rel}%5dms | ${truncate(msg, 32)}%-32s | ${truncate(f.model.toString, 60)}\n"
          )
        }
        sb.toString

    private def truncate(s: String, maxLen: Int): String =
      if s.length <= maxLen then s
      else s.take(math.max(0, maxLen - 1)) + "…"

  object History:

    /** Build an empty history with the given retention capacity. */
    def empty[Msg, Model](capacity: Int): History[Msg, Model] =
      History(capacity, Vector.empty, 0)

  /**
   * Widget for viewing and navigating a [[Devtools.History]].
   *
   * Panel is a thin read-only projection over a History: the caller
   * holds a cursor index in their model, forwards navigation keys
   * via [[Panel.handleKey]], and renders the panel via [[Panel.view]].
   * On `Enter`, `handleKey` returns the selected frame's
   * lifetime-unique index so the app can call
   * [[Devtools.History.rewindTo]] to time-travel.
   *
   * ## Integration pattern
   *
   * {{{
   * case class Model(
   *   count: Int,
   *   history: Devtools.History[Msg, Model],
   *   devCursor: Int,
   *   devFocused: Boolean
   * )
   *
   * // In update:
   * case Msg.Inc =>
   *   val next = model.copy(count = model.count + 1)
   *   next.copy(history = next.history.record(msg, next)).tui
   *
   * case Msg.DevKey(k) =>
   *   val (cursor, maybeIdx) = Devtools.Panel.handleKey(model.devCursor, model.history, k)
   *   maybeIdx match
   *     case None      => model.copy(devCursor = cursor).tui
   *     case Some(idx) =>
   *       // Rewind: restore the selected frame's model.
   *       val frame = model.history.at(idx).get
   *       frame.model.copy(history = model.history.rewindTo(idx).get,
   *                        devCursor = cursor).tui
   *
   * // In view:
   * Devtools.Panel.view(
   *   history  = model.history,
   *   cursor   = model.devCursor,
   *   width    = 60,
   *   visibleRows = 10,
   *   focused  = model.devFocused
   * )
   * }}}
   *
   * The Panel itself doesn't own state — the caller parks the cursor
   * `Int` wherever makes sense (directly in the model, inside a wrapper
   * struct, etc). Stateless rendering keeps composition simple.
   *
   * ## Row format
   *
   * Each frame is rendered as:
   * {{{
   * #  4 · +120ms · Msg.Inc
   * }}}
   *
   * The model snapshot is deliberately elided (models can be large);
   * callers who want full details can print `history.toReport` on
   * the side or use `history.at(cursor)` to drill in.
   */
  object Panel:

    /**
     * Process one keystroke. Returns the next cursor index and, if the
     * key was `Enter` (or `Space`) on a valid frame, the selected
     * frame's lifetime-unique `index` for the caller to rewind to.
     *
     * The cursor is a zero-based position into `history.frames` — not
     * the frame's own `index`. That's so it keeps pointing at a row
     * as frames scroll off the back of the ring buffer.
     *
     * Behaviour:
     *   - `ArrowUp` / `ArrowDown` move the cursor within the retained range
     *   - `Home` / `End`          jump to first / last retained frame
     *   - `Enter` / `Space`       return the selected frame's `Frame.index`
     *   - other keys              no change
     *
     * Empty-history operation is a no-op — cursor stays where it is and
     * Enter returns `None`.
     */
    def handleKey[Msg, Model](
      cursor: Int,
      history: History[Msg, Model],
      key: KeyDecoder.InputKey
    ): (Int, Option[Int]) =
      import KeyDecoder.InputKey.*
      val size = history.size
      if size == 0 then (cursor, None)
      else
        val clamped = math.max(0, math.min(size - 1, cursor))
        key match
          case ArrowUp   => (math.max(0, clamped - 1), None)
          case ArrowDown => (math.min(size - 1, clamped + 1), None)
          case Home      => (0, None)
          case End       => (size - 1, None)
          case Enter | CharKey(' ') =>
            val frame = history.frames(clamped)
            (clamped, Some(frame.index))
          case _ => (clamped, None)

    /**
     * Render the panel as a [[BoxNode]] with a title row, a divider,
     * and `visibleRows` body rows drawn from the tail of `history`.
     *
     * Sizing mirrors [[termflow.tui.widgets.Table]]: total height is
     * `2 + visibleRows` cells (title + divider + viewport). Rows that
     * don't have a frame render as blanks so the panel has a stable
     * height regardless of how full the history is.
     *
     * Focus gates the cursor: only a focused panel shows the `▸ `
     * marker + inverse-video row.
     *
     * @param history     The recording to display.
     * @param cursor      Zero-based index into the visible frames list.
     * @param at          Top-left cell. Defaults to `(1, 1)`.
     * @param width       Total width in cells (minimum 10).
     * @param visibleRows Viewport height in cells.
     * @param focused     Whether the panel currently has focus.
     * @param title       Title-row text.
     */
    def view[Msg, Model](
      history: History[Msg, Model],
      cursor: Int,
      at: Coord = Coord(XCoord(1), YCoord(1)),
      width: Int = 60,
      visibleRows: Int = 10,
      focused: Boolean = false,
      title: String = "Devtools History"
    )(using theme: Theme): VNode =
      import termflow.tui.TuiPrelude.*
      val w       = math.max(10, width)
      val rowsH   = math.max(1, visibleRows)
      val frames  = history.frames
      val size    = frames.size
      val clamped = math.max(0, math.min(math.max(0, size - 1), cursor))

      val headerLabel = s"$title (${size}/${history.capacity})".take(w).padTo(w, ' ')
      val headerStyle = Style(fg = theme.primary, bold = true)
      val headerRow   = TextNode(at.x, at.y, List(Text(headerLabel, headerStyle)))
      val divider     = TextNode(at.x, at.y + 1, List(Text("─" * w, Style(fg = theme.primary))))

      // Scroll so the cursor stays in-view (simple window-follow logic).
      val scrollOffset =
        if size <= rowsH then 0
        else if clamped < rowsH then 0
        else math.min(size - rowsH, clamped - rowsH + 1)

      val baseTs = frames.headOption.map(_.timestampMillis).getOrElse(0L)

      val bodyRows = (0 until rowsH).map { r =>
        val idx  = scrollOffset + r
        val rowY = at.y + 2 + r
        if idx >= size then TextNode(at.x, rowY, List(Text(" " * w, Style())))
        else
          val f          = frames(idx)
          val rel        = f.timestampMillis - baseTs
          val msgText    = f.msg.fold("(initial)")(_.toString)
          val line       = f"#${f.index}%4d · +${rel}%5dms · ${msgText}"
          val truncated  = line.take(w - 2).padTo(w - 2, ' ')
          val isSelected = idx == clamped
          val showCursor = isSelected && focused
          val prefix     = if showCursor then "▸ " else "  "
          val style =
            if showCursor then Style(fg = theme.background, bg = theme.primary)
            else Style(fg = theme.foreground)
          TextNode(at.x, rowY, List(Text(prefix, style), Text(truncated, style)))
      }.toList

      BoxNode(
        at.x,
        at.y,
        w,
        2 + rowsH,
        children = headerRow :: divider :: bodyRows,
        style = Style()
      )

    /** Total rendered height in cells (title + divider + viewport). */
    def height(visibleRows: Int): Int = 2 + math.max(1, visibleRows)

    /** Minimum panel width. */
    def width(lineWidth: Int): Int = math.max(10, lineWidth)
