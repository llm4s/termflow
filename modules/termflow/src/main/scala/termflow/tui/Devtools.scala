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
 * ## Phase 2 (deferred follow-up)
 *
 * A `TuiApp` wrapper that:
 *   - records every transition automatically
 *   - exposes a hotkey-toggled overlay (history list + rewind controls)
 *   - intercepts replay messages and re-applies forward transitions
 *
 * Phase 2 needs more design work around overlay rendering and key
 * interception — see the follow-up issue filed alongside this PR.
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
