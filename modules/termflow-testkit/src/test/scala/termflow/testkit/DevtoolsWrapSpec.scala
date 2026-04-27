package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.TuiTestDriver
import termflow.tui.Devtools.*
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.Tui.tui
import termflow.tui.TuiPrelude.*

class DevtoolsWrapSpec extends AnyFunSuite:

  given Theme = Theme.dark

  // A minimal inner app to wrap.
  enum InnerMsg:
    case Inc, Dec, Reset
    case Key(k: InputKey)

  private def innerApp: TuiApp[Int, InnerMsg] = new TuiApp[Int, InnerMsg]:
    override def init(ctx: RuntimeCtx[InnerMsg]): Tui[Int, InnerMsg] =
      // Inner registers a Sub.InputKey — the wrapper should capture + not start it.
      Sub.InputKey[InnerMsg](InnerMsg.Key.apply, _ => InnerMsg.Reset, ctx)
      0.tui

    override def update(model: Int, msg: InnerMsg, ctx: RuntimeCtx[InnerMsg]): Tui[Int, InnerMsg] =
      msg match
        case InnerMsg.Inc    => (model + 1).tui
        case InnerMsg.Dec    => (model - 1).tui
        case InnerMsg.Reset  => 0.tui
        case InnerMsg.Key(_) => model.tui // ignore raw keys for this test

    override def view(model: Int): RootNode =
      RootNode(
        width = 20,
        height = 1,
        children = List(TextNode(1.x, 1.y, List(Text(s"count=$model", Style())))),
        input = None
      )

    override def toMsg(input: PromptLine): Result[InnerMsg] = Right(InnerMsg.Inc)

  private def driver(): TuiTestDriver[Wrapped[Int, InnerMsg], WrapMsg[InnerMsg]] =
    val wrapped = Devtools.wrap(innerApp, toInputMsg = InnerMsg.Key.apply, capacity = 20)
    val d       = TuiTestDriver(wrapped, width = 40, height = 12)
    d.init()
    d

  // --- initial state -------------------------------------------------------

  test("init starts in Live mode with a single initial snapshot"):
    val d = driver()
    assert(d.model.mode == WrapMode.Live)
    assert(d.model.inner == 0)
    assert(d.model.history.size == 1)
    assert(d.model.history.frames.head.msg.isEmpty, "initial frame has msg=None")
    assert(d.model.history.frames.head.model == 0)

  // --- recording in Live mode ---------------------------------------------

  test("inner transitions in Live mode are recorded into history"):
    val d = driver()
    d.send(WrapMsg.Inner(InnerMsg.Inc))
    d.send(WrapMsg.Inner(InnerMsg.Inc))
    d.send(WrapMsg.Inner(InnerMsg.Dec))
    assert(d.model.inner == 1)
    assert(d.model.history.size == 4, "1 snapshot + 3 recorded transitions")
    assert(d.model.history.latest.get.model == 1)
    assert(d.model.history.latest.get.msg.contains(InnerMsg.Dec))

  test("inner transitions while in Inspect mode are ignored"):
    val d = driver()
    d.send(WrapMsg.Toggle)
    assert(d.model.mode == WrapMode.Inspect)
    d.send(WrapMsg.Inner(InnerMsg.Inc))
    assert(d.model.inner == 0, "inner update should be paused")
    // History size unchanged (still just the initial snapshot).
    assert(d.model.history.size == 1)

  // --- Key routing --------------------------------------------------------

  test("Live mode forwards Key messages to inner via toInputMsg"):
    val d = driver()
    d.send(WrapMsg.Key(InputKey.CharKey('x')))
    // The wrapped Key turns into InnerMsg.Key('x'), which inner.update
    // handles by ignoring — but the transition was recorded.
    assert(d.model.history.size == 2)
    val last = d.model.history.latest.get
    assert(last.msg.contains(InnerMsg.Key(InputKey.CharKey('x'))))

  test("Toggle hotkey Ctrl+G flips Live <-> Inspect"):
    val d = driver()
    d.send(WrapMsg.Key(InputKey.Ctrl('G')))
    assert(d.model.mode == WrapMode.Inspect)
    d.send(WrapMsg.Key(InputKey.Ctrl('G')))
    assert(d.model.mode == WrapMode.Live)

  test("Entering Inspect parks the cursor on the most recent frame"):
    val d = driver()
    (1 to 3).foreach(_ => d.send(WrapMsg.Inner(InnerMsg.Inc)))
    d.send(WrapMsg.Toggle)
    assert(d.model.mode == WrapMode.Inspect)
    assert(d.model.cursor == 3, s"history size is 4, cursor should park at 3; saw ${d.model.cursor}")

  // --- Inspect mode navigation --------------------------------------------

  test("ArrowUp in Inspect moves cursor backward"):
    val d = driver()
    (1 to 3).foreach(_ => d.send(WrapMsg.Inner(InnerMsg.Inc)))
    d.send(WrapMsg.Toggle) // cursor parks at 3
    d.send(WrapMsg.Key(InputKey.ArrowUp))
    assert(d.model.cursor == 2)

  test("Enter in Inspect rewinds inner model to the selected frame's model"):
    val d = driver()
    d.send(WrapMsg.Inner(InnerMsg.Inc)) // 1
    d.send(WrapMsg.Inner(InnerMsg.Inc)) // 2
    d.send(WrapMsg.Inner(InnerMsg.Inc)) // 3
    assert(d.model.inner == 3)
    d.send(WrapMsg.Toggle)
    // Move cursor back to frame at index 1 (the Inc that produced model=1).
    d.send(WrapMsg.Key(InputKey.ArrowUp)) // cursor=2
    d.send(WrapMsg.Key(InputKey.ArrowUp)) // cursor=1
    d.send(WrapMsg.Key(InputKey.Enter))
    assert(d.model.mode == WrapMode.Live, "rewind returns to Live")
    assert(d.model.inner == 1, "inner model restored from the selected frame")
    assert(d.model.history.size == 2, "history truncated to index 1 (size 2)")
    assert(d.model.history.latest.get.model == 1)

  test("Post-rewind transitions continue with fresh history indices"):
    val d = driver()
    d.send(WrapMsg.Inner(InnerMsg.Inc)) // 1
    d.send(WrapMsg.Inner(InnerMsg.Inc)) // 2
    d.send(WrapMsg.Toggle)
    d.send(WrapMsg.Key(InputKey.ArrowUp)) // cursor=1
    d.send(WrapMsg.Key(InputKey.Enter))   // rewind to 1
    assert(d.model.inner == 1)
    // A new transition after rewind should be recorded with nextIndex = 2.
    d.send(WrapMsg.Inner(InnerMsg.Dec)) // 0
    assert(d.model.history.latest.get.index == 2)
    assert(d.model.history.latest.get.model == 0)

  // --- view pass-through / takeover ---------------------------------------

  test("Live mode view is identical to inner.view"):
    val d = driver()
    d.send(WrapMsg.Inner(InnerMsg.Inc))
    val frame      = d.frame
    val innerRoot  = innerApp.view(1)
    val innerFrame = AnsiRenderer.buildFrame(innerRoot)
    assert(frame.width == innerFrame.width)
    assert(frame.height == innerFrame.height)
    val row0Frame = (0 until frame.width).map(c => frame.cells(0)(c).ch).mkString
    val row0Inner = (0 until innerFrame.width).map(c => innerFrame.cells(0)(c).ch).mkString
    assert(row0Frame == row0Inner)

  test("Inspect mode view shows the Devtools Panel instead of the inner view"):
    val d = driver()
    d.send(WrapMsg.Inner(InnerMsg.Inc))
    d.send(WrapMsg.Toggle)
    val frame = d.frame
    // Top row carries the Devtools title line.
    val row0 = (0 until frame.width).map(c => frame.cells(0)(c).ch).mkString
    assert(row0.contains("Devtools"))
    assert(row0.contains("Enter rewind"))
    assert(row0.contains("Ctrl+G"))
    // The count=N string from inner.view should NOT appear.
    val allChars = (0 until frame.height)
      .flatMap(r => (0 until frame.width).map(c => frame.cells(r)(c).ch))
      .mkString
    assert(!allChars.contains("count="), "inner view must be hidden in Inspect mode")

  // --- startup cmd propagation --------------------------------------------

  test("inner startup Cmd propagates through the wrapper"):
    // Use an inner app whose init returns Cmd.Exit — the wrapper should
    // propagate that as the outer startup cmd.
    val exitingInner: TuiApp[Int, InnerMsg] = new TuiApp[Int, InnerMsg]:
      override def init(ctx: RuntimeCtx[InnerMsg]): Tui[Int, InnerMsg]                          = Tui(0, Cmd.Exit)
      override def update(m: Int, msg: InnerMsg, ctx: RuntimeCtx[InnerMsg]): Tui[Int, InnerMsg] = m.tui
      override def view(m: Int): RootNode                     = RootNode(1, 1, List.empty, None)
      override def toMsg(input: PromptLine): Result[InnerMsg] = Right(InnerMsg.Inc)
    val wrapped = Devtools.wrap(exitingInner, toInputMsg = InnerMsg.Key.apply)
    val d       = TuiTestDriver(wrapped, width = 20, height = 4)
    d.init()
    assert(d.exited, "Cmd.Exit from inner init should terminate the wrapped app")

  // --- capacity ------------------------------------------------------------

  test("history capacity is honoured — older frames are evicted"):
    val wrapped = Devtools.wrap(innerApp, toInputMsg = InnerMsg.Key.apply, capacity = 3)
    val d       = TuiTestDriver(wrapped, width = 20, height = 4)
    d.init()
    // Initial snapshot + 5 Inc transitions = 6 frames, retained = 3.
    (1 to 5).foreach(_ => d.send(WrapMsg.Inner(InnerMsg.Inc)))
    assert(d.model.history.size == 3)
    // Oldest frames were evicted; at(0) / at(1) / at(2) now None.
    assert(d.model.history.at(0).isEmpty)
    assert(d.model.history.at(2).isEmpty)
    // The latest retained frames span 3..5 inclusive.
    assert(d.model.history.frames.map(_.index) == Vector(3, 4, 5))

  // --- inner sub forwarding -----------------------------------------------

  /** Test-only sub whose start/cancel flip flags and whose `fire` publishes on demand. */
  private class RecordingSub[Msg](sink: EventSink[Msg], onTick: () => Msg) extends Sub[Msg]:
    var started: Boolean           = false
    var cancelled: Boolean         = false
    override def isActive: Boolean = started && !cancelled
    override def cancel(): Unit    = cancelled = true
    override def start(): Unit     = started = true
    def fire(): Unit               = sink.publish(Cmd.GCmd(onTick()))

  /** Test-only input sub whose `start` flips a flag. */
  private class RecordingInputSub[Msg] extends Sub.InputSub[Msg]:
    var started: Boolean           = false
    override def isActive: Boolean = false
    override def cancel(): Unit    = ()
    override def start(): Unit     = started = true

  private def timerInner(subRef: RecordingSub[InnerMsg] => Unit): TuiApp[Int, InnerMsg] =
    new TuiApp[Int, InnerMsg]:
      override def init(ctx: RuntimeCtx[InnerMsg]): Tui[Int, InnerMsg] =
        val s = new RecordingSub[InnerMsg](ctx, () => InnerMsg.Inc)
        ctx.registerSub(s)
        subRef(s)
        0.tui
      override def update(m: Int, msg: InnerMsg, ctx: RuntimeCtx[InnerMsg]): Tui[Int, InnerMsg] =
        msg match
          case InnerMsg.Inc    => (m + 1).tui
          case InnerMsg.Dec    => (m - 1).tui
          case InnerMsg.Reset  => 0.tui
          case InnerMsg.Key(_) => m.tui
      override def view(m: Int): RootNode                     = RootNode(10, 1, List.empty, None)
      override def toMsg(input: PromptLine): Result[InnerMsg] = Right(InnerMsg.Inc)

  test("wrapper starts inner non-input subs (e.g. Sub.Every) during init"):
    var captured: RecordingSub[InnerMsg] = null
    val wrapped                          = Devtools.wrap(timerInner(s => captured = s), toInputMsg = InnerMsg.Key.apply)
    val d                                = TuiTestDriver(wrapped, width = 20, height = 4)
    d.init()
    assert(captured != null)
    assert(captured.started, "wrapper should call start() on inner non-input subs")
    assert(!captured.cancelled)

  test("inner non-input sub ticks flow through as WrapMsg.Inner while Live"):
    var captured: RecordingSub[InnerMsg] = null
    val wrapped                          = Devtools.wrap(timerInner(s => captured = s), toInputMsg = InnerMsg.Key.apply)
    val d                                = TuiTestDriver(wrapped, width = 20, height = 4)
    d.init()
    assert(d.model.inner == 0)
    captured.fire() // queues Cmd.GCmd(WrapMsg.Inner(Inc)) on the real ctx
    // Force a drain via a no-op message (KeyError is ignored by update).
    d.send(WrapMsg.KeyError(new RuntimeException("drain")))
    assert(d.model.inner == 1, "timer tick should have advanced the inner model")
    // And it was recorded into history just like any other transition.
    assert(d.model.history.size == 2)
    assert(d.model.history.latest.get.msg.contains(InnerMsg.Inc))

  test("inner non-input sub ticks are discarded while in Inspect mode"):
    var captured: RecordingSub[InnerMsg] = null
    val wrapped                          = Devtools.wrap(timerInner(s => captured = s), toInputMsg = InnerMsg.Key.apply)
    val d                                = TuiTestDriver(wrapped, width = 20, height = 4)
    d.init()
    d.send(WrapMsg.Inner(InnerMsg.Inc)) // model=1
    d.send(WrapMsg.Toggle)              // enter Inspect
    captured.fire()                     // queues an Inc — but mode is Inspect
    d.send(WrapMsg.KeyError(new RuntimeException("drain")))
    assert(d.model.inner == 1, "ticks during Inspect must not advance the model")
    assert(d.model.history.size == 2, "no new history frame while Inspect paused")

  test("real ctx cancellation propagates to the inner non-input sub"):
    var captured: RecordingSub[InnerMsg] = null
    val wrapped                          = Devtools.wrap(timerInner(s => captured = s), toInputMsg = InnerMsg.Key.apply)
    val d                                = TuiTestDriver(wrapped, width = 20, height = 4)
    d.init()
    assert(!captured.cancelled)
    d.ctx.cancelSubs()
    assert(captured.cancelled, "cancelling via real ctx should cancel the adapted inner sub")

  test("inner Sub.InputSub is captured but never started"):
    val probe = new RecordingInputSub[InnerMsg]
    val inputInner: TuiApp[Int, InnerMsg] = new TuiApp[Int, InnerMsg]:
      override def init(ctx: RuntimeCtx[InnerMsg]): Tui[Int, InnerMsg] =
        ctx.registerSub(probe)
        0.tui
      override def update(m: Int, msg: InnerMsg, ctx: RuntimeCtx[InnerMsg]): Tui[Int, InnerMsg] = m.tui
      override def view(m: Int): RootNode                     = RootNode(10, 1, List.empty, None)
      override def toMsg(input: PromptLine): Result[InnerMsg] = Right(InnerMsg.Inc)

    val wrapped = Devtools.wrap(inputInner, toInputMsg = InnerMsg.Key.apply)
    val d       = TuiTestDriver(wrapped, width = 20, height = 4)
    d.init()
    assert(!probe.started, "wrapper must NOT start an inner Sub.InputSub")
    // And the real ctx should have exactly one registered sub: the wrapper's own
    // Sub.InputKey. The inner's InputSub is captured-not-forwarded.
    assert(d.ctx.registeredSubs.size == 1)
