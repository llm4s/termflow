package termflow.tui

import termflow.tui.KeyDecoder.InputKey

import java.io.Reader
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.util.Failure
import scala.util.Try

/**
 * A subscription represents a long-running background task that emits messages.
 *
 * Subscriptions are used for:
 *  - Keyboard input (`Sub.InputKey`)
 *  - Timers (`Sub.Every`)
 *  - Terminal resize detection (`Sub.TerminalResize`)
 *
 * Subscriptions should be cancelled when no longer needed to free resources.
 * Register subscriptions with `RuntimeCtx.registerSub` for automatic cleanup on exit.
 *
 * @tparam Msg The message type emitted by this subscription
 */
trait Sub[+Msg]:

  /** Returns true if the subscription is still active. */
  def isActive: Boolean

  /** Cancel the subscription and release associated resources. */
  def cancel(): Unit

  /**
   * Begin any background activity (timers, reader threads, …).
   *
   * The default is a no-op for back-compat with `Sub` implementations that
   * start their work eagerly in their constructor. Deferred-start factories
   * — [[Sub.Every]] (timer scheduler) and [[Sub.InputKeyFromSource]]
   * (keyboard reader thread) — override this so the registering context
   * decides when the work actually begins:
   *
   *   - `LocalCmdBus.registerSub` calls `start()` immediately after recording
   *     the sub, preserving existing production behaviour.
   *   - `TestRuntimeCtx.registerSub` deliberately does **not** call `start()`,
   *     so both timer- and input-driven subs stay dormant inside the testkit.
   *   - Wrapper decorators (e.g. a future `Devtools.wrap`) can intercept the
   *     inner app's `registerSub`, swap its input source for their own, and
   *     skip calling `start()` on the original — all without racing for
   *     keystrokes.
   *
   * Calling `start()` more than once must be safe — the contract is "start if
   * not already started".
   */
  def start(): Unit = ()

object Sub:

  /**
   * Marker for subscriptions whose primary job is delivering keyboard
   * input to the app.
   *
   * Decorators that want to intercept the user's keystrokes (the most
   * prominent example is [[Devtools.wrap]]) use this marker to tell
   * input-sources apart from timer-/resize-style subs: the decorator
   * swallows [[InputSub]]s so it can feed the inner app its own input
   * stream, while still starting all other subs normally.
   *
   * Only [[InputKeyFromSource]] extends this today — apps rarely need
   * to construct an `InputSub` by hand, but custom key sources may
   * extend it to opt into the same treatment.
   */
  trait InputSub[+Msg] extends Sub[Msg]

  private def autoRegisterIfRuntimeCtx[Msg](sub: Sub[Msg], sink: EventSink[Msg]): Sub[Msg] =
    sink match
      case ctx: RuntimeCtx[?] =>
        ctx.asInstanceOf[RuntimeCtx[Msg]].registerSub(sub)
      case _ =>
        // Non-RuntimeCtx sinks don't get the registerSub hook, so honour the
        // start contract here to preserve eager-start semantics for existing
        // call sites that pass a bare EventSink.
        sub.start()
        sub

  /** A no-op subscription. Useful as a placeholder in model fields. */
  case object NoSub extends Sub[Nothing]:
    override def isActive: Boolean = false
    override def cancel(): Unit    = ()

  /**
   * Create a timer subscription that fires at a fixed interval.
   *
   * Each tick publishes `Cmd.GCmd(msg())` through `sink`. The thunk runs on
   * a single background scheduler thread, so `msg()` must not block.
   *
   * The scheduler is constructed lazily by [[Sub.start]] rather than in this
   * factory, so `TestRuntimeCtx` can keep timers dormant during snapshot
   * tests (it does not call `start()` on registered subs). For all
   * production paths (`LocalCmdBus.registerSub`, bare `EventSink` sinks)
   * `start()` runs synchronously immediately after construction, preserving
   * the original eager-start behaviour.
   *
   * @param millis Interval between ticks, in milliseconds.
   * @param msg Thunk producing the next message on each tick.
   * @param sink Where to publish ticks. When called with a [[RuntimeCtx]]
   *             the subscription auto-registers for cleanup on exit.
   */
  def Every[Msg](millis: Long, msg: () => Msg, sink: EventSink[Msg]): Sub[Msg] =
    val sub = new Sub[Msg]:
      private val lock                                                               = new Object
      @volatile private var active                                                   = true
      @volatile private var scheduler: java.util.concurrent.ScheduledExecutorService = null
      @volatile private var handle: java.util.concurrent.ScheduledFuture[?]          = null

      override def start(): Unit =
        lock.synchronized {
          if scheduler == null && active then
            val s = Executors.newSingleThreadScheduledExecutor(ThreadUtils.newThreadFactory())
            scheduler = s
            handle = s.scheduleAtFixedRate(
              () => sink.publish(Cmd.GCmd[Msg](msg())),
              0L,
              millis,
              TimeUnit.MILLISECONDS
            )
        }

      override def isActive: Boolean = active

      override def cancel(): Unit =
        lock.synchronized {
          active = false
          if handle != null then handle.cancel(true): Unit
          if scheduler != null then
            scheduler.shutdownNow(): Unit
            try scheduler.awaitTermination(200L, TimeUnit.MILLISECONDS): Unit
            catch {
              case _: InterruptedException =>
                Thread.currentThread().interrupt()
            }
        }
    autoRegisterIfRuntimeCtx(sub, sink)

  /**
   * Create a keyboard-input subscription from an explicit
   * [[TerminalKeySource]].
   *
   * Each decoded [[KeyDecoder.InputKey]] is wrapped with `msg` and published
   * to `sink`. Read failures are surfaced via `onError`.
   *
   * The producer thread is constructed lazily by [[Sub.start]] rather than
   * in this factory so `TestRuntimeCtx` can keep input subs dormant during
   * snapshot tests, and wrapper decorators can intercept the inner app's
   * input subscription by swapping it for their own before `start()`
   * is called. Production callers (`LocalCmdBus.registerSub` and bare
   * `EventSink` sinks routed through [[autoRegisterIfRuntimeCtx]]) get
   * `start()` invoked synchronously right after construction, so the
   * observable behaviour is identical to the eager version.
   *
   * Most apps should prefer [[InputKey]], which uses the runtime context's
   * terminal reader.
   */
  private def inputKeyFromSourceFactory[Msg](
    sourceFactory: () => TerminalKeySource,
    closeUnstarted: () => Unit,
    msg: InputKey => Msg,
    onError: Throwable => Msg,
    sink: EventSink[Msg]
  ): Sub[Msg] =
    val sub = new InputSub[Msg]:
      private val lock                                = new Object
      @volatile private var active                    = true
      @volatile private var source: TerminalKeySource = null
      @volatile private var thread: Thread            = null

      override def start(): Unit =
        lock.synchronized {
          if thread == null && active then
            val startedSource = sourceFactory()
            source = startedSource
            thread = ThreadUtils.startThread(() =>
              try
                while active do
                  startedSource.next() match
                    case InputRead.Key(key) =>
                      if active then sink.publish(Cmd.GCmd(msg(key)))
                    case InputRead.End =>
                      active = false
                    case InputRead.Failed(_: InterruptedException) =>
                      active = false
                    case InputRead.Failed(err) =>
                      if active then sink.publish(Cmd.GCmd(onError(err)))
              catch {
                case _: InterruptedException =>
                  ()
                case e: Throwable =>
                  if active then sink.publish(Cmd.GCmd(onError(e)))
              }
            )
        }

      override def isActive: Boolean = active

      override def cancel(): Unit =
        lock.synchronized {
          active = false
          if source == null then closeUnstarted()
          else
            source.close() match
              case Failure(_) => ()
              case _          => ()
          if thread != null then
            thread.interrupt()
            try thread.join(200L)
            catch {
              case _: InterruptedException =>
                Thread.currentThread().interrupt()
            }
        }
    autoRegisterIfRuntimeCtx(sub, sink)

  def InputKeyFromSource[Msg](
    source: TerminalKeySource,
    msg: InputKey => Msg,
    onError: Throwable => Msg,
    sink: EventSink[Msg]
  ): Sub[Msg] =
    inputKeyFromSourceFactory(
      sourceFactory = () => source,
      closeUnstarted = () => { val _ = source.close() },
      msg = msg,
      onError = onError,
      sink = sink
    )

  /**
   * Poll terminal dimensions at a fixed interval and publish a message when
   * they change.
   *
   * Useful for apps that need to re-flow their view when the window is
   * resized. The subscription publishes through `ctx.publish` and
   * auto-registers for cleanup on exit.
   *
   * @param millis Polling interval, in milliseconds.
   * @param mkMsg Function producing the resize message from `(width, height)`.
   */
  def TerminalResize[Msg](
    millis: Long,
    mkMsg: (Int, Int) => Msg,
    ctx: RuntimeCtx[Msg]
  ): Sub[Msg] =
    val sub = new Sub[Msg]:
      private val lock                                                               = new Object
      @volatile private var active                                                   = true
      @volatile private var scheduler: java.util.concurrent.ScheduledExecutorService = null
      @volatile private var handle: java.util.concurrent.ScheduledFuture[?]          = null
      @volatile private var w0                                                       = 0
      @volatile private var h0                                                       = 0

      override def start(): Unit =
        lock.synchronized {
          if scheduler == null && active then
            w0 = ctx.terminal.width
            h0 = ctx.terminal.height
            val s = Executors.newSingleThreadScheduledExecutor(ThreadUtils.newThreadFactory())
            scheduler = s
            handle = s.scheduleAtFixedRate(
              () =>
                if active then
                  val w = ctx.terminal.width
                  val h = ctx.terminal.height
                  if w != w0 || h != h0 then
                    w0 = w
                    h0 = h
                    ctx.publish(Cmd.GCmd(mkMsg(w, h))),
              0L,
              millis,
              TimeUnit.MILLISECONDS
            )
        }

      override def isActive: Boolean = active

      override def cancel(): Unit =
        lock.synchronized {
          active = false
          if handle != null then handle.cancel(true): Unit
          if scheduler != null then
            scheduler.shutdownNow(): Unit
            try scheduler.awaitTermination(200L, TimeUnit.MILLISECONDS): Unit
            catch {
              case _: InterruptedException =>
                Thread.currentThread().interrupt()
            }
        }
    ctx.registerSub(sub)

  /**
   * Create a keyboard-input subscription from an explicit [[java.io.Reader]].
   *
   * Prefer [[InputKey]] for apps using the runtime's terminal backend. This
   * form is mainly useful for tests and bespoke embeddings.
   */
  def InputKeyWithReader[Msg](
    msg: InputKey => Msg,
    onError: Throwable => Msg,
    sink: EventSink[Msg],
    reader: Reader
  ): Sub[Msg] =
    inputKeyFromSourceFactory(
      sourceFactory = () => ConsoleKeyPressSource(reader),
      closeUnstarted = () => { val _ = Try(reader.close()) },
      msg = msg,
      onError = onError,
      sink = sink
    )

  /**
   * Create a keyboard-input subscription using the terminal reader from the
   * runtime context. The idiomatic form for `TuiApp.init`:
   *
   * {{{
   * override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
   *   Model(
   *     input = Sub.InputKey(Msg.KeyPress.apply, Msg.KeyError.apply, ctx),
   *     ...
   *   ).tui
   * }}}
   */
  def InputKey[Msg](msg: InputKey => Msg, onError: Throwable => Msg, ctx: RuntimeCtx[Msg]): Sub[Msg] =
    inputKeyFromSourceFactory(
      sourceFactory = () => ConsoleKeyPressSource(ctx.terminal.reader),
      closeUnstarted = () => { val _ = Try(ctx.terminal.reader.close()) },
      msg = msg,
      onError = onError,
      sink = ctx
    )

/**
 * Sink that receives commands from subscriptions and other asynchronous
 * sources. The runtime implements this via [[CmdBus]].
 *
 * @note SPI. Custom implementations are only needed when replacing the
 *       default command bus.
 */
trait EventSink[Msg]:

  /** Publish a command to be processed by the runtime loop. Thread-safe. */
  def publish(cmd: Cmd[Msg]): Unit

/**
 * Producer of events that can be started against a sink.
 *
 * @note SPI. Most applications construct subscriptions directly via the
 *       factories on the [[Sub]] companion rather than implementing this.
 */
trait EventSource[Msg]:

  /** Start the source, publishing to `sink`, and return a cancellation handle. */
  def start(sink: EventSink[Msg]): Sub[Msg]

object RandomUtil:
  final class RandomSourceAtFixedRate[A, Msg](period: Long, next: () => A, toMsg: A => Msg):
    def asEventSource(sink: EventSink[Msg]): Sub[Msg] =
      val scheduler = Executors.newSingleThreadScheduledExecutor(ThreadUtils.newThreadFactory())
      scheduler.scheduleAtFixedRate(
        () =>
          val value = next()
          sink.publish(Cmd.GCmd[Msg](toMsg(value)))
        ,
        0L,
        period,
        TimeUnit.MILLISECONDS
      )
      new Sub[Msg]:
        @volatile private var active = true
        def isActive: Boolean        = active
        def cancel(): Unit =
          scheduler.shutdownNow(): Unit
          try scheduler.awaitTermination(200L, TimeUnit.MILLISECONDS): Unit
          catch {
            case _: InterruptedException =>
              Thread.currentThread().interrupt()
          }
          active = false
