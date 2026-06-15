package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.TuiPrelude.*

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.io.Reader
import java.io.StringReader
import java.io.StringWriter
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future

class TuiRuntimeSpec extends AnyFunSuite:
  private val TestConfig =
    TermFlowConfig(
      logging = LoggingConfig(LogPath(Path.of("target", "termflow-test.log"))),
      metrics = MetricsConfig(enabled = false)
    )

  final private class TestTerminalBackend extends TerminalBackend:
    val out                     = new StringWriter()
    override def reader: Reader = new StringReader("")
    override def writer         = out
    override def width: Int     = 80
    override def height: Int    = 24
    override def close(): Unit  = ()

  final private class TrackingTerminalBackend extends TerminalBackend:
    val closed                  = new AtomicBoolean(false)
    val out                     = new StringWriter()
    override def reader: Reader = new StringReader("")
    override def writer         = out
    override def width: Int     = 80
    override def height: Int    = 24
    override def close(): Unit  = closed.set(true)

  final private class NoopRenderer extends TuiRenderer:
    override def render(
      textNode: RootNode,
      err: Option[TermFlowError],
      terminal: TerminalBackend,
      renderMetrics: RenderMetrics
    ): Unit = ()

  test("TuiRuntime handles FCmd completion and exits"):
    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        Tui(
          model = 0,
          cmd = Cmd.FCmd(
            task = Future.successful(1),
            toCmd = _ => Cmd.Exit,
            onEnqueue = None
          )
        )

      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)

      override def view(model: Int): RootNode = RootNode(80, 24, children = List.empty, input = None)

      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    Console.withOut(new PrintStream(new ByteArrayOutputStream())):
      TuiRuntime.run(
        app = App,
        renderer = new NoopRenderer,
        terminalBackend = new TestTerminalBackend,
        config = TestConfig
      )

    succeed

  test("LocalCmdBus.cancelAllSubscriptions continues when one cancel throws"):
    val bus = new LocalCmdBus[Unit](new TestTerminalBackend, TestConfig)

    val called = new AtomicBoolean(false)
    bus.registerSub(new Sub[Unit]:
      override def isActive: Boolean = true
      override def cancel(): Unit    = throw new RuntimeException("boom")
    )
    bus.registerSub(new Sub[Unit]:
      override def isActive: Boolean = true
      override def cancel(): Unit    = called.set(true)
    )

    bus.cancelAllSubscriptions()
    assert(called.get())

  test("unexpectedMessage falls back to exception class when message is null or blank"):
    val nullMessageEx = new RuntimeException(null: String)
    assert(TuiRuntime.unexpectedMessage(nullMessageEx) == "RuntimeException")

    val blankMessageEx = new IllegalStateException("   ")
    assert(TuiRuntime.unexpectedMessage(blankMessageEx) == "IllegalStateException")

    val messageEx = new IllegalArgumentException("boom")
    assert(TuiRuntime.unexpectedMessage(messageEx) == "boom")

  test("runtime shutdown closes terminal backend and omits goodbye text"):
    val backend = new TrackingTerminalBackend

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit]                          = Tui(model = 0, cmd = Cmd.Exit)
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    TuiRuntime.run(
      app = App,
      renderer = new NoopRenderer,
      terminalBackend = backend,
      config = TestConfig
    )

    assert(backend.closed.get())
    assert(!backend.out.toString.contains("Goodbye from TermFlow!"))

  test("TuiRuntime executes FCmd failure path and surfaces fallback unexpected message"):
    final class StopRuntime extends RuntimeException("stop-runtime")

    val seenErr = new AtomicReference[Option[TermFlowError]](None)

    val renderer = new TuiRenderer:
      override def render(
        textNode: RootNode,
        err: Option[TermFlowError],
        terminal: TerminalBackend,
        renderMetrics: RenderMetrics
      ): Unit =
        err.foreach { e =>
          seenErr.set(Some(e))
          throw new StopRuntime
        }

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        Tui(
          model = 0,
          cmd = Cmd.FCmd(
            task = Future.failed(new RuntimeException(null: String)),
            toCmd = _ => Cmd.Exit,
            onEnqueue = None
          )
        )

      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    val backend = new TestTerminalBackend
    intercept[StopRuntime]:
      TuiRuntime.run(
        app = App,
        renderer = renderer,
        terminalBackend = backend,
        config = TestConfig
      )

    assert(
      seenErr.get().contains(TermFlowError.Unexpected("RuntimeException", Some(new RuntimeException(null: String))))
        || seenErr.get().exists {
          case TermFlowError.Unexpected(msg, Some(_: RuntimeException)) => msg == "RuntimeException"
          case _                                                        => false
        }
    )

  test("TuiRuntime surfaces an error when the FCmd success mapper throws"):
    final class StopRuntime extends RuntimeException("stop-runtime")

    val seenErr = new AtomicReference[Option[TermFlowError]](None)

    val renderer = new TuiRenderer:
      override def render(
        textNode: RootNode,
        err: Option[TermFlowError],
        terminal: TerminalBackend,
        renderMetrics: RenderMetrics
      ): Unit =
        err.foreach { e =>
          seenErr.set(Some(e))
          throw new StopRuntime
        }

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        Tui(
          model = 0,
          // The Future succeeds, but the success mapper blows up. Without the
          // guard this throws on the execution-context thread and the runtime
          // never sees either the intended command or an error.
          cmd = Cmd.FCmd(
            task = Future.successful(()),
            toCmd = _ => throw new RuntimeException("mapper-boom"),
            onEnqueue = None
          )
        )

      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    val backend = new TestTerminalBackend
    intercept[StopRuntime]:
      TuiRuntime.run(
        app = App,
        renderer = renderer,
        terminalBackend = backend,
        config = TestConfig
      )

    assert(seenErr.get().exists {
      case TermFlowError.Unexpected(msg, Some(_: RuntimeException)) => msg == "mapper-boom"
      case _                                                        => false
    })

  test("TuiRuntime dispatches GCmd and runs onEnqueue follow-up before completion"):
    enum Msg:
      case Step, Done

    val log = new java.util.concurrent.atomic.AtomicReference[List[Msg]](Nil)

    object App extends TuiApp[Int, Msg]:
      override def init(ctx: RuntimeCtx[Msg]): Tui[Int, Msg] =
        Tui(model = 0, cmd = Cmd.GCmd(Msg.Step))

      override def update(model: Int, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Int, Msg] =
        log.updateAndGet(prev => msg :: prev): Unit
        msg match
          case Msg.Step =>
            // Schedule a Future that resolves to Done; supply onEnqueue so the
            // runtime publishes a synchronous GCmd in addition to the Future
            // result. Both together exercise the FCmd onEnqueue Some branch.
            Tui(
              model = model + 1,
              cmd = Cmd.FCmd(
                task = Future.successful(()),
                toCmd = _ => Cmd.GCmd(Msg.Done),
                onEnqueue = Some(Msg.Step)
              )
            )
          case Msg.Done => Tui(model + 10, Cmd.Exit)

      override def view(model: Int): RootNode            = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Msg] = Right(Msg.Step)

    Console.withOut(new PrintStream(new ByteArrayOutputStream())):
      TuiRuntime.run(
        app = App,
        renderer = new NoopRenderer,
        terminalBackend = new TestTerminalBackend,
        config = TestConfig
      )

    val seen = log.get().reverse
    // We expect at least: initial Step, the onEnqueue-published Step, and Done.
    assert(seen.contains(Msg.Step))
    assert(seen.contains(Msg.Done))

  test("TuiRuntime forwards Cmd.RequestAttention and Cmd.Notify to the backend"):
    final class CapturingBackend extends TerminalBackend:
      val out                     = new StringWriter()
      val attentionCalls          = new java.util.concurrent.atomic.AtomicInteger(0)
      val notifyCalls             = new java.util.concurrent.atomic.AtomicInteger(0)
      override def reader: Reader = new StringReader("")
      override def writer         = out
      override def width: Int     = 80
      override def height: Int    = 24
      override def close(): Unit  = ()
      override def requestAttention(): Unit =
        attentionCalls.incrementAndGet(): Unit
      override def notify(title: String, body: String): Unit =
        notifyCalls.incrementAndGet(): Unit

    object App extends TuiApp[Int, Int]:
      override def init(ctx: RuntimeCtx[Int]): Tui[Int, Int] =
        // Publish RequestAttention and Notify directly via the bus so the
        // runtime processes them; chain a final GCmd that triggers Exit.
        ctx.publish(Cmd.RequestAttention)
        ctx.publish(Cmd.Notify("Build", "done"))
        ctx.publish(Cmd.GCmd(1))
        Tui(0, Cmd.NoCmd)
      override def update(model: Int, msg: Int, ctx: RuntimeCtx[Int]): Tui[Int, Int] =
        Tui(model, Cmd.Exit)

      override def view(model: Int): RootNode            = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Int] = Right(0)

    val backend = new CapturingBackend
    Console.withOut(new PrintStream(new ByteArrayOutputStream())):
      TuiRuntime.run(
        app = App,
        renderer = new NoopRenderer,
        terminalBackend = backend,
        config = TestConfig
      )

    assert(backend.attentionCalls.get() >= 1, "RequestAttention should reach the backend")
    assert(backend.notifyCalls.get() >= 1, "Notify should reach the backend")

  test("runtime restores cursor and exits alt buffer when render is interrupted"):
    final class StopRuntime extends RuntimeException("stop-runtime")

    val renderer = new TuiRenderer:
      override def render(
        textNode: RootNode,
        err: Option[TermFlowError],
        terminal: TerminalBackend,
        renderMetrics: RenderMetrics
      ): Unit =
        throw new StopRuntime

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model = 0, cmd = Cmd.NoCmd)
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    val backend = new TestTerminalBackend
    intercept[StopRuntime]:
      TuiRuntime.run(
        app = App,
        renderer = renderer,
        terminalBackend = backend,
        config = TestConfig
      )

    val printed = backend.out.toString
    assert(printed.contains(ANSI.showCursor))
    assert(printed.contains(ANSI.exitAltBuffer))

  test("runtime enables and disables bracketed paste when the backend supports it"):
    final class StopRuntime extends RuntimeException("stop-runtime")

    final class PasteCapableBackend extends TerminalBackend:
      val out                     = new StringWriter()
      override def reader: Reader = new StringReader("")
      override def writer         = out
      override def width: Int     = 80
      override def height: Int    = 24
      override def close(): Unit  = ()
      override def capabilities: Capabilities =
        Capabilities.default.copy(bracketedPaste = true)

    val renderer = new TuiRenderer:
      override def render(
        textNode: RootNode,
        err: Option[TermFlowError],
        terminal: TerminalBackend,
        renderMetrics: RenderMetrics
      ): Unit = throw new StopRuntime

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model = 0, cmd = Cmd.NoCmd)
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    val backend = new PasteCapableBackend
    intercept[StopRuntime]:
      TuiRuntime.run(app = App, renderer = renderer, terminalBackend = backend, config = TestConfig)

    val printed = backend.out.toString
    assert(printed.contains(ANSI.enableBracketedPaste))
    assert(printed.contains(ANSI.disableBracketedPaste))

  test("runtime enables and disables mouse mode when the backend supports it"):
    final class StopRuntime extends RuntimeException("stop-runtime")

    final class MouseCapableBackend extends TerminalBackend:
      val out                     = new StringWriter()
      override def reader: Reader = new StringReader("")
      override def writer         = out
      override def width: Int     = 80
      override def height: Int    = 24
      override def close(): Unit  = ()
      override def capabilities: Capabilities =
        Capabilities.default.copy(mouse = true)

    val renderer = new TuiRenderer:
      override def render(
        textNode: RootNode,
        err: Option[TermFlowError],
        terminal: TerminalBackend,
        renderMetrics: RenderMetrics
      ): Unit = throw new StopRuntime

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model = 0, cmd = Cmd.NoCmd)
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    val backend = new MouseCapableBackend
    intercept[StopRuntime]:
      TuiRuntime.run(app = App, renderer = renderer, terminalBackend = backend, config = TestConfig)

    val printed = backend.out.toString
    assert(printed.contains(ANSI.enableMouse))
    assert(printed.contains(ANSI.disableMouse))

  test("runtime omits bracketed-paste setup when backend does not support it"):
    final class StopRuntime extends RuntimeException("stop-runtime")

    final class NoPasteBackend extends TerminalBackend:
      val out                     = new StringWriter()
      override def reader: Reader = new StringReader("")
      override def writer         = out
      override def width: Int     = 80
      override def height: Int    = 24
      override def close(): Unit  = ()
      override def capabilities: Capabilities =
        Capabilities.default.copy(bracketedPaste = false)

    val renderer = new TuiRenderer:
      override def render(
        textNode: RootNode,
        err: Option[TermFlowError],
        terminal: TerminalBackend,
        renderMetrics: RenderMetrics
      ): Unit = throw new StopRuntime

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model = 0, cmd = Cmd.NoCmd)
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    val backend = new NoPasteBackend
    intercept[StopRuntime]:
      TuiRuntime.run(app = App, renderer = renderer, terminalBackend = backend, config = TestConfig)

    val printed = backend.out.toString
    assert(!printed.contains(ANSI.enableBracketedPaste))
    assert(!printed.contains(ANSI.disableBracketedPaste))

  final private class CapturingRenderer(onError: TermFlowError => Unit) extends TuiRenderer:
    override def render(
      textNode: RootNode,
      err: Option[TermFlowError],
      terminal: TerminalBackend,
      renderMetrics: RenderMetrics
    ): Unit = err.foreach(onError)

  test("a throwing update is surfaced as an error banner without crashing the loop"):
    val captured = new AtomicReference[Option[TermFlowError]](None)
    val done     = scala.concurrent.Promise[Unit]()

    object App extends TuiApp[Int, Unit]:
      // onEnqueue = Some(()) publishes a GCmd(()) (which throws in update);
      // the future, completed once the error banner renders, drives Exit.
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        Tui(0, Cmd.FCmd(done.future, _ => Cmd.Exit, onEnqueue = Some(())))
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        throw new RuntimeException("update boom")
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    val renderer = new CapturingRenderer(err =>
      if captured.get().isEmpty then
        captured.set(Some(err))
        done.trySuccess(()): Unit
    )
    Console.withOut(new PrintStream(new ByteArrayOutputStream())):
      TuiRuntime.run(app = App, renderer = renderer, terminalBackend = new TestTerminalBackend, config = TestConfig)
    assert(captured.get().nonEmpty, "a throwing update should surface a TermFlowError")

  test("a throwing view falls back to an error banner instead of crashing"):
    val captured = new AtomicReference[Option[TermFlowError]](None)
    val done     = scala.concurrent.Promise[Unit]()

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        Tui(0, Cmd.FCmd(done.future, _ => Cmd.Exit, onEnqueue = None))
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)
      override def view(model: Int): RootNode             = throw new RuntimeException("view boom")
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    val renderer = new CapturingRenderer(err =>
      if captured.get().isEmpty then
        captured.set(Some(err))
        done.trySuccess(()): Unit
    )
    Console.withOut(new PrintStream(new ByteArrayOutputStream())):
      TuiRuntime.run(app = App, renderer = renderer, terminalBackend = new TestTerminalBackend, config = TestConfig)
    assert(captured.get().nonEmpty, "a throwing view should surface a TermFlowError")

  test("a failing FCmd Future surfaces as a TermFlowError without crashing the loop"):
    val captured = new AtomicReference[Option[TermFlowError]](None)
    val done     = scala.concurrent.Promise[Unit]()

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        // A second FCmd, completed once the error banner renders, drives Exit;
        // the primary FCmd's Future fails and must surface as a TermFlowError
        // rather than dying silently on the execution context.
        ctx.publish(Cmd.FCmd(done.future, _ => Cmd.Exit, onEnqueue = None))
        Tui(0, Cmd.FCmd(Future.failed(new RuntimeException("future boom")), _ => Cmd.NoCmd, onEnqueue = None))
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    val renderer = new CapturingRenderer(err =>
      if captured.get().isEmpty then
        captured.set(Some(err))
        done.trySuccess(()): Unit
    )
    Console.withOut(new PrintStream(new ByteArrayOutputStream())):
      TuiRuntime.run(app = App, renderer = renderer, terminalBackend = new TestTerminalBackend, config = TestConfig)

    assert(
      captured.get().exists {
        case TermFlowError.Unexpected(msg, Some(_: RuntimeException)) => msg == "future boom"
        case _                                                        => false
      },
      s"a failing FCmd Future should surface as TermFlowError.Unexpected; saw ${captured.get()}"
    )

  test("a Sub raising an exception on its own thread does not kill the runtime"):
    val subFailed = new java.util.concurrent.CountDownLatch(1)
    val backend   = new TrackingTerminalBackend

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        // A subscription whose background thread throws. The runtime runs the
        // loop on a different thread, so the failure must stay contained. The
        // failing thread publishes a follow-up command in a `finally` (so it
        // runs even as the exception propagates) which drives a clean Exit,
        // proving the bus and loop survived the Sub blowing up.
        ctx.registerSub(new Sub[Unit]:
          override def isActive: Boolean = true
          override def cancel(): Unit    = ()
          override def start(): Unit =
            // Explicit Runnable (not a lambda): the body is `Nothing`-typed
            // because it ends in a throw, which a SAM lambda cannot convert to
            // void. The finally still runs as the exception propagates.
            val t = new Thread(
              new Runnable:
                override def run(): Unit =
                  try throw new RuntimeException("sub boom")
                  finally
                    subFailed.countDown()
                    ctx.publish(Cmd.GCmd(()))
            )
            // Swallow the (deliberately) uncaught exception so it does not
            // spam stderr; the latch above already proves the throw happened.
            t.setUncaughtExceptionHandler(
              new Thread.UncaughtExceptionHandler:
                override def uncaughtException(thread: Thread, ex: Throwable): Unit = ()
            )
            t.setDaemon(true)
            t.start()
        )
        Tui(0, Cmd.NoCmd)
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        Tui(model, Cmd.Exit)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    Console.withOut(new PrintStream(new ByteArrayOutputStream())):
      TuiRuntime.run(app = App, renderer = new NoopRenderer, terminalBackend = backend, config = TestConfig)

    assert(
      subFailed.await(2, java.util.concurrent.TimeUnit.SECONDS),
      "the Sub thread should have thrown its exception"
    )
    assert(backend.closed.get(), "runtime should shut down cleanly after a Sub failure")

  test("runtime shuts down cleanly with commands still pending in the bus"):
    val cancelled  = new AtomicBoolean(false)
    val updates    = new java.util.concurrent.atomic.AtomicInteger(0)
    val backend    = new TrackingTerminalBackend
    val FirstBurst = 25

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        ctx.registerSub(new Sub[Unit]:
          override def isActive: Boolean = true
          override def cancel(): Unit    = cancelled.set(true)
        )
        // Enqueue a burst, then Exit, then a second burst. Exit is consumed
        // before the trailing commands, so the runtime must tear down with
        // work still queued — cancelling subs and restoring the terminal
        // rather than draining the remainder or hanging.
        (1 to FirstBurst).foreach(_ => ctx.publish(Cmd.GCmd(())))
        ctx.publish(Cmd.Exit)
        (1 to FirstBurst).foreach(_ => ctx.publish(Cmd.GCmd(())))
        Tui(0, Cmd.NoCmd)
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        updates.incrementAndGet(): Unit
        Tui(model, Cmd.NoCmd)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    Console.withOut(new PrintStream(new ByteArrayOutputStream())):
      TuiRuntime.run(app = App, renderer = new NoopRenderer, terminalBackend = backend, config = TestConfig)

    assert(updates.get() == FirstBurst, s"only the pre-Exit burst should run; saw ${updates.get()}")
    assert(cancelled.get(), "subscriptions should be cancelled on shutdown")
    assert(backend.closed.get(), "terminal backend should be closed on shutdown")
    val printed = backend.out.toString
    assert(
      printed.contains(ANSI.showCursor) && printed.contains(ANSI.exitAltBuffer),
      "terminal state should be restored on shutdown"
    )

  test("a command burst exceeding the per-frame coalescing cap is fully processed without loss"):
    // MaxCoalescedCommandsPerFrame is 4096; exceed it so the drain loop hits
    // its cap in one frame and spills the remainder into a later frame. Every
    // command must still be handled — backpressure buffers, it does not drop.
    val Burst   = 5000
    val handled = new java.util.concurrent.atomic.AtomicInteger(0)
    val frames  = new java.util.concurrent.atomic.AtomicInteger(0)

    val renderer = new TuiRenderer:
      override def render(
        textNode: RootNode,
        err: Option[TermFlowError],
        terminal: TerminalBackend,
        renderMetrics: RenderMetrics
      ): Unit = frames.incrementAndGet(): Unit

    object App extends TuiApp[Int, Unit]:
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        (1 to Burst).foreach(_ => ctx.publish(Cmd.GCmd(())))
        Tui(0, Cmd.NoCmd)
      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        val n = handled.incrementAndGet()
        // Each handled command schedules a render-triggering NoCmd, so the
        // tail of the burst piles up more than 4096 NoCmds and overflows the
        // per-frame coalescing cap. The final command tears the loop down.
        if n >= Burst then Tui(model, Cmd.Exit) else Tui(model, Cmd.NoCmd)
      override def view(model: Int): RootNode             = RootNode(80, 24, children = List.empty, input = None)
      override def toMsg(input: PromptLine): Result[Unit] = Right(())

    Console.withOut(new PrintStream(new ByteArrayOutputStream())):
      TuiRuntime.run(app = App, renderer = renderer, terminalBackend = new TestTerminalBackend, config = TestConfig)

    assert(handled.get() == Burst, s"every burst command must be handled; saw ${handled.get()}")
    assert(frames.get() >= 1, "the burst should coalesce into at least one rendered frame")
    assert(frames.get() < Burst, s"thousands of commands must coalesce into few frames; saw ${frames.get()}")
