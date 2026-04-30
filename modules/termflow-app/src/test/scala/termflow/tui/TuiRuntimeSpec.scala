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
