package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.TuiPrelude._

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.io.Reader
import java.io.StringReader
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future

class TuiRuntimeSpec extends AnyFunSuite:

  final private class TestTerminalBackend extends TerminalBackend:
    override def reader: Reader = new StringReader("")
    override def width: Int     = 80
    override def height: Int    = 24
    override def close(): Unit  = ()

  final private class TrackingTerminalBackend extends TerminalBackend:
    val closed                  = new AtomicBoolean(false)
    override def reader: Reader = new StringReader("")
    override def width: Int     = 80
    override def height: Int    = 24
    override def close(): Unit  = closed.set(true)

  final private class NoopRenderer extends TuiRenderer:
    override def render(textNode: RootNode, err: Option[TermFlowError]): Unit = ()

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
        terminalBackend = new TestTerminalBackend
      )

    succeed

  test("LocalCmdBus.cancelAllSubscriptions continues when one cancel throws"):
    val bus = new LocalCmdBus[Unit](new TestTerminalBackend)

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

    val out = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(out)):
      TuiRuntime.run(
        app = App,
        renderer = new NoopRenderer,
        terminalBackend = backend
      )

    assert(backend.closed.get())
    assert(!out.toString("UTF-8").contains("Goodbye from TermFlow!"))
