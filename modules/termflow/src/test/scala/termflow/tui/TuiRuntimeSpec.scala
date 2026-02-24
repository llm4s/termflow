package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.TuiPrelude._

import java.io.{ ByteArrayOutputStream, PrintStream, Reader, StringReader }
import scala.concurrent.Future

class TuiRuntimeSpec extends AnyFunSuite {

  final private class TestTerminalBackend extends TerminalBackend {
    override def reader: Reader = new StringReader("")
    override def width: Int     = 80
    override def height: Int    = 24
    override def close(): Unit  = ()
  }

  final private class NoopRenderer extends TuiRenderer {
    override def render(textNode: RootNode, err: Option[TermFlowError]): Unit = ()
  }

  test("TuiRuntime handles FCmd completion and exits") {
    object App extends TuiApp[Int, Unit] {
      override def init(ctx: RuntimeCtx[Unit]): Tui[Int, Unit] =
        Tui(
          model = 0,
          cmd = Cmd.FCmd(
            task = Future.successful(1),
            toCMD = _ => Cmd.Exit,
            onEnqueue = None
          )
        )

      override def update(model: Int, msg: Unit, ctx: RuntimeCtx[Unit]): Tui[Int, Unit] = Tui(model)

      override def view(model: Int): RootNode = RootNode(80, 24, children = List.empty, input = None)

      override def toMsg(input: PromptLine): Result[Unit] = Right(())
    }

    Console.withOut(new PrintStream(new ByteArrayOutputStream())) {
      TuiRuntime.run(
        app = App,
        renderer = new NoopRenderer,
        terminalBackend = new TestTerminalBackend
      )
    }

    succeed
  }
}
