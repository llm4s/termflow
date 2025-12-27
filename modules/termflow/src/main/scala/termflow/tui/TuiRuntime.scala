package termflow.tui

import termflow.tui.ACSUtils._

import java.util.concurrent.LinkedBlockingQueue
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.{ Failure, Success }

trait TuiRenderer {
  def render(textNode: RootNode, err: Option[TermFlowError]): Unit
}

/** Write-only context that apps and subscriptions use to send commands to the runtime. */
trait RuntimeCtx[Msg] extends EventSink[Msg] {
  def terminal: TerminalBackend
}

/** Read side of the command bus used by the runtime loop. */
trait CmdConsumer[Msg] {
  def take(): Cmd[Msg]
}

/** Bidirectional command bus: producers publish, the runtime consumes. */
trait CmdBus[Msg] extends RuntimeCtx[Msg] with CmdConsumer[Msg]

/** Default in-memory command bus backed by a LinkedBlockingQueue. */
final class LocalCmdBus[Msg](val terminal: TerminalBackend) extends CmdBus[Msg] {
  private val queue                         = new LinkedBlockingQueue[Cmd[Msg]]()
  override def publish(cmd: Cmd[Msg]): Unit = queue.put(cmd)
  override def take(): Cmd[Msg]             = queue.take()
}

object TuiRuntime {

  implicit val ec: ExecutionContext = ExecutionContext.global

  def run[Model, Msg](
    app: TuiApp[Model, Msg],
    renderer: TuiRenderer = SimpleANSIRenderer(),
    terminalBackend: TerminalBackend = new JLineTerminalBackend()
  ): Unit = {

    val bus: CmdBus[Msg] = new LocalCmdBus[Msg](terminalBackend)

    EnterAlternatBuffer()
    ClearScreen()
    print(ANSI.showCursor)

    // Build initial model and command using the provided runtime context
    val initial: Tui[Model, Msg] = app.init(bus)
    bus.publish(initial.cmd)

    @tailrec
    def loop(model: Model): Unit = {
      val cmd = bus.take()
      cmd match {
        case Cmd.Exit =>
          print(ANSI.showCursor)
          EnterNormalBuffer()
          println("👋 Goodbye from TermFlow!")
          // Ensure we clean up the terminal backend before exiting.
          terminalBackend.close()
          System.exit(0)

        case Cmd.TermFlowErrorCmd(err) =>
          renderer.render(app.view(model), Some(err))
          loop(model)

        case Cmd.NoCmd =>
          renderer.render(app.view(model), None)
          loop(model)

        case Cmd.GCmd(g) =>
          val next: Tui[Model, Msg] = app.update(model, g, bus)
          bus.publish(next.cmd)
          loop(next.model)

        case Cmd.FCmd(task, toCmd, onEnqueue) =>
          task.onComplete {
            case Success(result) =>
              bus.publish(toCmd(result))
            case Failure(e) =>
              bus.publish(Cmd.TermFlowErrorCmd(TermFlowError.Unexpected(e.getMessage)))
          }(ec)
          onEnqueue match {
            case Some(msg) => bus.publish(Cmd.GCmd(msg))
            case None      => bus.publish(Cmd.NoCmd)
          }
          loop(model)
      }
    }

    loop(initial.model)
  }
}
