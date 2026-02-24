package termflow.tui

import termflow.tui.ACSUtils._

import java.util.concurrent.LinkedBlockingQueue
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.{ Failure, Success }

/**
 * Renderer responsible for converting virtual DOM to terminal output.
 */
trait TuiRenderer {

  /** Render the root node and optionally display an error. */
  def render(textNode: RootNode, err: Option[TermFlowError]): Unit
}

/**
 * Runtime context providing access to terminal and command publishing.
 *
 * The context is passed to `TuiApp.init` and `TuiApp.update` to allow
 * applications to:
 *  - Create subscriptions for keyboard input, timers, etc.
 *  - Access terminal dimensions
 *  - Publish commands to the runtime
 *
 * @tparam Msg The message type used by the application
 */
trait RuntimeCtx[Msg] extends EventSink[Msg] {

  /** Access the terminal backend for dimensions and reader. */
  def terminal: TerminalBackend

  /**
   * Register a subscription for automatic cleanup on exit.
   * Returns the subscription for chaining.
   */
  def registerSub(sub: Sub[Msg]): Sub[Msg]
}

/** Read side of the command bus used by the runtime loop. */
trait CmdConsumer[Msg] {
  def take(): Cmd[Msg]
}

/** Bidirectional command bus: producers publish, the runtime consumes. */
trait CmdBus[Msg] extends RuntimeCtx[Msg] with CmdConsumer[Msg] {
  def cancelAllSubscriptions(): Unit
}

/** Default in-memory command bus backed by a LinkedBlockingQueue. */
final class LocalCmdBus[Msg](val terminal: TerminalBackend) extends CmdBus[Msg] {
  private val queue                                   = new LinkedBlockingQueue[Cmd[Msg]]()
  private val subscriptions: java.util.List[Sub[Msg]] = new java.util.concurrent.CopyOnWriteArrayList[Sub[Msg]]()
  override def publish(cmd: Cmd[Msg]): Unit           = queue.put(cmd)
  override def take(): Cmd[Msg]                       = queue.take()
  override def registerSub(sub: Sub[Msg]): Sub[Msg]   = { subscriptions.add(sub); sub }
  override def cancelAllSubscriptions(): Unit         = subscriptions.forEach(_.cancel())
}

object TuiRuntime {

  given ExecutionContext = ExecutionContext.global

  /**
   * Run a TUI application with the given renderer and terminal backend.
   *
   * This method enters alternate buffer mode, runs the application loop,
   * and ensures terminal state is restored on exit (whether normal or due to error).
   */
  def run[Model, Msg](
    app: TuiApp[Model, Msg],
    renderer: TuiRenderer = SimpleANSIRenderer(),
    terminalBackend: TerminalBackend = new JLineTerminalBackend()
  ): Unit = {

    val bus: CmdBus[Msg] = new LocalCmdBus[Msg](terminalBackend)

    // Enter alternate buffer and set up terminal
    EnterAlternatBuffer()
    ClearScreen()
    print(ANSI.showCursor)

    try {
      // Build initial model and command using the provided runtime context
      val initial: Tui[Model, Msg] = app.init(bus)
      bus.publish(initial.cmd)

      @tailrec
      def loop(model: Model): Unit = {
        val cmd = bus.take()
        cmd match {
          case Cmd.Exit =>
            // Exit handled in finally block - just return
            ()

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
                bus.publish(Cmd.TermFlowErrorCmd(TermFlowError.Unexpected(e.getMessage, Some(e))))
            }
            onEnqueue match {
              case Some(msg) => bus.publish(Cmd.GCmd(msg))
              case None      => bus.publish(Cmd.NoCmd)
            }
            loop(model)
        }
      }

      loop(initial.model)
    } finally {
      // Cancel all registered subscriptions to stop background threads
      bus.cancelAllSubscriptions()
      // Always restore terminal state, even on crash
      print(ANSI.showCursor)
      EnterNormalBuffer()
      println("Goodbye from TermFlow!")
      terminalBackend.close()
    }
  }
}
