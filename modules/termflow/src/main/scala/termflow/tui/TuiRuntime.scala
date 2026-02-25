package termflow.tui

import termflow.tui.ACSUtils._

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.ExecutionContext
import scala.util.Failure
import scala.util.Success

/**
 * Renderer responsible for converting virtual DOM to terminal output.
 */
trait TuiRenderer:

  /** Render the root node and optionally display an error. */
  def render(textNode: RootNode, err: Option[TermFlowError]): Unit

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
trait RuntimeCtx[Msg] extends EventSink[Msg]:

  /** Access the terminal backend for dimensions and reader. */
  def terminal: TerminalBackend

  /**
   * Register a subscription for automatic cleanup on exit.
   * Returns the subscription for chaining.
   */
  def registerSub(sub: Sub[Msg]): Sub[Msg]

/** Read side of the command bus used by the runtime loop. */
trait CmdConsumer[Msg]:
  def take(): Cmd[Msg]
  def poll(timeoutMillis: Long): Option[Cmd[Msg]]

/** Bidirectional command bus: producers publish, the runtime consumes. */
trait CmdBus[Msg] extends RuntimeCtx[Msg] with CmdConsumer[Msg]:
  def cancelAllSubscriptions(): Unit

/** Default in-memory command bus backed by a LinkedBlockingQueue. */
final class LocalCmdBus[Msg](val terminal: TerminalBackend) extends CmdBus[Msg]:
  private val queue                                   = new LinkedBlockingQueue[Cmd[Msg]]()
  private val subscriptions: java.util.List[Sub[Msg]] = new java.util.concurrent.CopyOnWriteArrayList[Sub[Msg]]()
  override def publish(cmd: Cmd[Msg]): Unit           = queue.put(cmd)
  override def take(): Cmd[Msg]                       = queue.take()
  override def poll(timeoutMillis: Long): Option[Cmd[Msg]] =
    Option(queue.poll(timeoutMillis, TimeUnit.MILLISECONDS))
  override def registerSub(sub: Sub[Msg]): Sub[Msg]   = { subscriptions.add(sub); sub }
  override def cancelAllSubscriptions(): Unit =
    subscriptions.forEach { sub =>
      try sub.cancel()
      catch {
        case _: Throwable => ()
      }
    }

object TuiRuntime:

  given ExecutionContext = ExecutionContext.global
  private val TargetFps                    = 60
  private val FrameNanos                   = 1_000_000_000L / TargetFps
  private val MaxCoalescedCommandsPerFrame = 4096

  private[tui] def unexpectedMessage(e: Throwable): String =
    Option(e.getMessage).filter(_.trim.nonEmpty).getOrElse(e.getClass.getSimpleName)

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
  ): Unit =

    val bus: CmdBus[Msg] = new LocalCmdBus[Msg](terminalBackend)
    val restored         = new AtomicBoolean(false)

    def restoreTerminalState(): Unit =
      if restored.compareAndSet(false, true) then
        print(ANSI.showCursor)
        EnterNormalBuffer()
        Console.out.flush()
        // Close backend after restoring cursor/buffer state.
        terminalBackend.close()

    val shutdownHook = new Thread(
      () => restoreTerminalState(),
      "termflow-shutdown-hook"
    )

    // Enter alternate buffer and set up terminal
    EnterAlternateBuffer()
    ClearScreen()
    print(ANSI.showCursor)
    Console.out.flush()
    Runtime.getRuntime.addShutdownHook(shutdownHook)

    try
      // Build initial model and command using the provided runtime context
      val initial: Tui[Model, Msg] = app.init(bus)
      bus.publish(initial.cmd)

      var model: Model                    = initial.model
      var pendingErr: Option[TermFlowError] = None
      var shouldRender                    = false
      var shouldExit                      = false
      var lastRenderAtNanos               = 0L

      def processCommand(cmd: Cmd[Msg]): Unit =
        cmd match
          case Cmd.Exit =>
            shouldExit = true

          case Cmd.TermFlowErrorCmd(err) =>
            pendingErr = Some(err)
            shouldRender = true

          case Cmd.NoCmd =>
            shouldRender = true

          case Cmd.GCmd(g) =>
            val next: Tui[Model, Msg] = app.update(model, g, bus)
            model = next.model
            bus.publish(next.cmd)

          case Cmd.FCmd(task, toCmd, onEnqueue) =>
            task.onComplete:
              case Success(result) =>
                bus.publish(toCmd(result))
              case Failure(e) =>
                bus.publish(Cmd.TermFlowErrorCmd(TermFlowError.Unexpected(unexpectedMessage(e), Some(e))))
            onEnqueue match
              case Some(msg) => bus.publish(Cmd.GCmd(msg))
              case None      => bus.publish(Cmd.NoCmd)

      while !shouldExit do
        val cmd = bus.take()
        processCommand(cmd)

        if shouldRender && !shouldExit then
          // Drain already-queued work so multiple rapid updates collapse into one frame.
          var drained  = true
          var consumed = 0
          while drained && !shouldExit && consumed < MaxCoalescedCommandsPerFrame do
            bus.poll(0L) match
              case Some(nextCmd) =>
                processCommand(nextCmd)
                consumed += 1
              case None          => drained = false

          val elapsed = System.nanoTime() - lastRenderAtNanos
          val waitNanos =
            if lastRenderAtNanos == 0L then 0L
            else math.max(0L, FrameNanos - elapsed)

          if waitNanos > 0L then
            val deadline = System.nanoTime() + waitNanos
            while !shouldExit && System.nanoTime() < deadline do
              val remainingNanos = deadline - System.nanoTime()
              val timeoutMillis  = math.max(1L, remainingNanos / 1_000_000L)
              bus.poll(timeoutMillis) match
                case Some(nextCmd) => processCommand(nextCmd)
                case None          => ()

          if !shouldExit then
            renderer.render(app.view(model), pendingErr)
            pendingErr = None
            shouldRender = false
            lastRenderAtNanos = System.nanoTime()
    finally
      // Cancel all registered subscriptions to stop background threads
      bus.cancelAllSubscriptions()
      try Runtime.getRuntime.removeShutdownHook(shutdownHook)
      catch {
        case _: IllegalStateException => ()
      }
      // Always restore terminal state, even on crash.
      restoreTerminalState()
