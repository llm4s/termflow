package termflow.tui

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.ExecutionContext
import scala.util.Failure
import scala.util.Success

/**
 * Pluggable renderer invoked by the runtime loop once per frame.
 *
 * The default implementation is `SimpleANSIRenderer` which delegates to
 * `AnsiRenderer.buildFrame` + `AnsiRenderer.diff`. Custom renderers can be
 * supplied to `TuiRuntime.run` for backends that emit something other than
 * raw ANSI (e.g. test capture, alternate terminals).
 *
 * @note This is an SPI. Source-compatible changes are not guaranteed;
 *       downstream implementers should expect the trait to evolve.
 */
trait TuiRenderer:

  /**
   * Render `textNode` onto `terminal`, optionally overlaying `err` as an
   * error banner. Called at most once per frame by the runtime loop after
   * coalescing queued commands.
   *
   * @param textNode The virtual DOM root produced by `TuiApp.view`.
   * @param err A pending error to surface (cleared by the runtime after this call).
   * @param terminal The backend to write ANSI output to.
   * @param renderMetrics Metrics sink for diff size / coalescing counters.
   */
  def render(
    textNode: RootNode,
    err: Option[TermFlowError],
    terminal: TerminalBackend,
    renderMetrics: RenderMetrics
  ): Unit

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

  /** Access the resolved runtime configuration. */
  def config: TermFlowConfig

  /**
   * Register a subscription for automatic cleanup on exit.
   * Returns the subscription for chaining.
   */
  def registerSub(sub: Sub[Msg]): Sub[Msg]

/**
 * Read side of the command bus consumed by the runtime loop.
 *
 * @note SPI. Applications should interact with commands through [[Cmd]] and
 *       [[RuntimeCtx]], not this trait.
 */
trait CmdConsumer[Msg]:

  /** Block until a command is available and return it. */
  def take(): Cmd[Msg]

  /**
   * Wait up to `timeoutMillis` for a command. Returns `None` on timeout.
   * Used by the runtime to coalesce queued commands within a frame budget.
   */
  def poll(timeoutMillis: Long): Option[Cmd[Msg]]

/**
 * Bidirectional command bus: producers publish, the runtime consumes.
 *
 * @note SPI. Custom implementations are only useful when replacing
 *       `TuiRuntime.run` wholesale (e.g. for a bespoke test harness).
 */
trait CmdBus[Msg] extends RuntimeCtx[Msg] with CmdConsumer[Msg]:

  /** Cancel every registered subscription, swallowing individual errors. */
  def cancelAllSubscriptions(): Unit

/**
 * Default in-memory [[CmdBus]] backed by a `LinkedBlockingQueue`.
 *
 * Thread-safe: multiple subscription threads can call `publish` concurrently
 * while the runtime loop drains via `take` / `poll`.
 */
final class LocalCmdBus[Msg](val terminal: TerminalBackend, val config: TermFlowConfig) extends CmdBus[Msg]:
  private val queue                                   = new LinkedBlockingQueue[Cmd[Msg]]()
  private val subscriptions: java.util.List[Sub[Msg]] = new java.util.concurrent.CopyOnWriteArrayList[Sub[Msg]]()
  override def publish(cmd: Cmd[Msg]): Unit           = queue.put(cmd)
  override def take(): Cmd[Msg]                       = queue.take()
  override def poll(timeoutMillis: Long): Option[Cmd[Msg]] =
    Option(queue.poll(timeoutMillis, TimeUnit.MILLISECONDS))
  override def registerSub(sub: Sub[Msg]): Sub[Msg] = {
    subscriptions.add(sub): Unit
    // Start any deferred-start machinery (e.g. Sub.Every's scheduler).
    // Eagerly-started subs override `start` as a no-op, so this is safe
    // for every existing implementation.
    sub.start()
    sub
  }
  override def cancelAllSubscriptions(): Unit =
    subscriptions.forEach { sub =>
      try sub.cancel()
      catch {
        case _: Throwable => ()
      }
    }

object TuiRuntime:

  given ExecutionContext                   = ExecutionContext.global
  private val TargetFps                    = 60
  private val FrameNanos                   = 1_000_000_000L / TargetFps
  private val MaxCoalescedCommandsPerFrame = 4096

  private[tui] def unexpectedMessage(e: Throwable): String =
    Option(e.getMessage).filter(_.trim.nonEmpty).getOrElse(e.getClass.getSimpleName)

  /**
   * Run a TUI application, loading configuration from the environment.
   *
   * This is the entry point most applications use: it loads
   * [[TermFlowConfig]], delegates to the lower-level
   * [[TuiRuntime.run[Model,Msg](app,renderer,terminalBackend,config)* overload]],
   * and writes a diagnostic message if config loading fails.
   *
   * The runtime:
   *   1. enters the terminal alternate buffer,
   *   2. installs a JVM shutdown hook that restores cursor/alt-buffer state,
   *   3. drives an event loop that coalesces queued commands at 60 FPS,
   *   4. renders at most one frame per command burst,
   *   5. tears down subscriptions and restores terminal state on exit.
   *
   * The loop runs on the caller's thread and only returns when the app
   * publishes [[Cmd.Exit]] or throws.
   *
   * @param app The application to run.
   * @param renderer The renderer to use. Defaults to `SimpleANSIRenderer`.
   * @param terminalBackend The terminal backend. Defaults to `JLineTerminalBackend`.
   */
  def run[Model, Msg](
    app: TuiApp[Model, Msg],
    renderer: TuiRenderer = SimpleANSIRenderer(),
    terminalBackend: TerminalBackend = new JLineTerminalBackend()
  ): Unit =
    TermFlowConfig.load() match
      case Success(config) =>
        run(
          app = app,
          renderer = renderer,
          terminalBackend = terminalBackend,
          config = config
        )
      case Failure(err) =>
        terminalBackend.write(s"TermFlow startup failed: ${unexpectedMessage(err)}${System.lineSeparator()}")
        terminalBackend.flush()
        terminalBackend.close()

  /**
   * Run a TUI application with an explicitly supplied [[TermFlowConfig]].
   *
   * Use this overload when you need to override the default config (e.g.
   * from tests or custom embedding). Otherwise prefer the
   * [[TuiRuntime.run[Model,Msg](app,renderer,terminalBackend)* three-argument overload]]
   * which loads config from the environment.
   *
   * See the three-argument overload for the loop semantics — this method
   * only differs in that it skips the `TermFlowConfig.load()` step.
   */
  def run[Model, Msg](
    app: TuiApp[Model, Msg],
    renderer: TuiRenderer,
    terminalBackend: TerminalBackend,
    config: TermFlowConfig
  ): Unit =
    val bus: CmdBus[Msg] = new LocalCmdBus[Msg](terminalBackend, config)
    val restored         = new AtomicBoolean(false)
    val frameworkLog     = FrameworkLog(config.logging)
    val renderMetrics    = new RenderMetrics(config.metrics, frameworkLog)

    def restoreTerminalState(): Unit =
      if restored.compareAndSet(false, true) then
        terminalBackend.write(ANSI.showCursor)
        terminalBackend.write(ANSI.exitAltBuffer)
        terminalBackend.flush()
        // Close backend after restoring cursor/buffer state.
        terminalBackend.close()

    val shutdownHook = new Thread(
      () => restoreTerminalState(),
      "termflow-shutdown-hook"
    )

    // Enter alternate buffer and set up terminal
    terminalBackend.write(ANSI.enterAltBuffer)
    terminalBackend.write(ANSI.clearScreen)
    terminalBackend.write(ANSI.showCursor)
    terminalBackend.flush()
    Runtime.getRuntime.addShutdownHook(shutdownHook)

    try
      // Build initial model and command using the provided runtime context
      val initial: Tui[Model, Msg] = app.init(bus)
      bus.publish(initial.cmd)

      var model: Model                      = initial.model
      var pendingErr: Option[TermFlowError] = None
      var shouldRender                      = false
      var shouldExit                        = false
      var lastRenderAtNanos                 = 0L

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
              case None => drained = false

          val elapsed = System.nanoTime() - lastRenderAtNanos
          val waitNanos =
            if lastRenderAtNanos == 0L then 0L
            else math.max(0L, FrameNanos - elapsed)

          if waitNanos > 0L then
            val deadline     = System.nanoTime() + waitNanos
            var waitConsumed = 0
            while !shouldExit && System.nanoTime() < deadline do
              val remainingNanos = deadline - System.nanoTime()
              val timeoutMillis  = math.max(1L, remainingNanos / 1_000_000L)
              bus.poll(timeoutMillis) match
                case Some(nextCmd) =>
                  processCommand(nextCmd)
                  waitConsumed += 1
                case None => ()
            renderMetrics.recordCoalescing(consumed + waitConsumed)
          else renderMetrics.recordCoalescing(consumed)

          if !shouldExit then
            renderer.render(app.view(model), pendingErr, terminalBackend, renderMetrics)
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
      renderMetrics.printSummary()
      // Always restore terminal state, even on crash.
      restoreTerminalState()
