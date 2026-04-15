package termflow.testkit

import termflow.tui.AnsiRenderer
import termflow.tui.AnsiRenderer.RenderFrame
import termflow.tui.Cmd
import termflow.tui.TermFlowError
import termflow.tui.TuiApp

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.util.Failure
import scala.util.Success

/**
 * Drives a `TuiApp` synchronously for tests, without spawning the real
 * `TuiRuntime` loop.
 *
 * The driver calls `app.init`, `app.update`, and `app.view` directly, then
 * renders each resulting `RootNode` through `AnsiRenderer.buildFrame` to
 * produce a cell-matrix `RenderFrame` that can be compared against golden
 * snapshots.
 *
 * Cmd semantics roughly mirror `TuiRuntime.processCommand`
 * (`TuiRuntime.scala:150-175`), but `FCmd` tasks must already be completed
 * (`Future.successful` or equivalent) — the driver does NOT block. A
 * non-completed future raises an `IllegalStateException` with guidance.
 *
 * Subscriptions are intentionally never started; `Sub.Every`, `Sub.InputKey`,
 * and `Sub.TerminalResize` timers and reader threads stay dormant so tests
 * are fully deterministic. Tests that want to simulate keyboard input should
 * construct the wrapping `Msg` directly (e.g. `SyncCounter.Msg.ConsoleInputKey`)
 * and call `send`.
 */
final class TuiTestDriver[Model, Msg](
  app: TuiApp[Model, Msg],
  val ctx: TestRuntimeCtx[Msg]
):

  private var _model: Model                              = uninitialized
  private var _initialized: Boolean                      = false
  private var _exited: Boolean                           = false
  private val observed: mutable.ArrayBuffer[Cmd[Msg]]    = mutable.ArrayBuffer.empty
  private val errors: mutable.ArrayBuffer[TermFlowError] = mutable.ArrayBuffer.empty

  /** Current model. Throws if `init` hasn't been called yet. */
  def model: Model =
    if !_initialized then throw new IllegalStateException("TuiTestDriver.model accessed before init()")
    _model

  /** `true` after a `Cmd.Exit` has been observed. */
  def exited: Boolean = _exited

  /** Every `Cmd` the driver processed since construction, in order. */
  def cmds: List[Cmd[Msg]] = observed.toList

  /** Every `TermFlowError` surfaced via `Cmd.TermFlowErrorCmd` since construction. */
  def observedErrors: List[TermFlowError] = errors.toList

  /** Render the current model to a frame. Throws if `init` hasn't been called. */
  def frame: RenderFrame =
    AnsiRenderer.buildFrame(app.view(model))

  /** Call `app.init(ctx)`, store the initial model, and apply the startup `Cmd`. */
  def init(): Unit =
    if _initialized then throw new IllegalStateException("TuiTestDriver.init() called twice")
    val initial = app.init(ctx)
    _model = initial.model
    _initialized = true
    applyCmd(initial.cmd)
    drainCtxQueue()

  /**
   * Dispatch a message through `app.update` and apply the resulting `Cmd`.
   *
   * Any commands the app or its (neutered) subscriptions published via
   * `ctx.publish` during `update` are drained and applied in order after the
   * direct result, mirroring the runtime loop's ordering.
   */
  def send(msg: Msg): Unit =
    if !_initialized then throw new IllegalStateException("TuiTestDriver.send() called before init()")
    if _exited then throw new IllegalStateException("TuiTestDriver.send() called after Cmd.Exit")
    val next = app.update(_model, msg, ctx)
    _model = next.model
    applyCmd(next.cmd)
    drainCtxQueue()

  private def drainCtxQueue(): Unit =
    var drained = ctx.drainCmds()
    while drained.nonEmpty do
      drained.foreach(applyCmd)
      drained = ctx.drainCmds()

  private def applyCmd(cmd: Cmd[Msg]): Unit =
    observed += cmd
    cmd match
      case Cmd.NoCmd     => ()
      case Cmd.Exit      => _exited = true
      case Cmd.GCmd(msg) => send(msg)
      case Cmd.TermFlowErrorCmd(e) =>
        errors += e
      case fc: Cmd.FCmd[a, ?] =>
        // Mirror runtime: onEnqueue fires immediately (synchronously), then
        // resolve the future eagerly. Tests must supply Future.successful.
        fc.onEnqueue.foreach(m => applyCmd(Cmd.GCmd(m).asInstanceOf[Cmd[Msg]]))
        fc.task.value match
          case Some(Success(v)) =>
            applyCmd(fc.toCmd(v).asInstanceOf[Cmd[Msg]])
          case Some(Failure(e)) =>
            errors += TermFlowError.Unexpected(
              Option(e.getMessage).getOrElse(e.getClass.getSimpleName),
              Some(e)
            )
          case None =>
            throw new IllegalStateException(
              "TuiTestDriver received Cmd.FCmd with a non-completed Future. " +
                "Snapshot tests must use Future.successful or pre-resolved futures."
            )

object TuiTestDriver:

  /** Build a driver with a fresh `TestRuntimeCtx` of the given terminal dimensions. */
  def apply[Model, Msg](app: TuiApp[Model, Msg], width: Int, height: Int): TuiTestDriver[Model, Msg] =
    new TuiTestDriver[Model, Msg](app, TestRuntimeCtx[Msg](width, height))
