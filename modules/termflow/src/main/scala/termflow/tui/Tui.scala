package termflow.tui

import termflow.tui.TuiPrelude.*

import scala.concurrent.Future

/**
 * Recoverable error surface for TermFlow applications and the runtime.
 *
 * Errors are raised through [[Cmd.TermFlowErrorCmd]] and rendered by the
 * runtime as a transient overlay above the next frame. They are informational
 * rather than fatal — the runtime keeps looping unless an [[Cmd.Exit]] is
 * also produced.
 */
enum TermFlowError:

  /** Configuration load / parse failure. */
  case ConfigError(msg: String)

  /** Raised when a command refers to a model that doesn't exist. */
  case ModelNotFound

  /**
   * Catch-all for unanticipated failures.
   *
   * @param msg A short human-readable description (shown in the UI).
   * @param cause The originating throwable, if any. Included for logs.
   */
  case Unexpected(msg: String, cause: Option[Throwable] = None)

  /** A user-supplied value failed domain validation. */
  case Validation(msg: String)

  /**
   * Raised by `TuiApp.toMsg` when the prompt line cannot be parsed into a
   * meaningful message.
   *
   * @param input The original prompt line as typed by the user.
   */
  case CommandError(input: String)

  /** The runtime received a request for an app whose name isn't registered. */
  case UnknownApp(name: String)

/**
 * The fundamental state-transition unit of a TermFlow application.
 *
 * A `Tui` bundles the new model with an optional follow-up [[Cmd]] to be
 * executed by the runtime. Both `init` and `update` return a `Tui`.
 *
 * @param model The application model after the transition.
 * @param cmd An effect to run next — often [[Cmd.NoCmd]] for pure updates.
 *
 * @tparam Model The application's model type.
 * @tparam Msg The application's message type.
 */
final case class Tui[Model, Msg](
  model: Model,
  cmd: Cmd[Msg] = Cmd.NoCmd
)

object Tui:

  /**
   * Ergonomic lifting syntax for producing `Tui` values from a bare model.
   *
   * {{{
   * // Pure update: no follow-up command.
   * model.copy(counter = counter + 1).tui
   *
   * // Update plus a follow-up message dispatch.
   * model.copy(flash = "done").gCmd(Msg.Dismiss)
   * }}}
   */
  extension [Model](m: Model)

    /** Lift `m` into a `Tui` with no follow-up command. */
    def tui[Msg]: Tui[Model, Msg] = Tui(m)

    /**
     * Lift `m` into a `Tui` and immediately dispatch `msg` as the next
     * message. Equivalent to `Tui(m, Cmd.GCmd(msg))`.
     */
    def gCmd[Msg](msg: Msg): Tui[Model, Msg] = Tui(m, Cmd.GCmd(msg))

/**
 * Effects that an application can return from `init` / `update` to instruct
 * the runtime to do something beyond updating the model.
 *
 * Commands are processed in order by the runtime loop. `GCmd` dispatches a
 * follow-up message through `update`, `FCmd` bridges an async `Future` into
 * the runtime, `Exit` ends the loop, and `TermFlowErrorCmd` surfaces a
 * recoverable error to the renderer.
 *
 * @tparam Msg The application's message type.
 */
enum Cmd[+Msg]:

  /** No effect — the most common case for pure state transitions. */
  case NoCmd extends Cmd[Nothing]

  /**
   * Cleanly terminate the runtime loop.
   *
   * The runtime restores terminal state (cursor, alt buffer) before
   * returning from `TuiRuntime.run`.
   */
  case Exit extends Cmd[Nothing]

  /**
   * Dispatch a follow-up message through `update`.
   *
   * Useful for chaining state transitions from a single `update` call, or
   * for self-dispatching initialisation work from `init`.
   */
  case GCmd(msg: Msg) extends Cmd[Msg]

  /**
   * Run an asynchronous task and feed its result back into the runtime as a
   * command when it completes.
   *
   * When the future succeeds, `toCmd(result)` is published to the command
   * bus. When it fails, the runtime publishes a [[TermFlowErrorCmd]] with an
   * [[TermFlowError.Unexpected]] describing the exception.
   *
   * @param task The asynchronous task to run.
   * @param toCmd Function producing the follow-up command from the task's result.
   * @param onEnqueue An optional message dispatched immediately when the
   *                  command is enqueued — useful for updating a "loading"
   *                  indicator before the future resolves.
   */
  case FCmd[A, M](
    task: Future[A],
    toCmd: A => Cmd[M],
    onEnqueue: Option[M] = None
  ) extends Cmd[M]

  /**
   * Surface a recoverable error to the renderer.
   *
   * The error is displayed on the next frame (typically as an overlay) and
   * cleared after one render cycle. The application continues running.
   */
  case TermFlowErrorCmd(msg: TermFlowError) extends Cmd[Msg]

/**
 * The Elm-style application contract: a model, an update function, a view,
 * and a way to turn prompt input into messages.
 *
 * A typical application is an `object` that extends `TuiApp` and lives next
 * to its `Model` case class and `Msg` enum:
 *
 * {{{
 * object Counter:
 *   final case class Model(count: Int)
 *   enum Msg:
 *     case Increment, Decrement, Quit
 *
 *   object App extends TuiApp[Model, Msg]:
 *     def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] = Model(0).tui
 *
 *     def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
 *       msg match
 *         case Msg.Increment => m.copy(count = m.count + 1).tui
 *         case Msg.Decrement => m.copy(count = m.count - 1).tui
 *         case Msg.Quit      => Tui(m, Cmd.Exit)
 *
 *     def view(m: Model): RootNode = /* … build the virtual DOM … */ ???
 *
 *     def toMsg(input: PromptLine): Result[Msg] = /* parse commands */ ???
 * }}}
 *
 * Run the app with `TuiRuntime.run(Counter.App)`. For testing without a real
 * terminal, drive it through `termflow.testkit.TuiTestDriver` instead.
 *
 * The architectural shape and rationale are documented in more depth in
 * `docs/DESIGN.md` and `docs/RENDER_PIPELINE.md`.
 *
 * @tparam Model The application state type.
 * @tparam Msg The message / event type handled by the application.
 */
trait TuiApp[Model, Msg]:

  /**
   * Produce the initial model and an optional startup command.
   *
   * Typical uses of the startup command are registering subscriptions
   * (`Sub.InputKey`, `Sub.Every`) and kicking off async work with
   * [[Cmd.FCmd]].
   */
  def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg]

  /**
   * Apply a message to the current model and return the next state plus an
   * optional follow-up command.
   *
   * Must be pure with respect to `model` and `msg` — the runtime may re-run
   * or coalesce updates during render pacing, and the test driver relies on
   * determinism.
   */
  def update(model: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg]

  /**
   * Render the current model to a virtual-DOM tree.
   *
   * The returned [[RootNode]] is diffed against the previous frame by
   * `AnsiRenderer` to produce a minimal ANSI patch. See
   * `docs/RENDER_PIPELINE.md` for the full pipeline.
   */
  def view(model: Model): RootNode

  /**
   * Parse a submitted prompt line into a message, or a [[TermFlowError]] if
   * the line isn't recognised.
   *
   * Validation errors are surfaced to the user via the error overlay;
   * returning `Left(TermFlowError.Validation(...))` is the idiomatic way to
   * reject a bad command without aborting the app.
   */
  def toMsg(input: PromptLine): Result[Msg]
