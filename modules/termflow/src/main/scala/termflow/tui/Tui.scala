package termflow.tui

import termflow.tui.TuiPrelude._

import scala.concurrent.Future

/** Error ADT used by TermFlow. */
enum TermFlowError:
  case ConfigError(msg: String)
  case ModelNotFound
  case Unexpected(msg: String, cause: Option[Throwable] = None)
  case Validation(msg: String)
  case CommandError(input: String)
  case UnknownApp(name: String)

final case class Tui[Model, Msg](
  model: Model,
  cmd: Cmd[Msg] = Cmd.NoCmd
)

object Tui:

  /** Syntax helpers for lifting a model into a `Tui`. */
  extension [Model](m: Model)
    def tui[Msg]: Tui[Model, Msg]            = Tui(m)
    def gCmd[Msg](msg: Msg): Tui[Model, Msg] = Tui(m, Cmd.GCmd(msg))

enum Cmd[+Msg]:
  case NoCmd extends Cmd[Nothing]
  case Exit  extends Cmd[Nothing]

  case GCmd[Msg](msg: Msg) extends Cmd[Msg]

  case FCmd[A, Msg](
    task: Future[A],
    toCMD: A => Cmd[Msg],
    onEnqueue: Option[Msg] = None
  ) extends Cmd[Msg]

  case TermFlowErrorCmd[Msg](msg: TermFlowError) extends Cmd[Msg]

/**
 * Main trait for TermFlow applications following the Elm architecture.
 *
 * Applications define four core functions:
 *  - `init`: Create initial model and optional startup command
 *  - `update`: Handle messages and produce new model state
 *  - `view`: Render model to a virtual DOM tree
 *  - `toMsg`: Parse user input into messages
 *
 * @tparam Model The application state type
 * @tparam Msg The message/event type handled by the application
 */
trait TuiApp[Model, Msg]:

  /** Create the initial model and optional startup command. */
  def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg]

  /** Handle a message and produce a new model state with optional command. */
  def update(model: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg]

  /** Render the model to a virtual DOM tree for display. */
  def view(model: Model): RootNode

  /** Parse user input into a message. */
  def toMsg(input: PromptLine): Result[Msg]
