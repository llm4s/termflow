package termflow.tui

import termflow.tui.TuiPrelude._
import scala.concurrent.Future
import scala.language.implicitConversions

/** Error ADT used by TermFlow. */
sealed trait TermFlowError

object TermFlowError {
  final case class ConfigError(msg: String)                                 extends TermFlowError
  case object ModelNotFound                                                 extends TermFlowError
  final case class Unexpected(msg: String, cause: Option[Throwable] = None) extends TermFlowError
  final case class Validation(msg: String)                                  extends TermFlowError
  final case class CommandError(input: String)                              extends TermFlowError
  final case class UnknownApp(name: String)                                 extends TermFlowError
}

final case class Tui[Model, Msg](
  model: Model,
  cmd: Cmd[Msg] = Cmd.NoCmd
)

object Tui {

  /** Syntax helpers for lifting a model into a `Tui`. */
  implicit final class TuiOps[Model](private val m: Model) extends AnyVal {
    def tui[Msg]: Tui[Model, Msg]            = Tui(m)
    def gCmd[Msg](msg: Msg): Tui[Model, Msg] = Tui(m, Cmd.GCmd(msg))
  }

  /** Implicit conversion to treat a bare model as `Tui(model, NoCmd)`. */
  implicit def modelToTui[Model, Msg](m: Model): Tui[Model, Msg] =
    Tui(m, Cmd.NoCmd)
}

sealed trait Cmd[+Msg]

object Cmd {
  case object NoCmd extends Cmd[Nothing]
  case object Exit  extends Cmd[Nothing]

  final case class GCmd[Msg](msg: Msg) extends Cmd[Msg]

  final case class FCmd[A, Msg](
    task: Future[A],
    toCMD: A => Cmd[Msg],
    onEnqueue: Option[Msg] = None
  ) extends Cmd[Msg]

  final case class TermFlowErrorCmd[Msg](msg: TermFlowError) extends Cmd[Msg]
}

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
trait TuiApp[Model, Msg] {

  /** Create the initial model and optional startup command. */
  def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg]

  /** Handle a message and produce a new model state with optional command. */
  def update(model: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg]

  /** Render the model to a virtual DOM tree for display. */
  def view(model: Model): RootNode

  /** Parse user input into a message. */
  def toMsg(input: PromptLine): Result[Msg]
}
