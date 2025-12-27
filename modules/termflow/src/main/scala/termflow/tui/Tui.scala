package termflow.tui

import termflow.tui.TuiPrelude._
import scala.concurrent.Future
import scala.language.implicitConversions

/** Error ADT used by TermFlow. */
sealed trait TermFlowError

object TermFlowError {
  final case class ConfigError(msg: String)    extends TermFlowError
  case object ModelNotFound                    extends TermFlowError
  final case class Unexpected(msg: String)     extends TermFlowError
  final case class Validation(msg: String)     extends TermFlowError
  final case class CommandError(input: String) extends TermFlowError
  final case class UnknownApp(name: String)    extends TermFlowError
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

trait TuiApp[Model, Msg] {
  def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg]
  def update(model: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg]
  def view(model: Model): RootNode
  def toMsg(input: PromptLine): Result[Msg]
}
