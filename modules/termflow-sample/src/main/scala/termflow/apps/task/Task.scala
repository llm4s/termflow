package termflow.apps.task

import termflow.tui._
import termflow.tui.TuiPrelude._
import termflow.tui.Tui.*

object Task {
  type TaskId = String

  enum TaskStatus {
    case Pending, InProgress, Done, Cancelled
  }

  final case class Task(id: TaskId, status: TaskStatus)

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    tasks: Map[TaskId, Task],
    filteredList: List[Task],
    renderList: RenderMode,
    input: Sub[Msg],
    prompt: Prompt.State
  )

  enum RenderMode {
    case Init, Add, All, InProgress, Cancelled, Done, Count
    case AppErrorMsg(erroMsg: String)
  }

  sealed trait Msg
  object Msg {
    final case class Add(id: TaskId)                           extends Msg
    final case class Remove(id: TaskId)                        extends Msg
    final case class MarkInProgress(id: TaskId)                extends Msg
    final case class MarkDone(id: TaskId)                      extends Msg
    final case class MarkCancel(id: TaskId)                    extends Msg
    case object ListAll                                        extends Msg
    case object ListInProgress                                 extends Msg
    case object ListDone                                       extends Msg
    case object ListCancelled                                  extends Msg
    final case class InvalidCmd(msg: String)                   extends Msg
    final case class ConsoleInputKey(key: KeyDecoder.InputKey) extends Msg
    final case class ConsoleInputError(error: Throwable)       extends Msg
  }

  import Msg._

  object App extends TuiApp[Model, Msg] {
    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        tasks = Map.empty,
        filteredList = List.empty,
        renderList = RenderMode.Init,
        input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        prompt = Prompt.State()
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match {
        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](m.prompt, k)(toMsg)
          maybeCmd match {
            case Some(cmd) => Tui(m.copy(prompt = nextPrompt), cmd)
            case None      => m.copy(prompt = nextPrompt).tui
          }
        case ConsoleInputError(e) =>
          // Surface the decoding error as InvalidCmd
          m.copy(renderList = RenderMode.AppErrorMsg(s"Console Input Error: ${e.getMessage}")).tui
        case other =>
          UpdateApp(m, other)
      }

    override def view(m: Model): RootNode =
      RenderApp(m)

    override def toMsg(input: PromptLine): Result[Msg] =
      GetMsg(input)
  }
}
