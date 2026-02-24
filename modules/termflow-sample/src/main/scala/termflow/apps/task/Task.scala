package termflow.apps.task

import termflow.tui.Tui._
import termflow.tui.TuiPrelude._
import termflow.tui._

object Task {
  opaque type TaskId = String
  object TaskId {
    def apply(value: String): TaskId = value

    extension (id: TaskId) {
      def value: String = id
    }
  }

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
    case AppErrorMsg(errorMsg: String)
  }

  enum Msg {
    case Add(id: TaskId)
    case Remove(id: TaskId)
    case MarkInProgress(id: TaskId)
    case MarkDone(id: TaskId)
    case MarkCancel(id: TaskId)
    case ListAll
    case ListInProgress
    case ListDone
    case ListCancelled
    case InvalidCmd(msg: String)
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)
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
      GetMsg(input.value)
  }
}
