package termflow.apps.echo

import termflow.tui._
import termflow.tui.TuiPrelude._
import termflow.tui.Color._

object EchoApp {

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  // === Model ===
  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    messages: List[String],
    maxWidth: Int,
    input: Sub[Msg],
    prompt: Prompt.State
  )

  sealed trait Msg
  object Msg {
    final case class AddMessage(input: String)                 extends Msg
    case object Clear                                          extends Msg
    case object Exit                                           extends Msg
    final case class ConsoleInputKey(key: KeyDecoder.InputKey) extends Msg
    final case class ConsoleInputError(error: Throwable)       extends Msg
  }

  import Msg._

  // === App ===
  object App extends TuiApp[Model, Msg] {

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        messages = List.empty,
        // Leave a small margin on both sides for the echo box.
        maxWidth = math.max(40, ctx.terminal.width - 10),
        input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        prompt = Prompt.State()
      )

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match {
        case AddMessage(input) =>
          // Wrap long input into lines that fit in the box
          val wrappedLines = wrapText(input, m.maxWidth - 4)
          val newMsgs      = m.messages ++ wrappedLines
          m.copy(messages = newMsgs.takeRight(30))

        case Clear =>
          m.copy(messages = Nil)

        case Exit =>
          Tui(m, Cmd.Exit)

        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](m.prompt, k)(toMsg)
          maybeCmd match {
            case Some(cmd) => Tui(m.copy(prompt = nextPrompt), cmd)
            case None      => m.copy(prompt = nextPrompt)
          }

        case ConsoleInputError(_) =>
          m // ignore for now
      }

    override def view(m: Model): RootNode = {
      val boxTop         = 1
      val messagesHeight = math.max(8, math.min(20, m.messages.length + 4))
      val boxHeight      = messagesHeight
      val prefix         = "[]> "
      val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)

      RootNode(
        width = m.terminalWidth,
        height = boxHeight + 8,
        children = List(
          BoxNode(1.x, boxTop.y, m.maxWidth + 2, boxHeight, children = Nil, style = Style(border = true, fg = Blue))
        ) ++
          m.messages.takeRight(messagesHeight - 2).zipWithIndex.map { case (msg, i) =>
            TextNode(3.x, (boxTop + 1 + i).y, List(msg.text))
          } ++
          List(
            TextNode(2.x, (boxTop + boxHeight).y, List("──────────────────────────────".text(fg = Cyan))),
            TextNode(2.x, (boxTop + boxHeight + 1).y, List("Commands:".text(fg = Yellow))),
            TextNode(2.x, (boxTop + boxHeight + 2).y, List("  /clear → clear chat".text)),
            TextNode(2.x, (boxTop + boxHeight + 3).y, List("  exit   → quit".text))
          ),
        input = Some(
          InputNode(
            2.x,
            (boxTop + boxHeight + 5).y,
            prompt = renderedPrompt.text,
            style = Style(fg = Green),
            cursor = renderedPrompt.cursorIndex
          )
        )
      )
    }

    override def toMsg(input: PromptLine): Result[Msg] =
      input.trim match {
        case ""       => Left(termflow.tui.TermFlowError.Validation("Empty input"))
        case "/clear" => Right(Clear)
        case "exit"   => Right(Exit)
        case other    => Right(AddMessage(other))
      }

    // === Helpers ===
    private def wrapText(text: String, maxWidth: Int): List[String] =
      text.split("\n").toList.flatMap { line =>
        if (line.length <= maxWidth) List(line)
        else line.grouped(maxWidth).toList
      }
  }
}
