package termflow.apps.input

import termflow.tui.*
import termflow.tui.Color.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

object InputLineReproApp:

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    prompt: Prompt.State,
    lastSubmitted: String
  )

  enum Msg:
    case Submitted(value: String)
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)

  import Msg.*

  object App extends TuiApp[Model, Msg]:
    private val ViewportWidth = 6

    private def syncTerminalSize(model: Model, ctx: RuntimeCtx[Msg]): Model =
      val width  = ctx.terminal.width
      val height = ctx.terminal.height
      if width == model.terminalWidth && height == model.terminalHeight then model
      else model.copy(terminalWidth = width, terminalHeight = height)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Sub.InputKey(ConsoleInputKey.apply, ConsoleInputError.apply, ctx)
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        prompt = Prompt.State(),
        lastSubmitted = ""
      ).tui

    override def update(model: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncTerminalSize(model, ctx)
      msg match
        case Submitted(value) =>
          sized.copy(lastSubmitted = value).tui

        case ConsoleInputKey(key) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](sized.prompt, key)(toMsg)
          maybeCmd match
            case Some(cmd) => Tui(sized.copy(prompt = nextPrompt), cmd)
            case None      => sized.copy(prompt = nextPrompt).tui

        case ConsoleInputError(error) =>
          sized.copy(lastSubmitted = s"input error: ${Option(error.getMessage).getOrElse("unknown")}").tui

    override def view(model: Model): RootNode =
      val text   = Prompt.render(model.prompt)
      val cursor = math.max(0, math.min(model.prompt.cursor, text.length))

      RootNode(
        width = math.max(model.terminalWidth, 40),
        height = math.max(model.terminalHeight, 12),
        children = List(
          TextNode(2.x, 1.y, List(Text("Input Line Repro", Style(fg = Yellow, bold = true, underline = true)))),
          TextNode(2.x, 3.y, List(Text("Viewport width is fixed at 6 chars.", Style(fg = Cyan)))),
          TextNode(2.x, 4.y, List(Text("Type abcdefghi then press Left three times. Use Ctrl+C to exit.", Style()))),
          TextNode(2.x, 6.y, List(Text(s"Buffer : '$text'", Style(fg = Green)))),
          TextNode(2.x, 7.y, List(Text(s"Cursor : $cursor / ${text.length}", Style(fg = Green)))),
          TextNode(2.x, 8.y, List(Text(s"Submit : '${model.lastSubmitted}'", Style(fg = Magenta)))),
          TextNode(2.x, 10.y, List(Text("Input  :", Style(fg = Yellow))))
        ),
        input = Some(
          InputNode(
            11.x,
            10.y,
            prompt = text,
            style = Style(fg = White),
            cursor = cursor,
            lineWidth = ViewportWidth
          )
        )
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      Right(Submitted(input.value))
