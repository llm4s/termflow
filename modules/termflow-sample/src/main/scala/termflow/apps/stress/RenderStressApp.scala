package termflow.apps.stress

import termflow.tui.Color
import termflow.tui.Tui._
import termflow.tui.TuiPrelude._
import termflow.tui._

object RenderStressApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    frame: Int,
    running: Boolean,
    border: Boolean,
    narrowMode: Boolean,
    note: String,
    ticker: Sub[Msg],
    input: Sub[Msg],
    prompt: Prompt.State
  )

  enum Msg:
    case Tick
    case Start
    case Stop
    case ToggleBorder
    case ToggleNarrow
    case ClearNote
    case AddNote(text: String)
    case Exit
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:
    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        frame = 0,
        running = true,
        border = true,
        narrowMode = false,
        note = "",
        ticker = Sub.Every(50, () => Tick, ctx),
        input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        prompt = Prompt.State()
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Tick =>
          if m.running then m.copy(frame = m.frame + 1).tui
          else m.tui

        case Start =>
          if m.running then m.copy(note = "already running").tui
          else
            val ticker = if m.ticker.isActive then m.ticker else Sub.Every(50, () => Tick, ctx)
            m.copy(running = true, ticker = ticker, note = "running").tui

        case Stop =>
          if m.ticker.isActive then m.ticker.cancel()
          m.copy(running = false, ticker = Sub.NoSub, note = "stopped").tui

        case ToggleBorder =>
          m.copy(border = !m.border, note = s"border=${!m.border}").tui

        case ToggleNarrow =>
          m.copy(narrowMode = !m.narrowMode, note = s"narrow=${!m.narrowMode}").tui

        case ClearNote =>
          m.copy(note = "").tui

        case AddNote(text) =>
          m.copy(note = text).tui

        case Exit =>
          if m.ticker.isActive then m.ticker.cancel()
          Tui(m, Cmd.Exit)

        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](m.prompt, k)(toMsg)
          maybeCmd match
            case Some(cmd) => Tui(m.copy(prompt = nextPrompt), cmd)
            case None      => m.copy(prompt = nextPrompt).tui

        case ConsoleInputError(e) =>
          m.copy(note = s"input error: ${Option(e.getMessage).getOrElse("unknown")}").tui

    override def view(m: Model): RootNode =
      val width        = math.max(24, m.terminalWidth)
      val height       = math.max(14, m.terminalHeight)
      val boxHeight    = height - 2
      val leftX        = if m.border then 2 else 1
      val rightX       = if m.border then width - 1 else width
      val contentWidth = math.max(1, rightX - leftX + 1)

      val headerY       = if m.border then 2 else 1
      val separatorY    = headerY + 1
      val commandsStart = boxHeight - 3
      val dataStartY    = separatorY + 1
      val dataEndY      = commandsStart - 1
      val dataRows      = math.max(1, dataEndY - dataStartY + 1)
      val inputY        = boxHeight + 1

      val prefix         = "[]> "
      val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)

      def fitPad(raw: String): String =
        val clipped = if raw.length <= contentWidth then raw else raw.take(contentWidth)
        if clipped.length < contentWidth then clipped + (" " * (contentWidth - clipped.length)) else clipped

      val phase      = m.frame % 4
      val frames     = Array("|", "/", "-", "\\")
      val oscillate  = frames(phase)
      val waveLength = if m.narrowMode then math.max(1, contentWidth / 4) else math.max(1, contentWidth / 2)

      def dynamicLine(i: Int): String =
        val meter = (m.frame + i * 3) % (waveLength + 1)
        val wave  = ("#" * meter) + ("." * math.max(0, waveLength - meter))
        val base  = f"row=${i + 1}%02d frame=${m.frame}%06d $oscillate"
        if (i + m.frame) % 5 == 0 then s"$base  $wave"
        else if (i + m.frame) % 7 == 0 then s"$base  tail=${"x" * math.min(contentWidth, 12)}"
        else base

      val rows = (0 until dataRows).toList.map(i => fitPad(dynamicLine(i)))

      val header =
        fitPad(
          s"Render Stress | fps~20 | running=${m.running} border=${m.border} narrow=${m.narrowMode} ${
              if m.note.nonEmpty then s"note=${m.note}" else ""
            }"
        )

      val separator = "-" * contentWidth

      val command1 = fitPad("Commands: start | stop | border | narrow")
      val command2 = fitPad("          clear | note <text> | exit")
      val command3 = fitPad("Goal: stress diff/clear/cursor behavior")

      val styleA = Style(fg = Color.Cyan)
      val styleB = Style(fg = Color.Green)

      val children: List[VNode] =
        (if m.border then
           List(BoxNode(1.x, 1.y, width, boxHeight, children = Nil, style = Style(border = true, fg = Color.Blue)))
         else Nil) ++
          List(TextNode(leftX.x, headerY.y, List(Text(header, Style(fg = Color.Yellow, bold = true))))) ++
          List(TextNode(leftX.x, separatorY.y, List(Text(separator, Style(fg = Color.Magenta))))) ++
          rows.zipWithIndex.map { case (line, idx) =>
            val style = if idx % 2 == 0 then styleA else styleB
            TextNode(leftX.x, (dataStartY + idx).y, List(Text(line, style)))
          } ++
          List(
            TextNode(leftX.x, commandsStart.y, List(Text(command1, Style(fg = Color.White)))),
            TextNode(leftX.x, (commandsStart + 1).y, List(Text(command2, Style(fg = Color.White)))),
            TextNode(leftX.x, (commandsStart + 2).y, List(Text(command3, Style(fg = Color.Yellow))))
          )

      RootNode(
        width = width,
        height = height,
        children = children,
        input = Some(
          InputNode(
            leftX.x,
            inputY.y,
            renderedPrompt.text,
            Style(),
            cursor = renderedPrompt.cursorIndex,
            lineWidth = contentWidth
          )
        )
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      input.value.trim match
        case ""                                 => Right(ClearNote)
        case "start"                            => Right(Start)
        case "stop"                             => Right(Stop)
        case "border"                           => Right(ToggleBorder)
        case "narrow"                           => Right(ToggleNarrow)
        case "clear"                            => Right(ClearNote)
        case "exit"                             => Right(Exit)
        case other if other.startsWith("note ") => Right(AddNote(other.stripPrefix("note ").trim))
        case other                              => Right(AddNote(s"unknown command: $other"))
