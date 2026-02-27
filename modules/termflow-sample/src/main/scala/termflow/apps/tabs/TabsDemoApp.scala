package termflow.apps.tabs

import termflow.tui.Color.Blue
import termflow.tui.Color.Cyan
import termflow.tui.Color.Green
import termflow.tui.Color.Magenta
import termflow.tui.Color.White
import termflow.tui.Color.Yellow
import termflow.tui.Tui._
import termflow.tui.TuiPrelude._
import termflow.tui._

object TabsDemoApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  final case class TabState(
    name: String,
    counter: Int,
    notes: List[String]
  )

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    tabs: Vector[TabState],
    activeTab: Int,
    status: String,
    input: Sub[Msg],
    prompt: Prompt.State
  )

  enum Msg:
    case NextTab
    case PrevTab
    case SelectTab(idx: Int)
    case Increment
    case Decrement
    case AddNote(note: String)
    case ClearNotes
    case Exit
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:
    private def syncTerminalSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.terminalWidth && h == m.terminalHeight then m
      else m.copy(terminalWidth = w, terminalHeight = h)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        tabs = Vector(
          TabState("Home", counter = 0, notes = Nil),
          TabState("Work", counter = 0, notes = Nil),
          TabState("Notes", counter = 0, notes = Nil)
        ),
        activeTab = 0,
        status = "Tab demo ready. Use next/prev/tab 1..3.",
        input = Sub.InputKey(key => ConsoleInputKey(key), throwable => ConsoleInputError(throwable), ctx),
        prompt = Prompt.State()
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncTerminalSize(m, ctx)

      def withActive(update: TabState => TabState, status: String): Tui[Model, Msg] =
        val i       = sized.activeTab
        val updated = sized.tabs.updated(i, update(sized.tabs(i)))
        sized.copy(tabs = updated, status = status).tui

      msg match
        case NextTab =>
          val next = (sized.activeTab + 1) % sized.tabs.length
          sized.copy(activeTab = next, status = s"Switched to ${sized.tabs(next).name}").tui

        case PrevTab =>
          val next = (sized.activeTab - 1 + sized.tabs.length) % sized.tabs.length
          sized.copy(activeTab = next, status = s"Switched to ${sized.tabs(next).name}").tui

        case SelectTab(idx) =>
          if idx >= 0 && idx < sized.tabs.length then
            sized.copy(activeTab = idx, status = s"Switched to ${sized.tabs(idx).name}").tui
          else sized.copy(status = s"Invalid tab index: ${idx + 1}").tui

        case Increment =>
          withActive(t => t.copy(counter = t.counter + 1), "Counter incremented.")

        case Decrement =>
          withActive(t => t.copy(counter = t.counter - 1), "Counter decremented.")

        case AddNote(note) =>
          if note.isEmpty then sized.copy(status = "Note cannot be empty.").tui
          else withActive(t => t.copy(notes = (note :: t.notes).take(8)), "Note saved in current tab.")

        case ClearNotes =>
          withActive(t => t.copy(notes = Nil), "Current tab notes cleared.")

        case Exit =>
          Tui(sized, Cmd.Exit)

        case ConsoleInputKey(k) =>
          val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](sized.prompt, k)(toMsg)
          maybeCmd match
            case Some(cmd) => Tui(sized.copy(prompt = nextPrompt), cmd)
            case None      => sized.copy(prompt = nextPrompt).tui

        case ConsoleInputError(e) =>
          sized.copy(status = s"input error: ${Option(e.getMessage).getOrElse("unknown")}").tui

    override def view(m: Model): RootNode =
      val prefix         = "[]> "
      val renderedPrompt = Prompt.renderWithPrefix(m.prompt, prefix)
      val active         = m.tabs(m.activeTab)
      val boxWidth       = math.max(2, m.terminalWidth - 4)
      val innerWidth     = math.max(1, boxWidth - 2)
      val boxHeight      = math.max(12, m.terminalHeight - 5)
      val notesStartY    = 6
      val notesRows      = math.max(1, boxHeight - 12)
      val notes          = active.notes.take(notesRows)

      val tabBar = m.tabs.zipWithIndex
        .map { case (tab, idx) =>
          if idx == m.activeTab then s"[${idx + 1}:${tab.name}]"
          else s" ${idx + 1}:${tab.name} "
        }
        .mkString(" ")
      val tabBarFit  = if tabBar.length <= innerWidth then tabBar else tabBar.take(innerWidth)
      val statusFit  = if m.status.length <= innerWidth then m.status else m.status.take(innerWidth)
      val headerText = s"Active: ${active.name} | counter=${active.counter}"
      val headerFit  = if headerText.length <= innerWidth then headerText else headerText.take(innerWidth)

      val children: List[VNode] = List(
        BoxNode(1.x, 1.y, boxWidth, boxHeight, children = Nil, style = Style(border = true, fg = Blue)),
        TextNode(2.x, 2.y, List(Text("Tabs Demo", Style(fg = Yellow, bold = true, underline = true)))),
        TextNode(2.x, 3.y, List(Text(tabBarFit, Style(fg = Magenta, bold = true)))),
        TextNode(2.x, 4.y, List(Text(headerFit, Style(fg = Cyan)))),
        TextNode(2.x, 5.y, List(Text("-" * innerWidth, Style(fg = Blue)))),
        TextNode(2.x, (notesStartY - 1).y, List(Text("Notes (per-tab persistent):", Style(fg = White))))
      ) ++
        (if notes.isEmpty then List(TextNode(2.x, notesStartY.y, List(Text("(none)", Style(fg = White)))))
         else
           notes.zipWithIndex.map { case (line, idx) =>
             val text = if line.length <= innerWidth then line else line.take(innerWidth)
             TextNode(2.x, (notesStartY + idx).y, List(Text(s"- $text", Style(fg = White))))
           }
        ) ++
        List(
          TextNode(2.x, (boxHeight - 4).y, List(Text("-" * innerWidth, Style(fg = Blue)))),
          TextNode(
            2.x,
            (boxHeight - 3).y,
            List(Text("Commands: next | prev | tab 1|2|3 | inc | dec", Style(fg = White)))
          ),
          TextNode(2.x, (boxHeight - 2).y, List(Text("          note <text> | clear | exit", Style(fg = White)))),
          TextNode(2.x, (boxHeight - 1).y, List(Text(statusFit, Style(fg = Green))))
        )

      RootNode(
        width = m.terminalWidth,
        height = boxHeight + 3,
        children = children,
        input = Some(
          InputNode(
            2.x,
            (boxHeight + 1).y,
            renderedPrompt.text,
            Style(fg = Green),
            cursor = renderedPrompt.cursorIndex
          )
        )
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      val raw   = input.value.trim
      val lower = raw.toLowerCase
      lower match
        case "next"  => Right(NextTab)
        case "prev"  => Right(PrevTab)
        case "inc"   => Right(Increment)
        case "dec"   => Right(Decrement)
        case "clear" => Right(ClearNotes)
        case "exit"  => Right(Exit)
        case t if t.startsWith("tab ") =>
          t.stripPrefix("tab ").trim match
            case "1" => Right(SelectTab(0))
            case "2" => Right(SelectTab(1))
            case "3" => Right(SelectTab(2))
            case _   => Left(TermFlowError.Validation("Use tab 1, tab 2, or tab 3."))
        case t if t.startsWith("note ") =>
          Right(AddNote(raw.drop(5).trim))
        case other =>
          Left(TermFlowError.Validation(s"Unknown command: $other"))
