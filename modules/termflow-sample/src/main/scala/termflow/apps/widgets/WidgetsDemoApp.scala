package termflow.apps.widgets

import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets.*

/**
 * End-to-end demo of the `termflow.tui.widgets` package.
 *
 * Drives [[Button]], [[ProgressBar]], [[Spinner]], and [[StatusBar]]
 * through a `Layout.column` + `Layout.row` composition, using a `given
 * Theme` that the user can toggle at runtime.
 *
 * ## Keys
 *
 *   - `Tab` / `Shift+Tab` — cycle button focus
 *   - `Enter` / `Space`   — activate the focused button
 *   - `t`                 — toggle dark / light theme
 *   - `+` / `-`           — nudge progress by ±10 %
 *   - `q` / `Ctrl+C`      — quit
 *
 * The spinner and progress bar are driven by a `Sub.Every` timer so they
 * animate on their own.
 *
 * Run with:
 * {{{
 *   sbt "termflowSample/runMain termflow.apps.widgets.WidgetsDemoApp"
 * }}}
 */
object WidgetsDemoApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  enum Focus:
    case Save, Cancel

  enum Msg:
    case Tick
    case NextFocus
    case PrevFocus
    case Activate
    case ToggleTheme
    case BumpProgress(delta: Double)
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)
    case Quit

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    tick: Int,
    progress: Double,
    focus: Focus,
    darkTheme: Boolean,
    lastAction: String
  )

  import Msg.*

  object App extends TuiApp[Model, Msg]:

    private def syncTerminalSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.terminalWidth && h == m.terminalHeight then m
      else m.copy(terminalWidth = w, terminalHeight = h)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      // Spinner / progress animation.
      Sub.Every(millis = 120L, msg = () => Tick, sink = ctx)
      // Keyboard input. We don't use Prompt here — keys are handled directly.
      Sub.InputKey(ConsoleInputKey.apply, ConsoleInputError.apply, ctx)
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        tick = 0,
        progress = 0.0,
        focus = Focus.Save,
        darkTheme = true,
        lastAction = "—"
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncTerminalSize(m, ctx)
      msg match
        case Tick =>
          val nextProgress =
            if sized.progress >= 1.0 then 0.0
            else math.min(1.0, sized.progress + 0.02)
          sized.copy(tick = sized.tick + 1, progress = nextProgress).tui

        case NextFocus =>
          val nf = if sized.focus == Focus.Save then Focus.Cancel else Focus.Save
          sized.copy(focus = nf).tui

        case PrevFocus =>
          // Two-button demo; prev and next are symmetric.
          val nf = if sized.focus == Focus.Save then Focus.Cancel else Focus.Save
          sized.copy(focus = nf).tui

        case Activate =>
          sized.copy(lastAction = s"clicked ${sized.focus}").tui

        case ToggleTheme =>
          sized.copy(darkTheme = !sized.darkTheme).tui

        case BumpProgress(delta) =>
          val next = math.max(0.0, math.min(1.0, sized.progress + delta))
          sized.copy(progress = next).tui

        case Quit =>
          Tui(sized, Cmd.Exit)

        case ConsoleInputKey(key) =>
          keyToMsg(key) match
            case Some(next) => update(sized, next, ctx)
            case None       => sized.tui

        case ConsoleInputError(e) =>
          sized.copy(lastAction = s"input error: ${Option(e.getMessage).getOrElse("unknown")}").tui

    private def keyToMsg(key: KeyDecoder.InputKey): Option[Msg] =
      import KeyDecoder.InputKey.*
      key match
        // Tab arrives as Ctrl+I via the ASCII decoder.
        case Ctrl('I')                   => Some(NextFocus)
        case Enter | CharKey(' ')        => Some(Activate)
        case CharKey('t') | CharKey('T') => Some(ToggleTheme)
        case CharKey('+') | CharKey('=') => Some(BumpProgress(0.1))
        case CharKey('-') | CharKey('_') => Some(BumpProgress(-0.1))
        case CharKey('q') | CharKey('Q') => Some(Quit)
        case Ctrl('C') | Escape          => Some(Quit)
        case _                           => None

    override def view(m: Model): RootNode =
      given Theme = if m.darkTheme then Theme.dark else Theme.light
      val theme   = summon[Theme]

      val themeName  = if m.darkTheme then "dark" else "light"
      val termWidth  = math.max(40, m.terminalWidth)
      val termHeight = math.max(14, m.terminalHeight)
      val barWidth   = math.max(10, math.min(40, termWidth - 12))

      // Static caption line.
      val title = TextNode(
        1.x,
        1.y,
        List(Text("TermFlow Widget Demo", Style(fg = theme.primary, bold = true, underline = true)))
      )

      // Spinner + progress bar.
      val spinnerAndBar = Layout.row(gap = 2)(
        Spinner(Spinner.Braille, frame = m.tick),
        ProgressBar(m.progress, width = barWidth)
      )

      // Button row with focus tracking.
      val buttons = Layout.row(gap = 2)(
        Button(label = "Save", focused = m.focus == Focus.Save),
        Button(label = "Cancel", focused = m.focus == Focus.Cancel)
      )

      // Instructions + last-action feedback.
      val keyHelp = TextNode(
        1.x,
        1.y,
        List(
          "Tab: focus   Enter/Space: activate   t: theme   +/-: nudge   q: quit".text(fg = theme.foreground)
        )
      )
      val lastAction = TextNode(
        1.x,
        1.y,
        List(Text(s"last action: ${m.lastAction}", Style(fg = theme.info)))
      )

      // Body column stacking title, animated row, button row, help, and last
      // action. Uses the ADT form of `Column` so we can mix `Elem(vnode)`
      // leaves with nested `Row` layouts in the same children list.
      val column = Layout.Column(
        gap = 1,
        children = List(
          Layout.Elem(title),
          spinnerAndBar,
          buttons,
          Layout.Elem(keyHelp),
          Layout.Elem(lastAction)
        )
      )
      val columnChildren = column.resolve(Coord(2.x, 2.y))

      // Inverse-video status bar pinned to the bottom row.
      val statusRow = termHeight
      val status = StatusBar(
        left = " demo ",
        center = s"theme=$themeName  tick=${m.tick}",
        right = f" progress=${m.progress * 100}%3.0f%% ",
        width = termWidth,
        at = Coord(1.x, statusRow.y)
      )

      RootNode(
        width = termWidth,
        height = termHeight,
        children = columnChildren :+ status,
        input = None
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      // This app drives input directly through ConsoleInputKey; `toMsg` is
      // only called when a Prompt is present, which it isn't here.
      Left(TermFlowError.CommandError(input.value))
