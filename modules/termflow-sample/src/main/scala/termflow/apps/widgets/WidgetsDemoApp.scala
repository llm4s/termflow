package termflow.apps.widgets

import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets.*

/**
 * End-to-end demo of the new TermFlow stack: [[Layout]], [[Theme]], the
 * stateless widgets in `termflow.tui.widgets`, plus [[FocusManager]] and
 * [[Keymap]] for input dispatch.
 *
 * ## Keys
 *
 *   - `Tab`               — cycle button focus
 *   - `Enter` / `Space`   — activate the focused button
 *   - `t`                 — toggle dark / light theme
 *   - `+` / `-`           — nudge progress by ±10 %
 *   - `q` / `Ctrl+C` / `Esc` — quit
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

  /** Stable focus identifiers for the two demo buttons. */
  val SaveFocus: FocusId   = FocusId("save")
  val CancelFocus: FocusId = FocusId("cancel")

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
    fm: FocusManager,
    darkTheme: Boolean,
    lastAction: String
  )

  import Msg.*

  /**
   * Declarative key bindings — composed from the standard quit + focus
   * presets layered with the demo's app-specific shortcuts.
   *
   * Defining the keymap once at object scope rather than per-call keeps the
   * `update` body short and lets the bindings be inspected and tested
   * independently.
   */
  val Keys: Keymap[Msg] =
    Keymap.quit(Quit) ++
      Keymap.focus(next = NextFocus, previous = PrevFocus) ++
      Keymap(
        KeyDecoder.InputKey.Enter        -> Activate,
        KeyDecoder.InputKey.CharKey(' ') -> Activate,
        KeyDecoder.InputKey.CharKey('t') -> ToggleTheme,
        KeyDecoder.InputKey.CharKey('T') -> ToggleTheme,
        KeyDecoder.InputKey.CharKey('+') -> BumpProgress(0.1),
        KeyDecoder.InputKey.CharKey('=') -> BumpProgress(0.1),
        KeyDecoder.InputKey.CharKey('-') -> BumpProgress(-0.1),
        KeyDecoder.InputKey.CharKey('_') -> BumpProgress(-0.1)
      )

  object App extends TuiApp[Model, Msg]:

    private def syncTerminalSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.terminalWidth && h == m.terminalHeight then m
      else m.copy(terminalWidth = w, terminalHeight = h)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      // Spinner / progress animation.
      Sub.Every(millis = 120L, msg = () => Tick, sink = ctx)
      // Keyboard input. We don't use Prompt here — keys are dispatched via
      // the declarative Keymap below.
      Sub.InputKey(ConsoleInputKey.apply, ConsoleInputError.apply, ctx)
      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        tick = 0,
        progress = 0.0,
        fm = FocusManager(Vector(SaveFocus, CancelFocus)),
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
          sized.copy(fm = sized.fm.next).tui

        case PrevFocus =>
          sized.copy(fm = sized.fm.previous).tui

        case Activate =>
          val label = sized.fm.current match
            case Some(id) if id == SaveFocus   => "Save"
            case Some(id) if id == CancelFocus => "Cancel"
            case Some(id)                      => id.value
            case None                          => "(nothing focused)"
          sized.copy(lastAction = s"clicked $label").tui

        case ToggleTheme =>
          sized.copy(darkTheme = !sized.darkTheme).tui

        case BumpProgress(delta) =>
          val next = math.max(0.0, math.min(1.0, sized.progress + delta))
          sized.copy(progress = next).tui

        case Quit =>
          Tui(sized, Cmd.Exit)

        case ConsoleInputKey(key) =>
          Keys.lookup(key) match
            case Some(next) => update(sized, next, ctx)
            case None       => sized.tui

        case ConsoleInputError(e) =>
          sized.copy(lastAction = s"input error: ${Option(e.getMessage).getOrElse("unknown")}").tui

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

      // Button row driven by the focus manager — the manager owns which id
      // is focused; widgets just ask `isFocused(id)` and render accordingly.
      val buttons = Layout.row(gap = 2)(
        Button(label = "Save", focused = m.fm.isFocused(SaveFocus)),
        Button(label = "Cancel", focused = m.fm.isFocused(CancelFocus))
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
