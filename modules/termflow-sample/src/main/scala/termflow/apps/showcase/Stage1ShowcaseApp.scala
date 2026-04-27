package termflow.apps.showcase

import termflow.tui.*
import termflow.tui.Theme.themed
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

/**
 * One-screen showcase of every Stage 1 capability:
 *
 *   - **Color depth + capability detection** (PR #157 / issue #148): the
 *     "Capabilities" panel surfaces the detected `ColorDepth`, mouse, and
 *     unicode flags from `ctx.terminal.capabilities`. The "Palette" panel
 *     paints a truecolor RGB swatch row — the renderer downgrades it to
 *     whatever depth the terminal actually supports.
 *   - **Signal-driven resize** (PR #158 / issue #151): resize your
 *     terminal — the middle "Palette" column reflows in real time. The
 *     `Sub.TerminalResize` subscription is wired to JLine's SIGWINCH
 *     handler when available.
 *   - **Themable border characters** (PR #159 / issue #152): press `b` to
 *     cycle through `BorderChars.{sharp, rounded, double, ascii}`. Same
 *     widget code, visibly different chrome.
 *   - **Overlay / modal dialog** (PR #160 / issue #153): press `d` to open
 *     a confirm modal. Base-view bindings are gated on
 *     `model.dialog.isEmpty` — that's the modal contract.
 *   - **Layout.Fill + render-time resolution** (PR #162 / issue #154):
 *     the body uses `RootNode.layout` with a Row of fixed/Fill/fixed
 *     children. The renderer resolves it against the current terminal
 *     width, so resizing reflows automatically.
 *
 * Run with `sbt showcase`.
 */
object Stage1ShowcaseApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  private val borderStyles: Vector[(String, BorderChars)] = Vector(
    "sharp"   -> BorderChars.sharp,
    "rounded" -> BorderChars.rounded,
    "double"  -> BorderChars.double,
    "ascii"   -> BorderChars.ascii
  )

  private val themePresets: Vector[(String, Theme)] = Vector(
    "dark"  -> Theme.dark,
    "light" -> Theme.light,
    "mono"  -> Theme.mono
  )

  // RGB swatch row — the renderer downgrades to the active ColorDepth.
  private val swatchRgb: Vector[Color] = Vector(
    Color.Rgb(255, 64, 64),  // red
    Color.Rgb(255, 160, 32), // orange
    Color.Rgb(255, 224, 64), // yellow
    Color.Rgb(96, 224, 96),  // green
    Color.Rgb(64, 192, 224), // cyan
    Color.Rgb(96, 128, 255), // blue
    Color.Rgb(192, 96, 224)  // purple
  )

  enum Dialog:
    case None
    case ConfirmQuit(yesFocused: Boolean)

  final case class Model(
    width: Int,
    height: Int,
    borderIdx: Int,
    themeIdx: Int,
    dialog: Dialog,
    capabilities: Capabilities,
    termName: String,
    input: Sub[Msg],
    resize: Sub[Msg]
  ):
    def borderName: String = borderStyles(borderIdx)._1
    def chars: BorderChars = borderStyles(borderIdx)._2
    def themeName: String  = themePresets(themeIdx)._1
    def baseTheme: Theme   = themePresets(themeIdx)._2

    /** Theme with the user-selected border chars folded in. */
    def theme: Theme = baseTheme.copy(chars = chars)

  enum Msg:
    case Resize(w: Int, h: Int)
    case CycleBorder
    case CycleTheme
    case OpenDialog
    case CloseDialog
    case ToggleDialogFocus
    case Quit
    case Key(k: KeyDecoder.InputKey)
    case KeyError(t: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        width = ctx.terminal.width,
        height = ctx.terminal.height,
        borderIdx = 1, // start on rounded — visibly different from default
        themeIdx = 0,
        dialog = Dialog.None,
        capabilities = ctx.terminal.capabilities,
        termName = sys.env.getOrElse("TERM", "?"),
        input = Sub.InputKey(k => Key(k), e => KeyError(e), ctx),
        resize = Sub.TerminalResize[Msg](200, (w, h) => Resize(w, h), ctx)
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Resize(w, h) => m.copy(width = w, height = h).tui
        case CycleBorder  => m.copy(borderIdx = (m.borderIdx + 1) % borderStyles.size).tui
        case CycleTheme   => m.copy(themeIdx = (m.themeIdx + 1) % themePresets.size).tui
        case OpenDialog   => m.copy(dialog = Dialog.ConfirmQuit(yesFocused = false)).tui
        case CloseDialog  => m.copy(dialog = Dialog.None).tui
        case ToggleDialogFocus =>
          m.dialog match
            case Dialog.ConfirmQuit(yes) => m.copy(dialog = Dialog.ConfirmQuit(!yes)).tui
            case Dialog.None             => m.tui
        case Quit        => Tui(m, Cmd.Exit)
        case KeyError(_) => m.tui
        case Key(k)      => Tui(m, dispatch(m, k))

    private def dispatch(m: Model, k: KeyDecoder.InputKey): Cmd[Msg] =
      import KeyDecoder.InputKey.*
      m.dialog match
        case Dialog.ConfirmQuit(yesFocused) =>
          k match
            case ArrowLeft | ArrowRight => Cmd.GCmd(ToggleDialogFocus)
            case Enter                  => if yesFocused then Cmd.GCmd(Quit) else Cmd.GCmd(CloseDialog)
            case Escape                 => Cmd.GCmd(CloseDialog)
            case _                      => Cmd.NoCmd
        case Dialog.None =>
          k match
            case CharKey('b') | CharKey('B') => Cmd.GCmd(CycleBorder)
            case CharKey('t') | CharKey('T') => Cmd.GCmd(CycleTheme)
            case CharKey('d') | CharKey('D') => Cmd.GCmd(OpenDialog)
            case CharKey('q') | CharKey('Q') => Cmd.GCmd(OpenDialog)
            case Escape                      => Cmd.GCmd(OpenDialog)
            case _                           => Cmd.NoCmd

    override def view(m: Model): RootNode =
      given Theme = m.theme

      val titleNode = TextNode(
        2.x,
        1.y,
        List(
          "TermFlow Stage 1 Showcase".themed(_.primary),
          "  ".text,
          s"theme=${m.themeName}".text,
          "  ".text,
          s"border=${m.borderName}".text,
          "  ".text,
          s"size=${m.width}×${m.height}".text
        )
      )

      val body: Layout = Layout.Row(
        gap = 1,
        children = List(
          Layout.Elem(capabilitiesPanel(m)),
          Layout.Fill(Layout.Elem(palettePanel(m))),
          Layout.Elem(borderInfoPanel(m))
        )
      )

      val helpNode = TextNode(
        2.x,
        (m.height - 1).y,
        List(
          " b ".themed(_.primary),
          "border  ".text,
          " t ".themed(_.primary),
          "theme  ".text,
          " d ".themed(_.primary),
          "dialog  ".text,
          " q ".themed(_.primary),
          "quit  ".text,
          " resize the window — Fill layout reflows live ".themed(_.success)
        )
      )

      val baseLayout = Layout.Column(
        gap = 0,
        children = List(
          Layout.Spacer(1, 1), // title is on row 1, body starts row 3
          Layout.Fill(body)    // body fills the middle
        )
      )

      val baseRoot = RootNode(
        width = math.max(60, m.width),
        height = math.max(12, m.height - 2), // leave room for title + footer
        children = List(titleNode),
        input = None,
        layout = Some(baseLayout)
      )

      // Help footer is positioned absolutely at row m.height - 1 (above the
      // very last row, which TuiRuntime uses for cursor housekeeping).
      val withFooter = baseRoot.copy(children = baseRoot.children :+ helpNode)

      m.dialog match
        case Dialog.None => withFooter
        case Dialog.ConfirmQuit(yesFocused) =>
          withFooter.copy(overlays =
            List(
              Dialogs.confirm(
                prompt = "Quit the showcase?",
                yesFocused = yesFocused,
                title = "Confirm",
                yesLabel = "Quit",
                noLabel = "Stay"
              )
            )
          )

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.Validation("showcase has no prompt"))

    // ---- panels --------------------------------------------------------------

    private def capabilitiesPanel(m: Model)(using theme: Theme): VNode =
      val caps      = m.capabilities
      val title     = TextNode(2.x, 1.y, List(" Capabilities ".themed(_.primary)))
      val depthLine = TextNode(2.x, 3.y, List("depth: ".text, depthLabel(caps.colorDepth).themed(_.success)))
      val mouseLine = TextNode(2.x, 4.y, List("mouse: ".text, (if caps.mouse then "yes" else "no").themed(_.info)))
      val uniLine   = TextNode(2.x, 5.y, List("utf-8: ".text, (if caps.unicode then "yes" else "no").themed(_.info)))
      val noteLine  = TextNode(2.x, 7.y, List("$TERM=".text, m.termName.themed(_.info)))
      VNode.BoxNode(
        x = 1.x,
        y = 1.y,
        width = 22,
        height = 8,
        children = List(title, depthLine, mouseLine, uniLine, noteLine),
        style = Style(fg = theme.border, border = true),
        chars = theme.chars
      )

    private def palettePanel(m: Model)(using theme: Theme): VNode =
      // The middle panel is sized by Layout.Fill; the BoxNode's width is
      // overridden by the resolver. We author at width=10, the renderer
      // resizes us to whatever's left after the side panels.
      val title = TextNode(2.x, 1.y, List(" Palette ".themed(_.primary)))
      val description = TextNode(
        2.x,
        3.y,
        List(
          "RGB swatches downgrade ".text,
          s"to ${depthLabel(m.capabilities.colorDepth)}".themed(_.info)
        )
      )
      val swatch = TextNode(
        2.x,
        5.y,
        swatchRgb.toList.flatMap(c => List("███".text(fg = c), " ".text))
      )
      VNode.BoxNode(
        x = 1.x,
        y = 1.y,
        width = 10, // overridden at render time by Fill
        height = 8,
        children = List(title, description, swatch),
        style = Style(fg = theme.border, border = true),
        chars = theme.chars
      )

    private def borderInfoPanel(m: Model)(using theme: Theme): VNode =
      val title    = TextNode(2.x, 1.y, List(" Borders ".themed(_.primary)))
      val nameLine = TextNode(2.x, 3.y, List("style: ".text, m.borderName.themed(_.success)))
      val sample = TextNode(
        2.x,
        5.y,
        List(
          s"${m.chars.topLeft}${m.chars.horizontal}${m.chars.horizontal}${m.chars.topRight}".themed(_.border)
        )
      )
      val sample2 = TextNode(
        2.x,
        6.y,
        List(s"${m.chars.bottomLeft}${m.chars.horizontal}${m.chars.horizontal}${m.chars.bottomRight}".themed(_.border))
      )
      VNode.BoxNode(
        x = 1.x,
        y = 1.y,
        width = 17,
        height = 8,
        children = List(title, nameLine, sample, sample2),
        style = Style(fg = theme.border, border = true),
        chars = theme.chars
      )

    private def depthLabel(d: ColorDepth): String = d match
      case ColorDepth.Mono       => "Mono"
      case ColorDepth.Ansi8      => "Ansi8"
      case ColorDepth.Ansi16     => "Ansi16"
      case ColorDepth.Indexed256 => "Indexed256"
      case ColorDepth.Truecolor  => "Truecolor"
