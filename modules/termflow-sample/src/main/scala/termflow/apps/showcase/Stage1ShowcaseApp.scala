package termflow.apps.showcase

import termflow.tui.*
import termflow.tui.Theme.themed
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

/**
 * One-screen showcase of every Stage 1 capability *plus* the user-visible
 * Stage 2 additions:
 *
 * **Stage 1**
 *   - **Color depth + capability detection** (PR #157 / issue #148): the
 *     "Capabilities" panel surfaces detected `ColorDepth`, mouse, and
 *     unicode flags from `ctx.terminal.capabilities`; the "Palette" panel
 *     paints a truecolor swatch row that the renderer downgrades on the
 *     fly.
 *   - **Signal-driven resize** (PR #158 / issue #151): resize your
 *     terminal — the middle column reflows live thanks to
 *     `Sub.TerminalResize` wiring SIGWINCH.
 *   - **Themable border characters** (PR #159 / issue #152): `b` cycles
 *     `BorderChars.{sharp, rounded, double, ascii}` (or click the
 *     "Borders" panel rows).
 *   - **Overlay / modal dialog** (PR #160 / issue #153): `d` opens a
 *     confirm modal that suppresses base-view bindings.
 *
 * **Stage 2**
 *   - **Extended SGR attributes** (§5.5): the "Styles" panel renders bold
 *     / italic / dim / underline / reverse / strikethrough / blink. The
 *     terminal's `extendedStyles` capability decides which actually emit.
 *   - **Extended modifier parsing** (§5.6): the "Live input" panel
 *     reflects the most recent decoded key — try Ctrl+ArrowRight,
 *     Shift+F5, Alt+letter.
 *   - **Bracketed paste** (§5.4): paste anything into the terminal — the
 *     "Live input" panel collapses the paste into one event.
 *   - **Unicode width** (§5.3): the "Unicode" panel lays out CJK,
 *     fullwidth ASCII, and emoji over a column ruler.
 *   - **Mouse / SGR-1006** (§5.1): click a row in the **Themes** or
 *     **Borders** panel to switch directly, or hover and scroll the
 *     wheel to cycle. The "Live input" panel shows the decoded
 *     `MouseEvent`.
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

  // ---- Layout constants (absolute positioning so mouse hit-testing is exact) ----
  private val topRowY             = 3
  private val topPanelHeight      = 11
  private val bottomRowY          = topRowY + topPanelHeight + 1 // 15
  private val bottomPanelHeight   = 13
  private val capsPanelWidth      = 22
  private val themesPanelWidth    = 22
  private val bordersPanelWidth   = 22
  private val stylesPanelWidth    = 18
  private val liveInputPanelWidth = 36

  /**
   * Bounding box `(col, row, width, height)` of a panel — top-left col/row
   *  are 1-based to match the renderer's coordinate convention.
   */
  final private case class Rect(col: Int, row: Int, width: Int, height: Int):
    def contains(c: Int, r: Int): Boolean =
      c >= col && c < col + width && r >= row && r < row + height

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
    /** Most recent input event, formatted for display in the live panel. */
    lastEvent: Option[String],
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
    case SelectThemeIdx(i: Int)
    case SelectBorderIdx(i: Int)
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
        lastEvent = None,
        input = Sub.InputKey(k => Key(k), e => KeyError(e), ctx),
        resize = Sub.TerminalResize[Msg](200, (w, h) => Resize(w, h), ctx)
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Resize(w, h)       => m.copy(width = w, height = h).tui
        case CycleBorder        => m.copy(borderIdx = (m.borderIdx + 1) % borderStyles.size).tui
        case CycleTheme         => m.copy(themeIdx = (m.themeIdx + 1) % themePresets.size).tui
        case SelectThemeIdx(i)  => m.copy(themeIdx = clampIdx(i, themePresets.size)).tui
        case SelectBorderIdx(i) => m.copy(borderIdx = clampIdx(i, borderStyles.size)).tui
        case OpenDialog         => m.copy(dialog = Dialog.ConfirmQuit(yesFocused = false)).tui
        case CloseDialog        => m.copy(dialog = Dialog.None).tui
        case ToggleDialogFocus =>
          m.dialog match
            case Dialog.ConfirmQuit(yes) => m.copy(dialog = Dialog.ConfirmQuit(!yes)).tui
            case Dialog.None             => m.tui
        case Quit        => Tui(m, Cmd.Exit)
        case KeyError(_) => m.tui
        case Key(k) =>
          val withEvent = m.copy(lastEvent = Some(formatEvent(k)))
          Tui(withEvent, dispatch(withEvent, k))

    private def clampIdx(i: Int, size: Int): Int = math.max(0, math.min(size - 1, i))

    private def dispatch(m: Model, k: KeyDecoder.InputKey): Cmd[Msg] =
      import KeyDecoder.InputKey.*
      m.dialog match
        case Dialog.ConfirmQuit(yesFocused) =>
          k match
            case ArrowLeft | ArrowRight => Cmd.GCmd(ToggleDialogFocus)
            case Enter                  => if yesFocused then Cmd.GCmd(Quit) else Cmd.GCmd(CloseDialog)
            case Escape                 => Cmd.GCmd(CloseDialog)
            case Mouse(_)               => Cmd.NoCmd
            case _                      => Cmd.NoCmd
        case Dialog.None =>
          k match
            case CharKey('b') | CharKey('B') => Cmd.GCmd(CycleBorder)
            case CharKey('t') | CharKey('T') => Cmd.GCmd(CycleTheme)
            case CharKey('d') | CharKey('D') => Cmd.GCmd(OpenDialog)
            case CharKey('q') | CharKey('Q') => Cmd.GCmd(OpenDialog)
            case Escape                      => Cmd.GCmd(OpenDialog)
            case Mouse(ev)                   => mouseDispatch(m, ev)
            case _                           => Cmd.NoCmd

    /**
     * Map a [[MouseEvent]] to a model command using static panel rects.
     *
     *   - Press inside the Themes panel's row strip → [[SelectThemeIdx]].
     *   - Press inside the Borders panel's row strip → [[SelectBorderIdx]].
     *   - Scroll wheel inside either panel cycles through entries.
     *
     * Returns [[Cmd.NoCmd]] for releases, drags, and clicks outside any
     * known target — the live-input panel still reflects the raw event.
     */
    private def mouseDispatch(m: Model, ev: MouseEvent): Cmd[Msg] =
      val (col, row) = ev.at
      val themesR    = themesRect(m)
      val bordersR   = bordersRect(m)

      ev match
        case MouseEvent.Press(MouseButton.Left, _, _, _) =>
          if themesR.contains(col, row) then
            themeIndexAtRow(themesR, row).map(i => Cmd.GCmd[Msg](SelectThemeIdx(i))).getOrElse(Cmd.NoCmd)
          else if bordersR.contains(col, row) then
            borderIndexAtRow(bordersR, row).map(i => Cmd.GCmd[Msg](SelectBorderIdx(i))).getOrElse(Cmd.NoCmd)
          else Cmd.NoCmd

        case MouseEvent.Scroll(dir, _, _, _) =>
          if themesR.contains(col, row) then
            val next = nextIndex(m.themeIdx, themePresets.size, dir)
            Cmd.GCmd(SelectThemeIdx(next))
          else if bordersR.contains(col, row) then
            val next = nextIndex(m.borderIdx, borderStyles.size, dir)
            Cmd.GCmd(SelectBorderIdx(next))
          else Cmd.NoCmd

        case _ => Cmd.NoCmd

    private def nextIndex(current: Int, size: Int, dir: ScrollDirection): Int =
      dir match
        case ScrollDirection.Up | ScrollDirection.Left =>
          (current - 1 + size) % size
        case ScrollDirection.Down | ScrollDirection.Right =>
          (current + 1) % size

    /**
     * Inside the Themes panel, the first selectable row is at `panel.row + 3`
     *  (skip the top border + title + blank). One row per entry.
     */
    private def themeIndexAtRow(panel: Rect, row: Int): Option[Int] =
      val offset = row - (panel.row + 3)
      if offset >= 0 && offset < themePresets.size then Some(offset) else None

    private def borderIndexAtRow(panel: Rect, row: Int): Option[Int] =
      val offset = row - (panel.row + 3)
      if offset >= 0 && offset < borderStyles.size then Some(offset) else None

    private def themesRect(m: Model): Rect =
      Rect(col = m.width - themesPanelWidth, row = topRowY, width = themesPanelWidth, height = topPanelHeight)

    private def bordersRect(m: Model): Rect =
      Rect(
        col = m.width - themesPanelWidth - bordersPanelWidth - 1,
        row = topRowY,
        width = bordersPanelWidth,
        height = topPanelHeight
      )

    private def capsRect: Rect =
      Rect(col = 1, row = topRowY, width = capsPanelWidth, height = topPanelHeight)

    private def paletteRect(m: Model): Rect =
      val left  = capsRect.col + capsRect.width + 1
      val right = bordersRect(m).col - 1
      Rect(col = left, row = topRowY, width = math.max(10, right - left), height = topPanelHeight)

    private def stylesRect: Rect =
      Rect(col = 1, row = bottomRowY, width = stylesPanelWidth, height = bottomPanelHeight)

    private def liveInputRect(m: Model): Rect =
      Rect(
        col = m.width - liveInputPanelWidth,
        row = bottomRowY,
        width = liveInputPanelWidth,
        height = bottomPanelHeight
      )

    private def unicodeRect(m: Model): Rect =
      val left  = stylesRect.col + stylesRect.width + 1
      val right = liveInputRect(m).col - 1
      Rect(col = left, row = bottomRowY, width = math.max(20, right - left), height = bottomPanelHeight)

    /**
     * One-line description of an [[KeyDecoder.InputKey]] for the "Live
     * input" panel. Mouse / paste / modified keys get distinct prefixes so
     * the Stage 2 wiring is visible at a glance.
     */
    private def formatEvent(k: KeyDecoder.InputKey): String =
      import KeyDecoder.InputKey.*
      k match
        case CharKey(c)            => s"key  CharKey('${c}')"
        case Ctrl(c)               => s"key  Ctrl+${c}"
        case Modified(inner, mods) => s"key  ${describeMods(mods)}+${describeKey(inner)}"
        case Paste(text) =>
          val preview = text.take(40).replace("\n", "\\n").replace("\r", "\\r")
          val suffix  = if text.length > 40 then "…" else ""
          s"paste(${text.length} chars) \"$preview$suffix\""
        case Mouse(ev) => s"mouse ${describeMouse(ev)}"
        case other     => s"key  ${describeKey(other)}"

    private def describeKey(k: KeyDecoder.InputKey): String =
      import KeyDecoder.InputKey.*
      k match
        case CharKey(c) => s"'$c'"
        case Ctrl(c)    => s"Ctrl+$c"
        case other      => other.toString.takeWhile(_ != '(')

    private def describeMods(m: KeyDecoder.Modifiers): String =
      val parts = List(
        Option.when(m.ctrl)("Ctrl"),
        Option.when(m.alt)("Alt"),
        Option.when(m.shift)("Shift"),
        Option.when(m.meta)("Meta")
      ).flatten
      if parts.isEmpty then "" else parts.mkString("+")

    private def describeMouse(ev: MouseEvent): String =
      val (col, row) = ev.at
      val mods       = describeMods(ev.modifiers)
      val modPrefix  = if mods.isEmpty then "" else s"$mods+"
      ev match
        case MouseEvent.Press(b, _, _, _)   => s"${modPrefix}${b}-press   ($col,$row)"
        case MouseEvent.Release(b, _, _, _) => s"${modPrefix}${b}-release ($col,$row)"
        case MouseEvent.Drag(b, _, _, _)    => s"${modPrefix}${b}-drag    ($col,$row)"
        case MouseEvent.Move(_, _, _)       => s"${modPrefix}move           ($col,$row)"
        case MouseEvent.Scroll(d, _, _, _)  => s"${modPrefix}scroll-$d  ($col,$row)"

    override def view(m: Model): RootNode =
      given Theme = m.theme

      val titleNode = TextNode(
        2.x,
        1.y,
        List(
          Text(
            "TermFlow Showcase",
            Style(fg = m.theme.primary, bold = true, italic = true)
          ),
          "  ".text,
          s"theme=${m.themeName}".text,
          "  ".text,
          s"border=${m.borderName}".text,
          "  ".text,
          s"size=${m.width}×${m.height}".text
        )
      )

      val helpNode = TextNode(
        2.x,
        (m.height - 1).y,
        List(
          " click ".themed(_.primary),
          "/scroll Themes & Borders to change  ".text,
          " b ".themed(_.primary),
          "border  ".text,
          " t ".themed(_.primary),
          "theme  ".text,
          " d ".themed(_.primary),
          "dialog  ".text,
          " q ".themed(_.primary),
          "quit ".text
        )
      )

      val panels: List[VNode] = List(
        capabilitiesPanel(m, capsRect),
        palettePanel(m, paletteRect(m)),
        themesPanel(m, themesRect(m)),
        bordersPanel(m, bordersRect(m)),
        stylesPanel(stylesRect),
        unicodePanel(unicodeRect(m)),
        liveInputPanel(m, liveInputRect(m))
      )

      val baseRoot = RootNode(
        width = math.max(60, m.width),
        height = math.max(20, m.height),
        children = titleNode :: panels ++ List(helpNode),
        input = None
      )

      m.dialog match
        case Dialog.None => baseRoot
        case Dialog.ConfirmQuit(yesFocused) =>
          baseRoot.copy(overlays =
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

    private def capabilitiesPanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val caps  = m.capabilities
      val title = TextNode(2.x, 1.y, List(" Capabilities ".themed(_.primary)))
      val rows = List(
        kv(2, 3, "depth", depthLabel(caps.colorDepth)),
        kv(2, 4, "mouse", yesNo(caps.mouse)),
        kv(2, 5, "utf-8", yesNo(caps.unicode)),
        kv(2, 6, "styles+", yesNo(caps.extendedStyles)),
        kv(2, 7, "paste", yesNo(caps.bracketedPaste)),
        TextNode(2.x, 9.y, List("$TERM=".text, m.termName.themed(_.info)))
      )
      panel(r, theme, title :: rows)

    private def palettePanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Palette ".themed(_.primary)))
      val description = TextNode(
        2.x,
        3.y,
        List("RGB swatches downgrade ".text, s"to ${depthLabel(m.capabilities.colorDepth)}".themed(_.info))
      )
      val swatch = TextNode(
        2.x,
        5.y,
        swatchRgb.toList.flatMap(c => List("███".text(fg = c), " ".text))
      )
      panel(r, theme, List(title, description, swatch))

    private def themesPanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Themes (click) ".themed(_.primary)))
      val rows = themePresets.zipWithIndex.toList.map { case ((name, _), i) =>
        renderableRow(2, 3 + i, name, selected = i == m.themeIdx, theme)
      }
      val hint = TextNode(2.x, (3 + themePresets.size + 1).y, List("scroll to cycle".themed(_.info)))
      panel(r, theme, (title :: rows) :+ hint)

    private def bordersPanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Borders (click) ".themed(_.primary)))
      val rows = borderStyles.zipWithIndex.toList.map { case ((name, _), i) =>
        renderableRow(2, 3 + i, name, selected = i == m.borderIdx, theme)
      }
      val hint = TextNode(2.x, (3 + borderStyles.size + 1).y, List("scroll to cycle".themed(_.info)))
      panel(r, theme, (title :: rows) :+ hint)

    private def renderableRow(col: Int, row: Int, name: String, selected: Boolean, theme: Theme): VNode =
      val marker = if selected then "▸ " else "  "
      val style =
        if selected then Style(fg = theme.background, bg = theme.primary, bold = true)
        else Style(fg = theme.foreground)
      TextNode(col.x, row.y, List(Text(s"$marker$name", style)))

    private def stylesPanel(r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Styles ".themed(_.primary)))
      val rows = List(
        TextNode(2.x, 3.y, List(Text("bold", Style(fg = theme.primary, bold = true)))),
        TextNode(2.x, 4.y, List(Text("italic", Style(fg = theme.primary, italic = true)))),
        TextNode(2.x, 5.y, List(Text("underline", Style(fg = theme.primary, underline = true)))),
        TextNode(2.x, 6.y, List(Text("dim", Style(fg = theme.primary, dim = true)))),
        TextNode(2.x, 7.y, List(Text("reverse", Style(fg = theme.primary, reverse = true)))),
        TextNode(2.x, 8.y, List(Text("strike", Style(fg = theme.primary, strikethrough = true)))),
        TextNode(2.x, 9.y, List(Text("blink", Style(fg = theme.primary, blink = true)))),
        TextNode(
          2.x,
          11.y,
          List(Text("combo", Style(fg = theme.success, bold = true, italic = true, underline = true)))
        )
      )
      panel(r, theme, title :: rows)

    private def unicodePanel(r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Unicode width ".themed(_.primary)))
      val ruler = TextNode(2.x, 3.y, List("|0123456789|0123456789|0123456789|".themed(_.border)))
      val cjk   = TextNode(2.x, 4.y, List("中文 日本語 한국어 → CJK = 2 cells".text(fg = theme.info)))
      val full  = TextNode(2.x, 5.y, List("ＡＢＣＤＥ → fullwidth ASCII".text(fg = theme.info)))
      val emoji = TextNode(2.x, 6.y, List("hello 🎉 world 🚀 → emoji = 2 cells".text(fg = theme.info)))
      val mixed = TextNode(2.x, 7.y, List("a中b日c韓 → mixed lays out cleanly".text(fg = theme.info)))
      val hint  = TextNode(2.x, 9.y, List("Each glyph above lines up with the ruler.".themed(_.success)))
      panel(r, theme, List(title, ruler, cjk, full, emoji, mixed, hint))

    private def liveInputPanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val title    = TextNode(2.x, 1.y, List(" Live input ".themed(_.primary)))
      val intro    = TextNode(2.x, 3.y, List("Most recent decoded event:".text))
      val event    = m.lastEvent.getOrElse("(press a key, click, or paste)")
      val eventTxt = TextNode(2.x, 5.y, List(event.themed(_.success)))
      val tips = List(
        TextNode(2.x, 7.y, List("• Click rows in Themes/Borders".text)),
        TextNode(2.x, 8.y, List("• Scroll-wheel to cycle".text)),
        TextNode(2.x, 9.y, List("• Ctrl+Arrow → Modified key".text)),
        TextNode(2.x, 10.y, List("• ⌘V / Ctrl-V paste → Paste(...)".text))
      )
      panel(r, theme, List(title, intro, eventTxt) ++ tips)

    /** Bordered panel positioned at `r` with the theme's border style. */
    private def panel(r: Rect, theme: Theme, children: List[VNode]): VNode =
      VNode.BoxNode(
        x = r.col.x,
        y = r.row.y,
        width = r.width,
        height = r.height,
        children = children.map(translateInto(r, _)),
        style = Style(fg = theme.border, border = true),
        chars = theme.chars
      )

    /**
     * Translate a child authored in panel-local (1.x, 1.y) coordinates into
     *  the panel's absolute frame position.
     */
    private def translateInto(r: Rect, child: VNode): VNode =
      Layout.translate(child, r.col - 1, r.row - 1)

    private def kv(col: Int, row: Int, label: String, value: String)(using theme: Theme): VNode =
      TextNode(col.x, row.y, List(s"$label: ".text, value.themed(_.success)))

    private def yesNo(b: Boolean): String = if b then "yes" else "no"

    private def depthLabel(d: ColorDepth): String = d match
      case ColorDepth.Mono       => "Mono"
      case ColorDepth.Ansi8      => "Ansi8"
      case ColorDepth.Ansi16     => "Ansi16"
      case ColorDepth.Indexed256 => "Indexed256"
      case ColorDepth.Truecolor  => "Truecolor"
