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

  /** Sample list-select items used by the `l` dialog binding. */
  private val listSelectItems: Vector[String] =
    Vector("apple", "banana", "cherry", "date", "elderberry", "fig", "grape", "honeydew")

  /**
   * How many list rows the `l` dialog renders at once. Used by both the
   *  view (to render) and the mouse hit-test (to map clicks to items).
   */
  private val listSelectMaxVisible: Int = 5

  /**
   * Title shown on the listSelect dialog's top border — declared once so
   *  view / mouse hit-test stay in sync.
   */
  private val listSelectTitle: String = "List select demo"

  /** Max events kept in `Model.eventHistory` for the LogView in the live panel. */
  private val EventHistoryCap: Int = 50

  enum Dialog:
    case None
    case ConfirmQuit(yesFocused: Boolean)
    case TextInput(state: Prompt.State, okFocused: Boolean)
    case ListSelect(selectedIndex: Int)
    case Waiting(openedAtTick: Long, autoCloseAtTick: Long)

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
    /**
     * Bounded history of recent events fed to the live-input panel via the
     * LogView widget. Latest events live at the tail.
     */
    eventHistory: Vector[String],
    /** Last value submitted from a textInput dialog (rendered in Live input). */
    lastTextInput: Option[String],
    /** Last item picked from a listSelect dialog. */
    lastListPick: Option[String],
    /** Tick counter incremented by `tickSub`; drives the waiting spinner. */
    tick: Long,
    input: Sub[Msg],
    resize: Sub[Msg],
    tickSub: Sub[Msg]
  ):
    def borderName: String = borderStyles(borderIdx)._1
    def chars: BorderChars = borderStyles(borderIdx)._2
    def themeName: String  = themePresets(themeIdx)._1
    def baseTheme: Theme   = themePresets(themeIdx)._2

    /** Theme with the user-selected border chars folded in. */
    def theme: Theme = baseTheme.copy(chars = chars)

  enum Msg:
    case Resize(w: Int, h: Int)
    case Tick
    case CycleBorder
    case CycleTheme
    case SelectThemeIdx(i: Int)
    case SelectBorderIdx(i: Int)
    case SelectListIdx(i: Int)
    case OpenDialog
    case OpenTextInput
    case OpenListSelect
    case OpenWaiting
    case CloseDialog
    case ToggleDialogFocus
    case TextInputAccept(value: String)
    case ListSelectAccept(index: Int)
    case Quit
    case Key(k: KeyDecoder.InputKey)
    case KeyError(t: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:

    /** Spinner runs at 10 fps; cheap, makes the waiting overlay actually animate. */
    private val tickPeriodMs = 100L

    /** Auto-close the waiting dialog 30 ticks (~3s) after it opens. */
    private val waitingDurationTicks = 30L

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
        eventHistory = Vector.empty,
        lastTextInput = None,
        lastListPick = None,
        tick = 0L,
        input = Sub.InputKey(k => Key(k), e => KeyError(e), ctx),
        resize = Sub.TerminalResize[Msg](200, (w, h) => Resize(w, h), ctx),
        tickSub = Sub.Every(tickPeriodMs, () => Tick, ctx)
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Resize(w, h)       => m.copy(width = w, height = h).tui
        case Tick               => onTick(m)
        case CycleBorder        => m.copy(borderIdx = (m.borderIdx + 1) % borderStyles.size).tui
        case CycleTheme         => m.copy(themeIdx = (m.themeIdx + 1) % themePresets.size).tui
        case SelectThemeIdx(i)  => m.copy(themeIdx = clampIdx(i, themePresets.size)).tui
        case SelectBorderIdx(i) => m.copy(borderIdx = clampIdx(i, borderStyles.size)).tui
        case OpenDialog         => m.copy(dialog = Dialog.ConfirmQuit(yesFocused = false)).tui
        case OpenTextInput =>
          m.copy(dialog = Dialog.TextInput(state = Prompt.State(), okFocused = true)).tui
        case OpenListSelect =>
          m.copy(dialog = Dialog.ListSelect(selectedIndex = 0)).tui
        case OpenWaiting =>
          m.copy(dialog = Dialog.Waiting(openedAtTick = m.tick, autoCloseAtTick = m.tick + waitingDurationTicks)).tui
        case CloseDialog => m.copy(dialog = Dialog.None).tui
        case ToggleDialogFocus =>
          m.dialog match
            case Dialog.ConfirmQuit(yes)        => m.copy(dialog = Dialog.ConfirmQuit(!yes)).tui
            case Dialog.TextInput(state, focus) => m.copy(dialog = Dialog.TextInput(state, !focus)).tui
            case _                              => m.tui
        case TextInputAccept(value) =>
          m.copy(lastTextInput = Some(value), dialog = Dialog.None).tui
        case ListSelectAccept(idx) =>
          m.copy(lastListPick = Some(listSelectItems(clampIdx(idx, listSelectItems.size))), dialog = Dialog.None).tui
        case SelectListIdx(i) =>
          m.dialog match
            case Dialog.ListSelect(_) =>
              m.copy(dialog = Dialog.ListSelect(clampIdx(i, listSelectItems.size))).tui
            case _ => m.tui
        case Quit        => Tui(m, Cmd.Exit)
        case KeyError(_) => m.tui
        case Key(k)      =>
          // Dialog.TextInput edits its prompt state through Prompt.handleKey
          // before dispatch. Other dialogs / the base view route through dispatch.
          val formatted = formatEvent(k)
          val withEvent = m.copy(
            lastEvent = Some(formatted),
            eventHistory = (m.eventHistory :+ formatted).takeRight(EventHistoryCap)
          )
          withEvent.dialog match
            case Dialog.TextInput(state, focus) =>
              val (nextState, maybeCmd) =
                Prompt.handleKey[Msg](state, k)(line => Right(TextInputAccept(line.value)))
              val nextDialog = Dialog.TextInput(nextState, focus)
              val nextModel  = withEvent.copy(dialog = nextDialog)
              maybeCmd match
                case Some(cmd) => Tui(nextModel, cmd)
                case None      => Tui(nextModel, dispatch(nextModel, k))
            case _ => Tui(withEvent, dispatch(withEvent, k))

    /**
     * Tick handler: advance the counter, auto-close Waiting when its
     *  duration elapses, and otherwise leave the model alone.
     */
    private def onTick(m: Model): Tui[Model, Msg] =
      val tickedModel = m.copy(tick = m.tick + 1)
      tickedModel.dialog match
        case Dialog.Waiting(_, deadline) if tickedModel.tick >= deadline =>
          tickedModel.copy(dialog = Dialog.None).tui
        case _ => tickedModel.tui

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

        case Dialog.TextInput(_, _) =>
          k match
            // Esc cancels; Tab moves focus between OK and Cancel.
            case Escape => Cmd.GCmd(CloseDialog)
            case CharKey('\t') | KeyDecoder.InputKey.BackTab =>
              Cmd.GCmd(ToggleDialogFocus)
            case _ => Cmd.NoCmd

        case Dialog.ListSelect(idx) =>
          k match
            case ArrowUp =>
              val next = math.max(0, idx - 1)
              Cmd.GCmd(SelectListIdx(next))
            case ArrowDown =>
              val next = math.min(listSelectItems.size - 1, idx + 1)
              Cmd.GCmd(SelectListIdx(next))
            case Home      => Cmd.GCmd(SelectListIdx(0))
            case End       => Cmd.GCmd(SelectListIdx(listSelectItems.size - 1))
            case Enter     => Cmd.GCmd(ListSelectAccept(idx))
            case Escape    => Cmd.GCmd(CloseDialog)
            case Mouse(ev) => listSelectMouseDispatch(m, idx, ev)
            case _         => Cmd.NoCmd

        case Dialog.Waiting(_, _) =>
          k match
            case Escape => Cmd.GCmd(CloseDialog)
            case _      => Cmd.NoCmd

        case Dialog.None =>
          k match
            case CharKey('b') | CharKey('B') => Cmd.GCmd(CycleBorder)
            case CharKey('t') | CharKey('T') => Cmd.GCmd(CycleTheme)
            case CharKey('d') | CharKey('D') => Cmd.GCmd(OpenDialog)
            case CharKey('i') | CharKey('I') => Cmd.GCmd(OpenTextInput)
            case CharKey('l') | CharKey('L') => Cmd.GCmd(OpenListSelect)
            case CharKey('w') | CharKey('W') => Cmd.GCmd(OpenWaiting)
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
     * Construct the listSelect Overlay used both for rendering and for
     *  mouse hit-testing. Centralising the call keeps the title /
     *  maxVisible / theme parameters consistent.
     */
    private def buildListSelectOverlay(idx: Int)(using Theme): Overlay =
      Dialogs.listSelect(
        title = listSelectTitle,
        items = listSelectItems,
        selectedIndex = idx,
        maxVisible = listSelectMaxVisible
      )

    /**
     * Resolved on-screen rectangle of the listSelect dialog at the
     *  current selection. Mouse hit-tests use this to map clicks to
     *  rows.
     */
    private def listSelectRect(m: Model, idx: Int): Rect =
      given Theme  = m.theme
      val o        = buildListSelectOverlay(idx)
      val (ox, oy) = OverlayPosition.resolve(o.position, o.width, o.height, m.width, m.height)
      Rect(col = ox.value, row = oy.value, width = o.width, height = o.height)

    /**
     * Mouse handling while the listSelect dialog is open: clicks on a
     *  visible row commit that item, scroll-wheel cycles the selection.
     */
    private def listSelectMouseDispatch(m: Model, currentIdx: Int, ev: MouseEvent): Cmd[Msg] =
      val (col, row) = ev.at
      val rect       = listSelectRect(m, currentIdx)
      val layout     = Dialogs.listSelectLayout(listSelectItems.size, currentIdx, listSelectMaxVisible)

      if !rect.contains(col, row) then Cmd.NoCmd
      else
        ev match
          case MouseEvent.Press(MouseButton.Left, _, _, _) =>
            val firstItemRowAbs = rect.row + (layout.firstRowOffset - 1)
            val itemOffset      = row - firstItemRowAbs
            if itemOffset >= 0 && itemOffset < layout.visibleCount then
              val itemIdx = layout.anchorIndex + itemOffset
              if itemIdx < listSelectItems.size then Cmd.GCmd(ListSelectAccept(itemIdx))
              else Cmd.NoCmd
            else Cmd.NoCmd
          case MouseEvent.Scroll(dir, _, _, _) =>
            val next = nextIndex(currentIdx, listSelectItems.size, dir)
            Cmd.GCmd(SelectListIdx(next))
          case _ => Cmd.NoCmd

    /**
     * Inside a panel rendered via [[panel]], the BoxNode draws its border on
     * the panel's first row and `translateInto` shifts every child down by
     * `panel.row - 1`. A row authored at panel-local Y=3 (the first list
     * row in `themesPanel` / `bordersPanel`) therefore lands at absolute
     * row `3 + (panel.row - 1) = panel.row + 2`. Each subsequent item is
     * one row below.
     */
    private def themeIndexAtRow(panel: Rect, row: Int): Option[Int] =
      val offset = row - (panel.row + 2)
      if offset >= 0 && offset < themePresets.size then Some(offset) else None

    private def borderIndexAtRow(panel: Rect, row: Int): Option[Int] =
      val offset = row - (panel.row + 2)
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
          "/scroll Themes & Borders  ".text,
          " b ".themed(_.primary),
          "border  ".text,
          " t ".themed(_.primary),
          "theme  ".text,
          " d ".themed(_.primary),
          "confirm  ".text,
          " i ".themed(_.primary),
          "input  ".text,
          " l ".themed(_.primary),
          "list  ".text,
          " w ".themed(_.primary),
          "wait  ".text,
          " q ".themed(_.primary),
          "quit ".text
        )
      )

      // Static stage Tabs bar — sits on row 2, between the title row and
      // the panels (which start at topRowY = 3). The active tab marks the
      // current development stage. Renders the real Tabs widget so the
      // showcase exercises every shipped widget end-to-end.
      val tabsNode = widgets.Tabs(
        labels = showcaseStageTabs,
        activeIndex = showcaseStageTabs.size - 1
      )
      val stageTabs = Layout.translate(tabsNode, dx = 1, dy = 1) // anchor at (2, 2)

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
        children = (titleNode :: stageTabs :: panels) ++ List(helpNode),
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
        case Dialog.TextInput(state, okFocused) =>
          baseRoot.copy(overlays =
            List(
              Dialogs.textInput(
                title = "Text input demo",
                prompt = "Type a value, Enter to accept:",
                value = state.buffer.mkString,
                cursor = state.cursor,
                okFocused = okFocused
              )
            )
          )
        case Dialog.ListSelect(idx) =>
          baseRoot.copy(overlays = List(buildListSelectOverlay(idx)))
        case Dialog.Waiting(openedAt, deadline) =>
          val remaining = math.max(0L, deadline - m.tick)
          baseRoot.copy(overlays =
            List(
              Dialogs.waiting(
                title = "Working…",
                body = s"simulated task — ${remaining / 10}.${remaining % 10}s remaining",
                tick = m.tick - openedAt,
                cancelLabel = Some("Cancel")
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
      // Use the RadioGroup widget instead of hand-rolled rows so the
      // showcase exercises the real widget API.
      val rows = widgets.RadioGroup(
        options = themePresets.map(_._1),
        selectedIndex = m.themeIdx,
        focusedIndex = m.themeIdx,
        at = Coord(2.x, 3.y)
      )
      val hint = TextNode(2.x, (3 + themePresets.size + 1).y, List("scroll to cycle".themed(_.info)))
      panel(r, theme, (title :: rows) :+ hint)

    private def bordersPanel(m: Model, r: Rect)(using theme: Theme): VNode =
      val title = TextNode(2.x, 1.y, List(" Borders (click) ".themed(_.primary)))
      val rows = widgets.RadioGroup(
        options = borderStyles.map(_._1),
        selectedIndex = m.borderIdx,
        focusedIndex = m.borderIdx,
        at = Coord(2.x, 3.y)
      )
      val hint = TextNode(2.x, (3 + borderStyles.size + 1).y, List("scroll to cycle".themed(_.info)))
      panel(r, theme, (title :: rows) :+ hint)

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
        // Quick CheckBox demo — three states rendered through the real
        // widget so visual differences (theme.success on the checked
        // marker, bold on focus) are visible at a glance.
        widgets.CheckBox(label = "off", checked = false, focused = false, at = Coord(2.x, 10.y)),
        widgets.CheckBox(label = "on", checked = true, focused = false, at = Coord(2.x, 11.y)),
        widgets.CheckBox(label = "focus", checked = true, focused = true, at = Coord(2.x, 12.y))
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
      val title = TextNode(2.x, 1.y, List(" Live input ".themed(_.primary)))
      val intro = TextNode(2.x, 3.y, List("Recent events (LogView, scrollable):".text))
      // LogView for the rolling event history. Five rows fit cleanly under
      // the intro inside the 13-row bottom panel.
      val historyRows = widgets.LogView(
        lines = m.eventHistory,
        width = r.width - 4,
        height = 5,
        at = Coord(2.x, 4.y),
        style = Some(Style(fg = theme.success))
      )
      val txt  = m.lastTextInput.map(s => s"\"${truncate(s, 24)}\"").getOrElse("—")
      val pick = m.lastListPick.getOrElse("—")
      val results = List(
        TextNode(2.x, 10.y, List("Last text input: ".text, txt.themed(_.info))),
        TextNode(2.x, 11.y, List("Last list pick:  ".text, pick.themed(_.info)))
      )
      panel(r, theme, (title :: intro :: historyRows) ++ results)

    private def truncate(s: String, max: Int): String =
      if s.length <= max then s else s.take(max - 1) + "…"

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
