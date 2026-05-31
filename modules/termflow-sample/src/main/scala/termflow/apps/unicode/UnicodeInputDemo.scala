package termflow.apps.unicode

import termflow.tui.*
import termflow.tui.Color.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets

/**
 * Unicode input demo with per-language sample text and macOS keyboard setup instructions.
 *
 * Key bindings:
 *   Tab / Enter — move and pick menu items
 *   ESC — leave the composer and focus the menu
 *   Ctrl+C — exit
 *   Enter — newline in composer
 *   Ctrl+D — submit message
 */
object UnicodeInputDemo:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  // === Language data ===

  final case class Lang(
    name: String,
    script: String,
    inputSource: String, // macOS Input Source name to add
    tip: String,
    samples: Vector[String]
  )

  val languages: Vector[Lang] = Vector(
    Lang(
      "Arabic",
      "العربية",
      "Arabic",
      "Arabic script connects right-to-left automatically in the terminal.",
      Vector(
        "مرحبا بك في عالم الطرفية",
        "كيف حالك اليوم؟",
        "اللغة العربية لغة جميلة"
      )
    ),
    Lang(
      "Chinese",
      "中文",
      "Pinyin - Simplified",
      "Type Pinyin, then press Space and pick characters from the candidate window.",
      Vector(
        "你好，欢迎使用 TermFlow",
        "这是一个多行输入测试",
        "编程让世界更美好"
      )
    ),
    Lang(
      "Japanese",
      "日本語",
      "Japanese - Romaji",
      "Type Romaji, press Space to convert to Kana/Kanji, Enter to confirm.",
      Vector(
        "こんにちは、TermFlow へようこそ",
        "これはテストです",
        "プログラミングは楽しい"
      )
    ),
    Lang(
      "Korean",
      "한국어",
      "2-Set Korean",
      "Type jamo (자모) — they compose into syllable blocks automatically.",
      Vector(
        "안녕하세요, TermFlow에 오신 것을 환영합니다",
        "이것은 테스트입니다",
        "프로그래밍은 재미있습니다"
      )
    ),
    Lang(
      "Russian",
      "Русский",
      "Russian",
      "Standard ЙЦУКЕН layout — phonetic mapping to QWERTY keys.",
      Vector(
        "Привет, добро пожаловать в TermFlow",
        "Это тест многострочного ввода",
        "Программирование — это весело"
      )
    ),
  )

  private val exitMenuLabel = "Exit"

  final case class Model(
    w: Int,
    h: Int,
    activeIdx: Int,
    menuIdx: Int,
    menuOpen: Boolean,
    messages: Vector[Vector[String]], // one Vector per language
    composer: widgets.MultiLineInput.State,
    input: Sub[Msg]
  )

  enum Msg:
    case Submit(text: String)
    case EditorKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)
    case Quit

  import Msg.*

  // === App ===

  object App extends TuiApp[Model, Msg]:

    private def syncSize(m: Model, ctx: RuntimeCtx[Msg]): Model =
      val w = ctx.terminal.width
      val h = ctx.terminal.height
      if w == m.w && h == m.h then m
      else m.copy(w = w, h = h)

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        w = ctx.terminal.width,
        h = ctx.terminal.height,
        activeIdx = 0,
        menuIdx = 0,
        menuOpen = false,
        messages = Vector.fill(languages.size)(Vector.empty),
        composer = widgets.MultiLineInput.State.empty,
        input = Sub.InputKey(key => EditorKey(key), err => ConsoleInputError(err), ctx)
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = syncSize(m, ctx)
      msg match
        case Submit(text) =>
          if text.trim.isEmpty then sized.tui
          else
            val updated = sized.messages.updated(sized.activeIdx, sized.messages(sized.activeIdx) :+ text)
            sized.copy(messages = updated).tui

        case EditorKey(k) =>
          if sized.menuOpen then
            val menuCount = languages.size + 1
            val cycle =
              (idx: Int, delta: Int) => (idx + delta + menuCount) % menuCount
            k match
              case KeyDecoder.InputKey.Ctrl('C') =>
                Tui(sized, Cmd.Exit)
              case KeyDecoder.InputKey.Escape =>
                sized.copy(menuOpen = false).tui
              case KeyDecoder.InputKey.Tab | KeyDecoder.InputKey.ArrowRight | KeyDecoder.InputKey.ArrowDown =>
                sized.copy(menuIdx = cycle(sized.menuIdx, 1)).tui
              case KeyDecoder.InputKey.BackTab | KeyDecoder.InputKey.ArrowLeft | KeyDecoder.InputKey.ArrowUp =>
                sized.copy(menuIdx = cycle(sized.menuIdx, -1)).tui
              case KeyDecoder.InputKey.Enter =>
                if sized.menuIdx >= languages.size then Tui(sized, Cmd.Exit)
                else sized.copy(activeIdx = sized.menuIdx, menuOpen = false).tui
              case _ =>
                sized.tui
          else
            k match
              case KeyDecoder.InputKey.Ctrl('C') =>
                Tui(sized, Cmd.Exit)
              case KeyDecoder.InputKey.Escape =>
                sized.copy(menuOpen = true, menuIdx = sized.activeIdx).tui
              case KeyDecoder.InputKey.Tab =>
                sized.copy(menuOpen = true, menuIdx = sized.activeIdx).tui
              case _ =>
                val submitKey = KeyDecoder.InputKey.Ctrl('D')
                val (nextComposer, maybeCmd) =
                  widgets.MultiLineInput.handleKey[Msg](sized.composer, k, submitKey) { text =>
                    Right(Msg.Submit(text))
                  }
                maybeCmd match
                  case Some(cmd) => Tui(sized.copy(composer = nextComposer), cmd)
                  case None      => sized.copy(composer = nextComposer).tui

        case Quit =>
          Tui(sized, Cmd.Exit)

        case ConsoleInputError(_) =>
          sized.tui

    override def toMsg(input: PromptLine): Result[Msg] =
      Right(Submit(input.value))

    override def view(m: Model): RootNode =
      val w          = m.w
      val h          = m.h
      val lang       = languages(m.activeIdx)
      val maxW       = math.min(w, 82)
      val innerW     = maxW - 4
      val activeMsgs = m.messages(m.activeIdx)

      // ---- ROW LAYOUT (fixed zones, dynamically positioned) ----
      // Row 1: header
      // Row 3: lang selector label
      // Row 4-5: lang buttons (2 rows of 4/3, plus exit in the last slot)
      // Row 6: separator
      // Row 7: active lang header
      // Row 8-10: sample texts (3 lines max)
      // Row 11-13: macOS tip (3 lines)
      // Row 14: messages separator
      // Row 15..N: messages
      // Bottom: composer box + footer

      def text(x: Int, y: Int, s: String, style: Style = Style()): TextNode =
        TextNode(x.x, y.y, List(Text(s, style)))

      def dim(x: Int, y: Int, s: String): TextNode =
        text(x, y, s, Style(fg = BrightBlack))

      def accent(x: Int, y: Int, s: String): TextNode =
        text(x, y, s, Style(fg = Cyan, bold = true))

      def label(x: Int, y: Int, s: String): TextNode =
        text(x, y, s, Style(fg = Yellow))

      def highlight(x: Int, y: Int, s: String): TextNode =
        text(x, y, s, Style(fg = Green, bold = true))

      // --- Header ---
      val header = TextNode(
        1.x,
        1.y,
        List(
          Text(" Unicode Input Demo ", Style(fg = Black, bg = Cyan, bold = true)),
          Text("  Tab: menu  |  Enter: pick  |  ESC: menu  |  Ctrl+C: exit ", Style(fg = White, bg = Magenta))
        )
      )

      // --- Language selector ---
      val selLabelY = 3
      val selLabel  = label(2, selLabelY, "Select language (Tab to menu, Enter to pick):")

      val selRow1Y = selLabelY + 1
      val selRow1 = List(
        menuButton(2, selRow1Y, 0, "1. Arabic", m.activeIdx, m.menuIdx, m.menuOpen),
        menuButton(22, selRow1Y, 1, "2. Chinese", m.activeIdx, m.menuIdx, m.menuOpen),
        menuButton(42, selRow1Y, 2, "3. Japanese", m.activeIdx, m.menuIdx, m.menuOpen),
        menuButton(62, selRow1Y, 3, "4. Korean", m.activeIdx, m.menuIdx, m.menuOpen)
      )

      val selRow2Y = selRow1Y + 1
      val selRow2 = List(
        menuButton(2, selRow2Y, 4, "5. Russian", m.activeIdx, m.menuIdx, m.menuOpen),
        menuButton(42, selRow2Y, languages.size, s"6. $exitMenuLabel", m.activeIdx, m.menuIdx, m.menuOpen)
      )

      // --- Separator ---
      val sep1Y = selRow2Y + 1
      val sep1  = dim(2, sep1Y, "─" * (maxW - 2))

      // --- Active language ---
      val langHeaderY = sep1Y + 1
      val langHeader  = accent(2, langHeaderY, s"◉ ${lang.name} — ${lang.script}")

      // --- Sample texts ---
      val samplesY    = langHeaderY + 1
      val sampleLabel = label(2, samplesY, "📋 Sample text (copy-paste to test):")
      val sampleTexts = lang.samples.zipWithIndex.map((s, i) => highlight(4, samplesY + 1 + i, s"• $s"))

      // --- macOS tip ---
      val tipY = samplesY + 1 + lang.samples.size
      val tipAdd = label(
        2,
        tipY,
        s"💡 Add «${lang.inputSource}» input source (Settings → Keyboard → Input Sources → +). Switch ⌃Space."
      )
      val tipLines = Vector(lang.tip)
      val tipLangNodes = tipLines.zipWithIndex.map { case (line, i) =>
        dim(4, tipY + 1 + i, line)
      }

      // --- Messages ---
      val msgSepY    = tipY + tipLines.size + 2
      val msgSep     = dim(2, msgSepY, "─" * (maxW - 2))
      val msgHeaderY = msgSepY + 1
      val msgHeader  = label(2, msgHeaderY, s"Messages: ${activeMsgs.size} submitted")

      val msgStartY      = msgHeaderY + 1
      val composerH      = 8
      val composerY      = math.max(msgStartY + 1, h - composerH + 1)
      val composerInnerY = composerY + 1
      val composerInnerH = composerH - 2
      val msgEndY        = math.max(msgStartY, composerY - 1)

      val allLines: Vector[String] = activeMsgs.flatMap(_.split("\n", -1).toVector)
      val msgVisible               = math.max(0, msgEndY - msgStartY)
      val visible                  = allLines.takeRight(msgVisible)
      val pad                      = msgVisible - visible.size

      val msgNodes = (0 until msgVisible).map { i =>
        val y = msgStartY + i
        if i < pad then text(2, y, " " * innerW)
        else
          val line    = visible(i - pad)
          val display = if line.length <= innerW then line else line.take(innerW - 3) + "..."
          text(2, y, s" $display", Style(fg = White))
      }.toList

      // --- Chat composer ---
      val composerBox = BoxNode(
        2.x,
        composerY.y,
        maxW - 2,
        composerH,
        children = Nil,
        style = Style(border = true, fg = Cyan),
        chars = BorderChars.rounded
      )
      val composerBg     = Color.Rgb(248, 248, 248)
      val composerWidth  = math.max(20, maxW - 16)
      val composerInputX = 14
      val composerFill = (0 until composerInnerH).map { row =>
        TextNode(3.x, (composerInnerY + row).y, List(Text(" " * (maxW - 4), Style(bg = composerBg))))
      }.toList
      val composerTitle   = label(4, composerInnerY, "Message:")
      val composerHint    = dim(4, composerInnerY + composerInnerH - 1, "Enter: newline | Ctrl+D: send | Esc/Tab: menu")
      val composerVisible = widgets.MultiLineInput.scrollFor(m.composer, 4)
      val composerInputY  = (composerInnerY + 1 + (composerVisible.cursorRow - composerVisible.scrollTop)).y
      val composerInput: InputNode =
        InputNode(
          composerInputX.x,
          composerInputY,
          prompt = composerVisible.currentLine,
          style = Style(fg = Black, bg = composerBg),
          cursor = composerVisible.cursorCol,
          lineWidth = composerWidth
        )
      def clipToWidth(text: String, maxWidth: Int): String =
        if maxWidth <= 0 then ""
        else
          val sb        = new StringBuilder
          var i         = 0
          var w         = 0
          var truncated = false
          while i < text.length && w < maxWidth do
            val cp = text.codePointAt(i)
            val cw = WCWidth.codePointWidth(cp)
            if cw > 0 then
              if w + cw > maxWidth then
                truncated = true
                i = text.length
              else
                sb.append(new String(Character.toChars(cp)))
                w += cw
                i += Character.charCount(cp)
            else i += Character.charCount(cp)
          if truncated && maxWidth > 0 then sb.append("…")
          sb.toString

      def padToWidth(text: String, width: Int): String =
        val clipped = clipToWidth(text, width)
        val pad     = math.max(0, width - WCWidth.stringWidth(clipped))
        clipped + (" " * pad)

      val composerRows = {
        val top    = composerVisible.scrollTop
        val bottom = math.min(m.composer.lines.size, top + 4)
        (top until bottom).toList.flatMap { rowIdx =>
          if rowIdx == composerVisible.cursorRow then Nil
          else
            val rowText = padToWidth(m.composer.lines(rowIdx), composerWidth)
            List(
              TextNode(
                composerInputX.x,
                (composerInnerY + 1 + (rowIdx - top)).y,
                List(Text(rowText, Style(fg = Black, bg = composerBg)))
              )
            )
        }
      }

      RootNode(
        width = w,
        height = h,
        input = Some(composerInput),
        children = List(header, selLabel) ++
          selRow1 ++ selRow2 ++
          List(sep1, langHeader) ++
          List(sampleLabel) ++ sampleTexts ++
          List(tipAdd) ++ tipLangNodes ++
          List(msgSep, msgHeader) ++ msgNodes ++
          List(composerBox) ++ composerFill ++ List(composerTitle, composerHint) ++ composerRows
      )

    private def menuButton(
      x: Int,
      y: Int,
      idx: Int,
      label: String,
      active: Int,
      selected: Int,
      menuOpen: Boolean
    ): TextNode =
      val isActive   = idx == active
      val isSelected = menuOpen && idx == selected
      val prefix     = if isActive then "▶" else " "
      val style =
        if isSelected then Style(fg = Black, bg = Green, bold = true)
        else if isActive then Style(fg = Green, bold = true)
        else Style(fg = White)
      TextNode(x.x, y.y, List(Text(s"$prefix[$label]$prefix", style)))
