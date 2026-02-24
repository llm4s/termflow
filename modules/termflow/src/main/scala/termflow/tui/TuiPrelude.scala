package termflow.tui

/** Shared aliases and lightweight syntax; replace former package object. */
object TuiPrelude {

  /** Fully edited line accepted from the prompt. */
  opaque type PromptLine = String
  object PromptLine {
    def apply(value: String): PromptLine = value

    extension (line: PromptLine) {
      def value: String = line
    }
  }

  /** Result type used by TUI parsing and commands. */
  type Result[A] = Either[TermFlowError, A]

  /** Syntax helpers: `2.x`, `3.y`. */
  extension (i: Int) {
    def x: XCoord = XCoord(i)
    def y: YCoord = YCoord(i)
  }

  /** String syntax helpers for creating styled text nodes. */
  extension (txt: String) {
    def text: Text                                      = Text(txt, Style())
    def text(style: Style): Text                        = Text(txt, style)
    def text(fg: Color): Text                           = Text(txt, Style(fg = fg))
    def text(fg: Color, bg: Color): Text                = Text(txt, Style(fg = fg, bg = bg))
    def text(fg: Color, bg: Color, bold: Boolean): Text = Text(txt, Style(fg = fg, bg = bg, bold = bold))
    def text(fg: Color, bg: Color, bold: Boolean, underline: Boolean): Text =
      Text(txt, Style(fg = fg, bg = bg, bold = bold, underline = underline))
    def text(fg: Color, bg: Color, bold: Boolean, underline: Boolean, border: Boolean): Text =
      Text(txt, Style(fg = fg, bg = bg, bold = bold, underline = underline, border = border))
  }
}
