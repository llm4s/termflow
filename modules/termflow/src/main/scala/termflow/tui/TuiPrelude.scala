package termflow.tui

/** Shared aliases and lightweight syntax; replace former package object. */
object TuiPrelude {

  /** Fully edited line accepted from the prompt. */
  type PromptLine = String

  /** Result type used by TUI parsing and commands. */
  type Result[A] = Either[TermFlowError, A]

  /** Syntax helpers: `2.x`, `3.y`. */
  implicit final class CoordsIntOps(val i: Int) extends AnyVal {
    def x: XCoord = XCoord(i)
    def y: YCoord = YCoord(i)
  }

  /** String syntax helpers for creating styled text nodes. */
  implicit final class StringTextOps(val txt: String) extends AnyVal {
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
