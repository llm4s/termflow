package termflow.tui

/**
 * Lightweight screen-layer syntax: 1-based coordinate constructors and
 * fluent [[Text]] segment builders.
 *
 * Lives in `termflow-screen` so apps that depend only on the screen layer
 * (or unit tests for screen primitives) can write `2.x` / `3.y` /
 * `"hi".text` without pulling in the rest of the app layer. The
 * `termflow-app` [[TuiPrelude]] re-exports the same names alongside its
 * own additions so consumers that currently say
 * `import termflow.tui.TuiPrelude.*` keep working unchanged.
 */
object ScreenPrelude:

  /**
   * 1-based coordinate syntax: `2.x` / `10.y` for `XCoord(2)` /
   * `YCoord(10)` when building nodes in `view`.
   */
  extension (i: Int)
    def x: XCoord = XCoord(i)
    def y: YCoord = YCoord(i)

  /**
   * Fluent helpers for constructing [[Text]] segments from plain strings.
   *
   * {{{
   * TextNode(2.x, 3.y, List(
   *   "Status: ".text,
   *   "ready".text(fg = Color.Green, bold = true)
   * ))
   * }}}
   */
  extension (txt: String)
    def text: Text                                      = Text(txt, Style())
    def text(style: Style): Text                        = Text(txt, style)
    def text(fg: Color): Text                           = Text(txt, Style(fg = fg))
    def text(fg: Color, bg: Color): Text                = Text(txt, Style(fg = fg, bg = bg))
    def text(fg: Color, bg: Color, bold: Boolean): Text = Text(txt, Style(fg = fg, bg = bg, bold = bold))
    def text(fg: Color, bg: Color, bold: Boolean, underline: Boolean): Text =
      Text(txt, Style(fg = fg, bg = bg, bold = bold, underline = underline))
    def text(fg: Color, bg: Color, bold: Boolean, underline: Boolean, border: Boolean): Text =
      Text(txt, Style(fg = fg, bg = bg, bold = bold, underline = underline, border = border))
