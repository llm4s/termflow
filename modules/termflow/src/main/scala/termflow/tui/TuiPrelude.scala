package termflow.tui

/**
 * Shared type aliases and lightweight syntax sugar used across TermFlow
 * applications.
 *
 * Import the contents of `TuiPrelude.*` at the top of an app file to
 * unlock:
 *   - `Result[A]` as a shorthand for `Either[TermFlowError, A]`
 *   - `2.x` / `3.y` coordinate syntax
 *   - `"hello".text` / `"hi".text(fg = Color.Green, bold = true)` styled
 *     text constructors
 *   - `PromptLine`, the opaque type wrapping a submitted prompt line
 */
object TuiPrelude:

  /**
   * Opaque wrapper for a completed line submitted from a prompt.
   *
   * `PromptLine` exists to keep `String`-typed raw input separate from
   * domain strings in `update` / `view` call sites. Construct with
   * `PromptLine(value)` and extract with `line.value`.
   */
  opaque type PromptLine = String
  object PromptLine:

    /** Wrap a raw string as a `PromptLine`. */
    def apply(value: String): PromptLine = value

    /** Extension exposing the underlying string value. */
    extension (line: PromptLine) def value: String = line

  /**
   * Convenience alias for the fallible result of parsing user input or
   * running a validation step.
   *
   * Left-valued results surface through the runtime as
   * [[Cmd.TermFlowErrorCmd]] without terminating the app.
   */
  type Result[A] = Either[TermFlowError, A]

  /**
   * 1-based coordinate syntax: write `2.x` / `10.y` instead of
   * `XCoord(2)` / `YCoord(10)` when building nodes in `view`.
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
