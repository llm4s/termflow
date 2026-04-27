package termflow.tui

/**
 * Character set used to draw a [[VNode.BoxNode]]'s border.
 *
 * Apps can swap a `BorderChars` value via the ambient [[Theme.chars]] (or by
 * passing `chars` directly to the box) without re-implementing the
 * box-drawing logic in the renderer.
 *
 * @param topLeft     Top-left corner glyph.
 * @param topRight    Top-right corner glyph.
 * @param bottomLeft  Bottom-left corner glyph.
 * @param bottomRight Bottom-right corner glyph.
 * @param horizontal  Horizontal edge glyph (top and bottom rows).
 * @param vertical    Vertical edge glyph (left and right columns).
 */
final case class BorderChars(
  topLeft: Char,
  topRight: Char,
  bottomLeft: Char,
  bottomRight: Char,
  horizontal: Char,
  vertical: Char
)

object BorderChars:

  /** Single-line square corners (`┌─┐ │ │ └─┘`). The default. */
  val sharp: BorderChars =
    BorderChars('┌', '┐', '└', '┘', '─', '│')

  /** Single-line rounded corners (`╭─╮ │ │ ╰─╯`). */
  val rounded: BorderChars =
    BorderChars('╭', '╮', '╰', '╯', '─', '│')

  /** Double-line corners (`╔═╗ ║ ║ ╚═╝`). */
  val double: BorderChars =
    BorderChars('╔', '╗', '╚', '╝', '═', '║')

  /** ASCII-only fallback for terminals without box-drawing glyph support. */
  val ascii: BorderChars =
    BorderChars('+', '+', '+', '+', '-', '|')
