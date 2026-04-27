package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.Theme.themed

class ThemeSpec extends AnyFunSuite:

  test("dark theme exposes every semantic slot with a non-default colour"):
    val slots = Theme.dark.slots
    assert(
      slots.map(_._1).toSet == Set(
        "primary",
        "secondary",
        "error",
        "warning",
        "success",
        "info",
        "border",
        "background",
        "foreground"
      )
    )
    // Every slot in dark is concrete — no Color.Default leaks.
    assert(slots.forall { case (_, c) => c != Color.Default })

  test("light theme exposes every semantic slot with a non-default colour"):
    val slots = Theme.light.slots
    assert(slots.forall { case (_, c) => c != Color.Default })

  test("dark and light differ on background / foreground but share accents"):
    assert(Theme.dark.background != Theme.light.background)
    assert(Theme.dark.foreground != Theme.light.foreground)
    // Accents are identical — basic ANSI-16 renders fine on either bg.
    assert(Theme.dark.primary == Theme.light.primary)
    assert(Theme.dark.success == Theme.light.success)
    assert(Theme.dark.error == Theme.light.error)

  test("mono theme has every slot set to Color.Default"):
    assert(Theme.mono.slots.forall { case (_, c) => c == Color.Default })

  test("copy lets callers override individual slots"):
    val high = Theme.dark.copy(success = Color.White)
    assert(high.success == Color.White)
    // Other slots untouched.
    assert(high.primary == Theme.dark.primary)
    assert(high.error == Theme.dark.error)

  test("Theme.current returns the given in implicit scope"):
    given Theme = Theme.light
    assert(Theme.current eq Theme.light)

  test("Theme.slot projects a slot from the ambient theme"):
    given Theme = Theme.dark
    assert(Theme.slot(_.primary) == Color.Blue)
    assert(Theme.slot(_.success) == Color.Green)

  test("Style.themed copies the receiver and sets fg from a slot"):
    given Theme         = Theme.dark
    val base            = Style(bold = true, underline = true)
    val accented: Style = base.themed(_.primary)
    assert(accented.fg == Color.Blue)
    // Other fields preserved.
    assert(accented.bold)
    assert(accented.underline)
    // Receiver is not mutated.
    assert(base.fg == Color.Default)

  test("String.themed produces a Text with the ambient theme's slot colour"):
    given Theme = Theme.dark
    val t: Text = "ready".themed(_.success)
    assert(t.txt == "ready")
    assert(t.style.fg == Color.Green)
    assert(!t.style.bold)

  test("theming against the mono theme is a no-op colour-wise"):
    given Theme = Theme.mono
    val t       = "hello".themed(_.primary)
    assert(t.style.fg == Color.Default)
    val s = Style(bold = true).themed(_.error)
    assert(s.fg == Color.Default)
    assert(s.bold)

  test("dark/light/mono all default to BorderChars.sharp"):
    assert(Theme.dark.chars == BorderChars.sharp)
    assert(Theme.light.chars == BorderChars.sharp)
    assert(Theme.mono.chars == BorderChars.sharp)

  test("Theme.rounded uses rounded corners but inherits dark colours"):
    assert(Theme.rounded.chars == BorderChars.rounded)
    assert(Theme.rounded.primary == Theme.dark.primary)
    assert(Theme.rounded.background == Theme.dark.background)
    assert(Theme.rounded.foreground == Theme.dark.foreground)

  test("Theme.box uses ambient chars and border slot"):
    given Theme = Theme.rounded
    val box     = Theme.box(XCoord(2), YCoord(3), 10, 4)
    box match
      case b @ VNode.BoxNode(_, _, _, _, _, _, _) =>
        assert(b.chars == BorderChars.rounded)
        assert(b.style.fg == Theme.rounded.border)
        assert(b.style.border)
      case _ => fail("expected BoxNode")

  test("Theme.box preserves an explicit fg colour passed via style"):
    given Theme  = Theme.rounded
    val redStyle = Style(border = true, fg = Color.Red)
    val box      = Theme.box(XCoord(1), YCoord(1), 4, 4, style = redStyle)
    box match
      case b @ VNode.BoxNode(_, _, _, _, _, _, _) =>
        assert(b.style.fg == Color.Red, "explicit fg must not be overridden by theme.border")
      case _ => fail("expected BoxNode")
