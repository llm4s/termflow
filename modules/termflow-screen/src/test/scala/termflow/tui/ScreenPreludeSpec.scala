package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.ScreenPrelude.*

class ScreenPreludeSpec extends AnyFunSuite:

  test("Int.x and Int.y construct opaque coordinates"):
    assert(2.x == XCoord(2))
    assert(10.y == YCoord(10))

  test("text helper produces a default-styled segment"):
    assert("hello".text == Text("hello", Style()))

  test("text(style) overload uses the supplied style"):
    val s = Style(fg = Color.Red, italic = true)
    assert("hi".text(s) == Text("hi", s))

  test("text(fg) overload sets the foreground color only"):
    assert("hi".text(Color.Green) == Text("hi", Style(fg = Color.Green)))

  test("text(fg, bg) overload sets both colors"):
    val seg = "hi".text(Color.Red, Color.Black)
    assert(seg == Text("hi", Style(fg = Color.Red, bg = Color.Black)))

  test("text(fg, bg, bold) overload toggles bold"):
    val seg = "hi".text(Color.Red, Color.Black, bold = true)
    assert(seg == Text("hi", Style(fg = Color.Red, bg = Color.Black, bold = true)))

  test("text(fg, bg, bold, underline) overload toggles underline"):
    val seg = "hi".text(Color.Red, Color.Black, bold = true, underline = true)
    assert(seg == Text("hi", Style(fg = Color.Red, bg = Color.Black, bold = true, underline = true)))

  test("text(fg, bg, bold, underline, border) overload sets border too"):
    val seg = "hi".text(Color.Red, Color.Black, bold = true, underline = true, border = true)
    assert(
      seg == Text(
        "hi",
        Style(fg = Color.Red, bg = Color.Black, bold = true, underline = true, border = true)
      )
    )
