package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.TuiPrelude.*

class TuiPreludeSpec extends AnyFunSuite:

  test("PromptLine wraps and unwraps the underlying string"):
    val line = PromptLine("hello")
    assert(line.value == "hello")

  test("re-exported coordinate / text syntax is callable through TuiPrelude"):
    // Exercise every overload of `text` exported from ScreenPrelude so the
    // forwarder lines all get hit.
    assert(2.x == XCoord(2))
    assert(3.y == YCoord(3))
    assert("hi".text == Text("hi", Style()))
    assert("hi".text(Style(bold = true)) == Text("hi", Style(bold = true)))
    assert("hi".text(Color.Red) == Text("hi", Style(fg = Color.Red)))
    assert("hi".text(Color.Red, Color.Blue) == Text("hi", Style(fg = Color.Red, bg = Color.Blue)))
    assert(
      "hi".text(Color.Red, Color.Blue, bold = true) ==
        Text("hi", Style(fg = Color.Red, bg = Color.Blue, bold = true))
    )
    assert(
      "hi".text(Color.Red, Color.Blue, bold = true, underline = true) ==
        Text("hi", Style(fg = Color.Red, bg = Color.Blue, bold = true, underline = true))
    )
    assert(
      "hi".text(Color.Red, Color.Blue, bold = true, underline = true, border = true) ==
        Text("hi", Style(fg = Color.Red, bg = Color.Blue, bold = true, underline = true, border = true))
    )

  test("Devtools.now returns a wall-clock timestamp"):
    val before = System.currentTimeMillis()
    val ts     = Devtools.now()
    val after  = System.currentTimeMillis()
    assert(ts >= before && ts <= after + 1)
