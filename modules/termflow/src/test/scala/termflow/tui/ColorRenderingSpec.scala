package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class ColorRenderingSpec extends AnyFunSuite:
  import AnsiRenderer.colorToAnsi

  // -- legacy 8-color path is unchanged -------------------------------------

  test("Color.Default emits no SGR at any depth") {
    for depth <- ColorDepth.values do
      assert(colorToAnsi(Color.Default, isBg = false, depth) == "")
      assert(colorToAnsi(Color.Default, isBg = true, depth) == "")
  }

  test("named colors map to 30..37 / 40..47 (Ansi8+)") {
    assert(colorToAnsi(Color.Black, isBg = false, ColorDepth.Ansi8) == s"[30m")
    assert(colorToAnsi(Color.Red, isBg = false, ColorDepth.Ansi8) == s"[31m")
    assert(colorToAnsi(Color.White, isBg = false, ColorDepth.Ansi8) == s"[37m")
    assert(colorToAnsi(Color.Black, isBg = true, ColorDepth.Ansi8) == s"[40m")
    assert(colorToAnsi(Color.White, isBg = true, ColorDepth.Ansi8) == s"[47m")
  }

  test("Mono emits empty for every color") {
    assert(colorToAnsi(Color.Red, isBg = false, ColorDepth.Mono) == "")
    assert(colorToAnsi(Color.Indexed(123), isBg = false, ColorDepth.Mono) == "")
    assert(colorToAnsi(Color.Rgb(10, 20, 30), isBg = false, ColorDepth.Mono) == "")
  }

  // -- bright colors --------------------------------------------------------

  test("BrightRed emits 91 / 101 on Ansi16 and downgrades to 31 / 41 on Ansi8") {
    assert(colorToAnsi(Color.BrightRed, isBg = false, ColorDepth.Ansi16) == s"[91m")
    assert(colorToAnsi(Color.BrightRed, isBg = true, ColorDepth.Ansi16) == s"[101m")
    assert(colorToAnsi(Color.BrightRed, isBg = false, ColorDepth.Ansi8) == s"[31m")
    assert(colorToAnsi(Color.BrightRed, isBg = true, ColorDepth.Ansi8) == s"[41m")
  }

  // -- indexed --------------------------------------------------------------

  test("Indexed(123) on Indexed256 → 38;5;123") {
    assert(colorToAnsi(Color.Indexed(123), isBg = false, ColorDepth.Indexed256) == s"[38;5;123m")
    assert(colorToAnsi(Color.Indexed(123), isBg = true, ColorDepth.Indexed256) == s"[48;5;123m")
  }

  test("Indexed clamps to 0..255") {
    assert(colorToAnsi(Color.Indexed(-5), isBg = false, ColorDepth.Indexed256) == s"[38;5;0m")
    assert(colorToAnsi(Color.Indexed(999), isBg = false, ColorDepth.Indexed256) == s"[38;5;255m")
  }

  test("Indexed(196) (a vivid red in the cube) downgrades to a red on Ansi8") {
    val out = colorToAnsi(Color.Indexed(196), isBg = false, ColorDepth.Ansi8)
    // Ansi8 reds are 31 (Red); BrightRed (91) won't appear because we're on Ansi8.
    assert(out == s"[31m", s"got $out")
  }

  test("Indexed downgrades to Truecolor pass-through if depth is Truecolor") {
    assert(colorToAnsi(Color.Indexed(123), isBg = false, ColorDepth.Truecolor) == s"[38;5;123m")
  }

  // -- truecolor ------------------------------------------------------------

  test("Rgb on Truecolor → 38;2;r;g;b") {
    assert(colorToAnsi(Color.Rgb(10, 20, 30), isBg = false, ColorDepth.Truecolor) == s"[38;2;10;20;30m")
    assert(colorToAnsi(Color.Rgb(10, 20, 30), isBg = true, ColorDepth.Truecolor) == s"[48;2;10;20;30m")
  }

  test("Rgb clamps components to 0..255") {
    assert(colorToAnsi(Color.Rgb(-1, 300, 128), isBg = false, ColorDepth.Truecolor) == s"[38;2;0;255;128m")
  }

  test("Rgb(255,0,0) on Indexed256 maps to a red in the cube") {
    val out = colorToAnsi(Color.Rgb(255, 0, 0), isBg = false, ColorDepth.Indexed256)
    // 196 is the cube entry for r=5, g=0, b=0 → 16 + 36*5 = 196.
    assert(out == s"[38;5;196m", s"got $out")
  }

  test("Rgb(255,0,0) on Ansi8 nearest to Red") {
    assert(colorToAnsi(Color.Rgb(255, 0, 0), isBg = false, ColorDepth.Ansi8) == s"[31m")
  }

  test("Rgb(0,0,0) on Ansi8 nearest to Black") {
    assert(colorToAnsi(Color.Rgb(0, 0, 0), isBg = false, ColorDepth.Ansi8) == s"[30m")
  }

  test("Rgb(255,255,255) on Ansi8 nearest to White") {
    assert(colorToAnsi(Color.Rgb(255, 255, 255), isBg = false, ColorDepth.Ansi8) == s"[37m")
  }

  test("Rgb(255,255,255) on Ansi16 chooses BrightWhite (97)") {
    assert(colorToAnsi(Color.Rgb(255, 255, 255), isBg = false, ColorDepth.Ansi16) == s"[97m")
  }

  // -- helpers --------------------------------------------------------------

  test("Color.indexedToRgb spot-checks") {
    assert(Color.indexedToRgb(0) == (0, 0, 0))         // Black named
    assert(Color.indexedToRgb(15) == (255, 255, 255))  // BrightWhite
    assert(Color.indexedToRgb(196) == (255, 0, 0))     // cube vivid red
    assert(Color.indexedToRgb(232) == (8, 8, 8))       // grayscale start
    assert(Color.indexedToRgb(255) == (238, 238, 238)) // grayscale end
  }

  test("Color.nearestIndexed picks the cube entry for a vivid color") {
    assert(Color.nearestIndexed(255, 0, 0) == 196)
    assert(Color.nearestIndexed(0, 0, 0) == 16)
  }

  test("Color.nearestIndexed picks the grayscale ramp for a gray") {
    val n = Color.nearestIndexed(128, 128, 128)
    assert(n >= 232 && n <= 255, s"expected grayscale, got $n")
  }
