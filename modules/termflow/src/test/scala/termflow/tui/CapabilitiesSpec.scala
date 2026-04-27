package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class CapabilitiesSpec extends AnyFunSuite:

  test("NO_COLOR forces Mono regardless of TERM/COLORTERM") {
    val caps = Capabilities.detect(
      Map(
        "TERM"      -> "xterm-256color",
        "COLORTERM" -> "truecolor",
        "NO_COLOR"  -> "1"
      )
    )
    assert(caps.colorDepth == ColorDepth.Mono)
  }

  test("COLORTERM=truecolor wins over TERM") {
    val caps = Capabilities.detect(
      Map(
        "TERM"      -> "xterm",
        "COLORTERM" -> "truecolor"
      )
    )
    assert(caps.colorDepth == ColorDepth.Truecolor)
  }

  test("COLORTERM=24bit is also Truecolor") {
    assert(Capabilities.detect(Map("COLORTERM" -> "24bit")).colorDepth == ColorDepth.Truecolor)
  }

  test("TERM=xterm-256color → Indexed256") {
    assert(Capabilities.detect(Map("TERM" -> "xterm-256color")).colorDepth == ColorDepth.Indexed256)
  }

  test("TERM=screen-256color → Indexed256") {
    assert(Capabilities.detect(Map("TERM" -> "screen-256color")).colorDepth == ColorDepth.Indexed256)
  }

  test("TERM=xterm → Ansi16") {
    assert(Capabilities.detect(Map("TERM" -> "xterm")).colorDepth == ColorDepth.Ansi16)
  }

  test("TERM=tmux → Ansi16") {
    assert(Capabilities.detect(Map("TERM" -> "tmux")).colorDepth == ColorDepth.Ansi16)
  }

  test("TERM=ansi → Ansi8") {
    assert(Capabilities.detect(Map("TERM" -> "ansi")).colorDepth == ColorDepth.Ansi8)
  }

  test("TERM=dumb → Mono") {
    assert(Capabilities.detect(Map("TERM" -> "dumb")).colorDepth == ColorDepth.Mono)
  }

  test("Empty TERM → Mono") {
    assert(Capabilities.detect(Map.empty).colorDepth == ColorDepth.Mono)
  }

  test("UTF-8 LANG → unicode = true") {
    assert(Capabilities.detect(Map("LANG" -> "en_US.UTF-8")).unicode)
  }

  test("Non-UTF-8 LANG → unicode = false") {
    assert(!Capabilities.detect(Map("LANG" -> "C")).unicode)
  }

  test("LC_ALL takes precedence over LANG") {
    val caps = Capabilities.detect(Map("LANG" -> "C", "LC_ALL" -> "en_GB.UTF-8"))
    assert(caps.unicode)
  }

  test("xterm family advertises mouse") {
    assert(Capabilities.detect(Map("TERM" -> "xterm")).mouse)
    assert(Capabilities.detect(Map("TERM" -> "xterm-256color")).mouse)
    assert(Capabilities.detect(Map("TERM" -> "screen")).mouse)
    assert(Capabilities.detect(Map("TERM" -> "tmux-256color")).mouse)
    assert(Capabilities.detect(Map("TERM" -> "alacritty")).mouse)
  }

  test("non-xterm TERM does not advertise mouse") {
    assert(!Capabilities.detect(Map("TERM" -> "ansi")).mouse)
    assert(!Capabilities.detect(Map("TERM" -> "vt100")).mouse)
  }

  test("ColorDepth.supports is monotonic") {
    assert(ColorDepth.Truecolor.supports(ColorDepth.Indexed256))
    assert(ColorDepth.Indexed256.supports(ColorDepth.Ansi16))
    assert(ColorDepth.Ansi16.supports(ColorDepth.Ansi8))
    assert(ColorDepth.Ansi8.supports(ColorDepth.Mono))
    assert(!ColorDepth.Mono.supports(ColorDepth.Ansi8))
    assert(!ColorDepth.Ansi16.supports(ColorDepth.Indexed256))
  }

  test("default capabilities are Ansi8 + Unicode + no mouse + extended styles on") {
    val d = Capabilities.default
    assert(d.colorDepth == ColorDepth.Ansi8)
    assert(d.unicode)
    assert(!d.mouse)
    assert(d.extendedStyles)
  }

  test("xterm-family TERM advertises extendedStyles") {
    assert(Capabilities.detect(Map("TERM" -> "xterm")).extendedStyles)
    assert(Capabilities.detect(Map("TERM" -> "xterm-256color")).extendedStyles)
    assert(Capabilities.detect(Map("TERM" -> "tmux")).extendedStyles)
    assert(Capabilities.detect(Map("TERM" -> "alacritty")).extendedStyles)
  }

  test("vt-family / ansi advertise extendedStyles") {
    assert(Capabilities.detect(Map("TERM" -> "vt100")).extendedStyles)
    assert(Capabilities.detect(Map("TERM" -> "ansi")).extendedStyles)
  }

  test("dumb / empty TERM disables extendedStyles") {
    assert(!Capabilities.detect(Map("TERM" -> "dumb")).extendedStyles)
    assert(!Capabilities.detect(Map.empty).extendedStyles)
  }

  test("xterm-family advertises bracketedPaste; dumb / unknown does not") {
    assert(Capabilities.detect(Map("TERM" -> "xterm-256color")).bracketedPaste)
    assert(Capabilities.detect(Map("TERM" -> "tmux")).bracketedPaste)
    assert(!Capabilities.detect(Map("TERM" -> "dumb")).bracketedPaste)
    assert(!Capabilities.detect(Map("TERM" -> "vt100")).bracketedPaste)
    assert(!Capabilities.detect(Map.empty).bracketedPaste)
  }
