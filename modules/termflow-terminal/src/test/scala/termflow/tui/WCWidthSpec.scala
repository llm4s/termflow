package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class WCWidthSpec extends AnyFunSuite:

  test("ASCII printable code points are width 1") {
    assert(WCWidth.codePointWidth('A') == 1)
    assert(WCWidth.codePointWidth(' ') == 1)
    assert(WCWidth.codePointWidth('~') == 1)
  }

  test("control characters report -1") {
    assert(WCWidth.codePointWidth(0x07) == -1) // BEL
    assert(WCWidth.codePointWidth(0x1b) == -1) // ESC
    assert(WCWidth.codePointWidth(0x7f) == -1) // DEL
  }

  test("CJK characters are width 2") {
    assert(WCWidth.codePointWidth('中') == 2) // U+4E2D
    assert(WCWidth.codePointWidth('日') == 2)
    assert(WCWidth.codePointWidth('한') == 2)    // Hangul syllable
    assert(WCWidth.codePointWidth(0x3042) == 2) // ぁ Hiragana
  }

  test("Fullwidth ASCII variants are width 2") {
    assert(WCWidth.codePointWidth(0xff21) == 2) // Ａ fullwidth A
    assert(WCWidth.codePointWidth(0xff10) == 2) // ０ fullwidth 0
  }

  test("emoji code points are width 2") {
    assert(WCWidth.codePointWidth(0x1f600) == 2) // 😀
    assert(WCWidth.codePointWidth(0x1f389) == 2) // 🎉
    assert(WCWidth.codePointWidth(0x1f680) == 2) // 🚀
  }

  test("combining marks and ZWJ are width 0") {
    assert(WCWidth.codePointWidth(0x0301) == 0) // combining acute accent
    assert(WCWidth.codePointWidth(0x200d) == 0) // ZWJ
    assert(WCWidth.codePointWidth(0xfe0f) == 0) // VS16 (emoji presentation)
    assert(WCWidth.codePointWidth(0x200b) == 0) // zero-width space
  }

  test("stringWidth sums code-point widths and ignores controls") {
    assert(WCWidth.stringWidth("hello") == 5)
    assert(WCWidth.stringWidth("中国") == 4)
    assert(WCWidth.stringWidth("a中b") == 4)          // 1 + 2 + 1
    assert(WCWidth.stringWidth("helloworld") == 10) // ESC swallowed
    assert(WCWidth.stringWidth("é") == 1)           // e + combining acute
    assert(WCWidth.stringWidth("") == 0)
  }

  test("stringWidth handles surrogate-pair emoji") {
    val rocket = new String(Character.toChars(0x1f680))
    assert(rocket.length == 2)               // two Java chars
    assert(WCWidth.stringWidth(rocket) == 2) // one wide column pair
  }
