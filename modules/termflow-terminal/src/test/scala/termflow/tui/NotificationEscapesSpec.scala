package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class NotificationEscapesSpec extends AnyFunSuite:

  private val BEL = "\u0007"
  private val ESC = "\u001b"
  private val ST  = ESC + "\\"

  test("attentionFor Disabled returns None") {
    assert(NotificationEscapes.attentionFor(NotificationKind.Disabled).isEmpty)
  }

  test("attentionFor BellOnly emits BEL") {
    assert(NotificationEscapes.attentionFor(NotificationKind.BellOnly).contains(BEL))
  }

  test("attentionFor ITerm2 emits OSC 1337 RequestAttention plus BEL") {
    val seq = NotificationEscapes.attentionFor(NotificationKind.ITerm2).get
    assert(seq.contains("1337;RequestAttention=yes"))
    assert(seq.endsWith(BEL))
  }

  test("notifyFor Disabled returns None") {
    assert(NotificationEscapes.notifyFor(NotificationKind.Disabled, "t", "b").isEmpty)
  }

  test("notifyFor BellOnly returns BEL regardless of title/body") {
    assert(NotificationEscapes.notifyFor(NotificationKind.BellOnly, "t", "b").contains(BEL))
  }

  test("notifyFor ITerm2 wraps in OSC 9 with 'title: body'") {
    val seq = NotificationEscapes.notifyFor(NotificationKind.ITerm2, "Build", "done").get
    assert(seq == s"${ESC}]9;Build: done${BEL}")
  }

  test("notifyFor ITerm2 omits the colon when title is empty") {
    val seq = NotificationEscapes.notifyFor(NotificationKind.ITerm2, "", "ping").get
    assert(seq == s"${ESC}]9;ping${BEL}")
  }

  test("notifyFor Kitty uses OSC 99 with title metadata and ST terminator") {
    val seq = NotificationEscapes.notifyFor(NotificationKind.Kitty, "Build", "done").get
    assert(seq.startsWith(s"${ESC}]99;"))
    assert(seq.contains("p=title;Build"))
    assert(seq.endsWith(ST))
  }

  test("notifyFor Vte uses OSC 777;notify;title;body BEL") {
    val seq = NotificationEscapes.notifyFor(NotificationKind.Vte, "Build", "done").get
    assert(seq == s"${ESC}]777;notify;Build;done${BEL}")
  }

  test("sanitize strips embedded ESC and BEL so the envelope stays valid") {
    val seq = NotificationEscapes.notifyFor(NotificationKind.Vte, s"a${ESC}b", s"c${BEL}d").get
    assert(seq == s"${ESC}]777;notify;ab;cd${BEL}")
  }
