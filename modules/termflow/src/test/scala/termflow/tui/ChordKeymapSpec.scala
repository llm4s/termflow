package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey

class ChordKeymapSpec extends AnyFunSuite:

  enum DemoMsg:
    case Save, Quit, Find

  // ---- ChordKeymap construction + lookup ---------------------------------

  test("empty chord keymap is empty and does not match anything") {
    val k = ChordKeymap.empty[DemoMsg]
    assert(k.isEmpty)
    assert(k.lookup(Vector(InputKey.Ctrl('X'))).isEmpty)
  }

  test("bind installs a multi-key sequence") {
    val k = ChordKeymap
      .empty[DemoMsg]
      .bind(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('S')), DemoMsg.Save)
    assert(k.size == 1)
    assert(k.lookup(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('S'))) == Some(DemoMsg.Save))
  }

  test("bind(key, msg) is shorthand for a one-key chord") {
    val k = ChordKeymap.empty[DemoMsg].bind(InputKey.CharKey('q'), DemoMsg.Quit)
    assert(k.lookup(Vector(InputKey.CharKey('q'))) == Some(DemoMsg.Quit))
  }

  test("isPrefix is true for any strict prefix of a bound chord") {
    val k = ChordKeymap
      .empty[DemoMsg]
      .bind(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('C')), DemoMsg.Quit)
    assert(k.isPrefix(Vector(InputKey.Ctrl('X'))))
    assert(!k.isPrefix(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('C'))))
  }

  test("fromKeymap promotes a single-key Keymap") {
    val flat = Keymap[DemoMsg](InputKey.CharKey('q') -> DemoMsg.Quit)
    val k    = ChordKeymap.fromKeymap(flat)
    assert(k.lookup(Vector(InputKey.CharKey('q'))) == Some(DemoMsg.Quit))
  }

  // ---- step ---------------------------------------------------------------

  test("step on a complete one-key chord resolves immediately") {
    val k = ChordKeymap.empty[DemoMsg].bind(InputKey.CharKey('q'), DemoMsg.Quit)
    val r = k.step(ChordState(), InputKey.CharKey('q'))
    r match
      case ChordResult.Resolved(_, msg) => assert(msg == DemoMsg.Quit)
      case other                        => fail(s"expected Resolved, got $other")
  }

  test("step on a prefix returns Pending; second step resolves the chord") {
    val k = ChordKeymap
      .empty[DemoMsg]
      .bind(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('S')), DemoMsg.Save)
    val r1 = k.step(ChordState(), InputKey.Ctrl('X'))
    val pending = r1 match
      case ChordResult.Pending(s) => s
      case other                  => fail(s"expected Pending, got $other")
    val r2 = k.step(pending, InputKey.Ctrl('S'))
    r2 match
      case ChordResult.Resolved(_, msg) => assert(msg == DemoMsg.Save)
      case other                        => fail(s"expected Resolved, got $other")
  }

  test("step on an unknown key from empty state returns NoMatch") {
    val k = ChordKeymap.empty[DemoMsg].bind(InputKey.CharKey('q'), DemoMsg.Quit)
    val r = k.step(ChordState(), InputKey.CharKey('z'))
    r match
      case ChordResult.NoMatch(s, key) =>
        assert(s.isEmpty, "state should be reset after a NoMatch")
        assert(key == InputKey.CharKey('z'))
      case other => fail(s"expected NoMatch, got $other")
  }

  test("a partial chord followed by an unknown key returns NoMatch and resets") {
    val k = ChordKeymap
      .empty[DemoMsg]
      .bind(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('S')), DemoMsg.Save)
    val pending = k.step(ChordState(), InputKey.Ctrl('X')) match
      case ChordResult.Pending(s) => s
      case _                      => fail()
    val r = k.step(pending, InputKey.CharKey('z'))
    r match
      case ChordResult.NoMatch(s, key) =>
        assert(s.isEmpty)
        assert(key == InputKey.CharKey('z'))
      case other => fail(s"expected NoMatch, got $other")
  }

  test("two chords sharing a prefix can both resolve") {
    val k = ChordKeymap
      .empty[DemoMsg]
      .bind(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('S')), DemoMsg.Save)
      .bind(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('C')), DemoMsg.Quit)
    val ps1 = k.step(ChordState(), InputKey.Ctrl('X')) match
      case ChordResult.Pending(s) => s
      case _                      => fail()
    val r1 = k.step(ps1, InputKey.Ctrl('S'))
    assert(r1.isInstanceOf[ChordResult.Resolved[?]])
    val ps2 = k.step(ChordState(), InputKey.Ctrl('X')) match
      case ChordResult.Pending(s) => s
      case _                      => fail()
    val r2 = k.step(ps2, InputKey.Ctrl('C'))
    assert(r2.isInstanceOf[ChordResult.Resolved[?]])
  }

  // ---- merge --------------------------------------------------------------

  test("++ merges with right-side winning") {
    val a = ChordKeymap.empty[DemoMsg].bind(InputKey.CharKey('q'), DemoMsg.Quit)
    val b = ChordKeymap.empty[DemoMsg].bind(InputKey.CharKey('q'), DemoMsg.Save)
    val c = a ++ b
    assert(c.lookup(Vector(InputKey.CharKey('q'))) == Some(DemoMsg.Save))
  }

  // ---- helpEntries --------------------------------------------------------

  test("helpEntries renders chords as space-separated tokens") {
    val k = ChordKeymap
      .empty[DemoMsg]
      .bind(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('S')), DemoMsg.Save)
      .bind(InputKey.CharKey('f'), DemoMsg.Find)
    val entries = k.helpEntries.map(_._1)
    assert(entries.contains("C-x C-s"))
    assert(entries.contains("f"))
  }

  // ---- ModalKeymap --------------------------------------------------------

  enum Mode:
    case Normal, Insert

  test("ModalKeymap dispatches per current mode") {
    val keys = ModalKeymap[Mode, DemoMsg](
      Mode.Normal -> ChordKeymap.empty.bind(InputKey.CharKey('q'), DemoMsg.Quit),
      Mode.Insert -> ChordKeymap.empty.bind(InputKey.Escape, DemoMsg.Save)
    )
    val rN = keys.step(Mode.Normal, ChordState(), InputKey.CharKey('q'))
    rN match
      case ChordResult.Resolved(_, m) => assert(m == DemoMsg.Quit)
      case _                          => fail()
    val rI = keys.step(Mode.Insert, ChordState(), InputKey.CharKey('q'))
    rI match
      case ChordResult.NoMatch(_, _) => succeed
      case _                         => fail()
  }

  test("ModalKeymap.forMode returns empty for an unbound mode") {
    val keys = ModalKeymap.empty[Mode, DemoMsg]
    assert(keys.forMode(Mode.Insert).isEmpty)
  }

  // ---- KeymapHelp.overlay -------------------------------------------------

  test("KeymapHelp.overlay produces a centred Modal overlay listing all chords") {
    given Theme = Theme.dark
    val k = ChordKeymap
      .empty[DemoMsg]
      .bind(InputKey.CharKey('q'), DemoMsg.Quit)
      .bind(InputKey.CharKey('s'), DemoMsg.Save)
    val o = KeymapHelp.overlay(
      "Help",
      k,
      {
        case DemoMsg.Quit => "Quit"
        case DemoMsg.Save => "Save"
        case DemoMsg.Find => "Find"
      }
    )
    assert(o.position == OverlayPosition.Centered)
    assert(o.inputCapture == InputCapture.Modal)
    assert(o.height >= 5, "title + two help rows + borders")
  }

  // ---- renderKey / renderChord -------------------------------------------

  test("renderKey produces readable strings") {
    assert(Keymap.renderKey(InputKey.Ctrl('X')) == "C-x")
    assert(Keymap.renderKey(InputKey.CharKey('a')) == "a")
    assert(Keymap.renderKey(InputKey.CharKey(' ')) == "Space")
    assert(Keymap.renderKey(InputKey.Enter) == "Enter")
  }

  test("renderChord joins multiple keys with spaces") {
    val seq = Vector(InputKey.Ctrl('X'), InputKey.CharKey('a'))
    assert(Keymap.renderChord(seq) == "C-x a")
  }
