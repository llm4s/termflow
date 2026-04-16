package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey

class KeymapSpec extends AnyFunSuite:

  enum DemoMsg:
    case A, B, Quit, NextFocus

  test("empty keymap has no bindings and lookup always returns None"):
    val k = Keymap.empty[DemoMsg]
    assert(k.isEmpty)
    assert(k.size == 0)
    assert(k.lookup(InputKey.Enter).isEmpty)

  test("apply with varargs builds a keymap from explicit bindings"):
    val k = Keymap(
      InputKey.Enter        -> DemoMsg.A,
      InputKey.CharKey('q') -> DemoMsg.Quit
    )
    assert(k.size == 2)
    assert(k.lookup(InputKey.Enter).contains(DemoMsg.A))
    assert(k.lookup(InputKey.CharKey('q')).contains(DemoMsg.Quit))
    assert(k.lookup(InputKey.CharKey('z')).isEmpty)

  test("+ adds a single binding without disturbing existing ones"):
    val k0 = Keymap[DemoMsg](InputKey.Enter -> DemoMsg.A)
    val k1 = k0 + (InputKey.Backspace -> DemoMsg.B)
    assert(k1.size == 2)
    assert(k1.lookup(InputKey.Enter).contains(DemoMsg.A))
    assert(k1.lookup(InputKey.Backspace).contains(DemoMsg.B))

  test("+ replaces an existing binding for the same key"):
    val k = Keymap[DemoMsg](InputKey.Enter -> DemoMsg.A) +
      (InputKey.Enter -> DemoMsg.B)
    assert(k.size == 1)
    assert(k.lookup(InputKey.Enter).contains(DemoMsg.B))

  test("++ merges keymaps with right-side winning on conflict"):
    val left = Keymap[DemoMsg](InputKey.CharKey('q') -> DemoMsg.Quit)
    val right = Keymap[DemoMsg](
      InputKey.CharKey('q') -> DemoMsg.A, // overrides
      InputKey.Enter        -> DemoMsg.B
    )
    val merged = left ++ right
    assert(merged.size == 2)
    assert(merged.lookup(InputKey.CharKey('q')).contains(DemoMsg.A))
    assert(merged.lookup(InputKey.Enter).contains(DemoMsg.B))

  test("Keymap.quit binds Ctrl+C, Escape, q, and Q to the supplied message"):
    val k = Keymap.quit(DemoMsg.Quit)
    assert(k.lookup(InputKey.Ctrl('C')).contains(DemoMsg.Quit))
    assert(k.lookup(InputKey.Escape).contains(DemoMsg.Quit))
    assert(k.lookup(InputKey.CharKey('q')).contains(DemoMsg.Quit))
    assert(k.lookup(InputKey.CharKey('Q')).contains(DemoMsg.Quit))

  test("Keymap.focus binds Tab (Ctrl+I) to the next-focus message"):
    val k = Keymap.focus(next = DemoMsg.NextFocus, previous = DemoMsg.A)
    assert(k.lookup(InputKey.Ctrl('I')).contains(DemoMsg.NextFocus))
    // Shift+Tab is unsupported by KeyDecoder today; the parameter is reserved.
    assert(k.size == 1)

  test("Keymap.editing binds Enter, Backspace, ArrowLeft, ArrowRight"):
    val k = Keymap.editing(
      onEnter = DemoMsg.A,
      onBackspace = DemoMsg.B,
      onLeft = DemoMsg.NextFocus,
      onRight = DemoMsg.Quit
    )
    assert(k.lookup(InputKey.Enter).contains(DemoMsg.A))
    assert(k.lookup(InputKey.Backspace).contains(DemoMsg.B))
    assert(k.lookup(InputKey.ArrowLeft).contains(DemoMsg.NextFocus))
    assert(k.lookup(InputKey.ArrowRight).contains(DemoMsg.Quit))

  test("layered keymaps form a baseline plus app-specific overrides"):
    val baseline = Keymap.quit(DemoMsg.Quit) ++
      Keymap.focus(next = DemoMsg.NextFocus, previous = DemoMsg.A)
    val mine = Keymap[DemoMsg](
      InputKey.CharKey('q') -> DemoMsg.A // override quit's q->Quit
    )
    val full = baseline ++ mine
    assert(full.lookup(InputKey.Ctrl('C')).contains(DemoMsg.Quit))      // baseline
    assert(full.lookup(InputKey.Ctrl('I')).contains(DemoMsg.NextFocus)) // baseline
    assert(full.lookup(InputKey.CharKey('q')).contains(DemoMsg.A))      // override
    assert(full.lookup(InputKey.CharKey('Q')).contains(DemoMsg.Quit))   // baseline preserved
