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

  test("Keymap.focus binds Tab to next and BackTab (Shift+Tab) to previous"):
    val k = Keymap.focus(next = DemoMsg.NextFocus, previous = DemoMsg.A)
    assert(k.lookup(InputKey.Tab).contains(DemoMsg.NextFocus))
    assert(k.lookup(InputKey.BackTab).contains(DemoMsg.A))
    assert(k.size == 2)

  test("Keymap.focusVertical binds ArrowUp to previous and ArrowDown to next"):
    val k = Keymap.focusVertical(previous = DemoMsg.A, next = DemoMsg.B)
    assert(k.lookup(InputKey.ArrowUp).contains(DemoMsg.A))
    assert(k.lookup(InputKey.ArrowDown).contains(DemoMsg.B))
    assert(k.size == 2)

  test("Keymap.focusHorizontal binds ArrowLeft to previous and ArrowRight to next"):
    val k = Keymap.focusHorizontal(previous = DemoMsg.A, next = DemoMsg.B)
    assert(k.lookup(InputKey.ArrowLeft).contains(DemoMsg.A))
    assert(k.lookup(InputKey.ArrowRight).contains(DemoMsg.B))
    assert(k.size == 2)

  test("focus / focusVertical / focusHorizontal compose orthogonally via ++"):
    val k =
      Keymap.focus(next = DemoMsg.NextFocus, previous = DemoMsg.A) ++
        Keymap.focusVertical(previous = DemoMsg.A, next = DemoMsg.NextFocus) ++
        Keymap.focusHorizontal(previous = DemoMsg.A, next = DemoMsg.NextFocus)
    assert(k.size == 6) // Tab + BackTab + Up + Down + Left + Right
    // Spot-check that all six distinct keys land where expected.
    assert(k.lookup(InputKey.Tab).contains(DemoMsg.NextFocus))
    assert(k.lookup(InputKey.BackTab).contains(DemoMsg.A))
    assert(k.lookup(InputKey.ArrowUp).contains(DemoMsg.A))
    assert(k.lookup(InputKey.ArrowDown).contains(DemoMsg.NextFocus))
    assert(k.lookup(InputKey.ArrowLeft).contains(DemoMsg.A))
    assert(k.lookup(InputKey.ArrowRight).contains(DemoMsg.NextFocus))

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

  test("renderKey covers every InputKey case (incl. NoOp / EndOfInput)"):
    // Spot-check the cases that don't share a common shape, so the match
    // stays exhaustive: bare singletons, parameterised cases, the synthetic
    // NoOp / EndOfInput sinks, and a Modified wrapper.
    assert(Keymap.renderKey(InputKey.Tab) == "Tab")
    assert(Keymap.renderKey(InputKey.BackTab) == "S-Tab")
    assert(Keymap.renderKey(InputKey.CharKey(' ')) == "Space")
    assert(Keymap.renderKey(InputKey.CharKey('q')) == "q")
    assert(Keymap.renderKey(InputKey.Ctrl('C')) == "C-c")
    assert(Keymap.renderKey(InputKey.NoOp) == "NoOp")
    assert(Keymap.renderKey(InputKey.EndOfInput) == "EOF")
    assert(Keymap.renderKey(InputKey.Unknown("xyz")) == "?(xyz)")
    val mods = KeyDecoder.Modifiers(shift = true, alt = false, ctrl = true, meta = false)
    assert(Keymap.renderKey(InputKey.Modified(InputKey.Tab, mods)) == "S-C-Tab")

  test("renderKey produces readable strings for every navigation / function key"):
    assert(Keymap.renderKey(InputKey.Enter) == "Enter")
    assert(Keymap.renderKey(InputKey.Escape) == "Esc")
    assert(Keymap.renderKey(InputKey.Backspace) == "Backspace")
    assert(Keymap.renderKey(InputKey.Delete) == "Delete")
    assert(Keymap.renderKey(InputKey.Insert) == "Insert")
    assert(Keymap.renderKey(InputKey.Home) == "Home")
    assert(Keymap.renderKey(InputKey.End) == "End")
    assert(Keymap.renderKey(InputKey.PageUp) == "PageUp")
    assert(Keymap.renderKey(InputKey.PageDown) == "PageDown")
    assert(Keymap.renderKey(InputKey.ArrowUp) == "Up")
    assert(Keymap.renderKey(InputKey.ArrowDown) == "Down")
    assert(Keymap.renderKey(InputKey.ArrowLeft) == "Left")
    assert(Keymap.renderKey(InputKey.ArrowRight) == "Right")
    assert(Keymap.renderKey(InputKey.F1) == "F1")
    assert(Keymap.renderKey(InputKey.F2) == "F2")
    assert(Keymap.renderKey(InputKey.F3) == "F3")
    assert(Keymap.renderKey(InputKey.F4) == "F4")
    assert(Keymap.renderKey(InputKey.F5) == "F5")
    assert(Keymap.renderKey(InputKey.F6) == "F6")
    assert(Keymap.renderKey(InputKey.F7) == "F7")
    assert(Keymap.renderKey(InputKey.F8) == "F8")
    assert(Keymap.renderKey(InputKey.F9) == "F9")
    assert(Keymap.renderKey(InputKey.F10) == "F10")
    assert(Keymap.renderKey(InputKey.F11) == "F11")
    assert(Keymap.renderKey(InputKey.F12) == "F12")
    assert(Keymap.renderKey(InputKey.Paste("anything")) == "Paste")
    assert(
      Keymap.renderKey(InputKey.Mouse(MouseEvent.Press(MouseButton.Left, 1, 1, KeyDecoder.Modifiers()))) ==
        "Mouse"
    )

  test("renderKey on a Modified wrapper with no modifiers unwraps to the inner key"):
    val empty = KeyDecoder.Modifiers()
    assert(Keymap.renderKey(InputKey.Modified(InputKey.Home, empty)) == "Home")

  test("ChordKeymap.bind on an empty sequence returns the receiver unchanged"):
    val km    = ChordKeymap.empty[DemoMsg]
    val again = km.bind(Vector.empty[InputKey], DemoMsg.Quit)
    assert(again eq km)

  test("ModalKeymap.withMode replaces or extends the keymap for that mode"):
    enum Mode:
      case Normal, Edit
    val k1 = ChordKeymap.empty[DemoMsg].bind(InputKey.CharKey('a'), DemoMsg.A)
    val k2 = ChordKeymap.empty[DemoMsg].bind(InputKey.CharKey('b'), DemoMsg.B)
    val mk = ModalKeymap.empty[Mode, DemoMsg].withMode(Mode.Normal, k1).withMode(Mode.Edit, k2)
    assert(mk.helpEntries(Mode.Normal).map(_._2) == List(DemoMsg.A))
    assert(mk.helpEntries(Mode.Edit).map(_._2) == List(DemoMsg.B))

  test("KeymapHelp.overlay renders a centered title plus per-chord rows"):
    val km = ChordKeymap
      .empty[DemoMsg]
      .bind(InputKey.Ctrl('C'), DemoMsg.Quit)
      .bind(Vector(InputKey.Ctrl('X'), InputKey.Ctrl('S')), DemoMsg.A)
    given Theme = Theme.dark
    val ov = KeymapHelp.overlay(
      title = "Help",
      chords = km,
      describe = (msg: DemoMsg) => msg.toString
    )
    assert(ov.position == OverlayPosition.Centered)
    assert(ov.width >= "Help".length + 4)
    assert(ov.height >= 4)

  test("layered keymaps form a baseline plus app-specific overrides"):
    val baseline = Keymap.quit(DemoMsg.Quit) ++
      Keymap.focus(next = DemoMsg.NextFocus, previous = DemoMsg.A)
    val mine = Keymap[DemoMsg](
      InputKey.CharKey('q') -> DemoMsg.A // override quit's q->Quit
    )
    val full = baseline ++ mine
    assert(full.lookup(InputKey.Ctrl('C')).contains(DemoMsg.Quit))    // baseline
    assert(full.lookup(InputKey.Tab).contains(DemoMsg.NextFocus))     // baseline
    assert(full.lookup(InputKey.CharKey('q')).contains(DemoMsg.A))    // override
    assert(full.lookup(InputKey.CharKey('Q')).contains(DemoMsg.Quit)) // baseline preserved
