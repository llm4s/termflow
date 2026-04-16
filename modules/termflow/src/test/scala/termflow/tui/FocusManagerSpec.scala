package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class FocusManagerSpec extends AnyFunSuite:

  private val A = FocusId("a")
  private val B = FocusId("b")
  private val C = FocusId("c")

  test("FocusId equality is based on the underlying string"):
    assert(FocusId("x") == FocusId("x"))
    assert(FocusId("x") != FocusId("y"))
    assert(FocusId("x").value == "x")

  test("FocusManager.empty has no ids and no current focus"):
    val fm = FocusManager.empty
    assert(fm.ids.isEmpty)
    assert(fm.current.isEmpty)

  test("apply with non-empty ids picks the first as initially focused"):
    val fm = FocusManager(Vector(A, B, C))
    assert(fm.current.contains(A))

  test("apply with an empty vector leaves current focus as None"):
    val fm = FocusManager(Vector.empty)
    assert(fm.current.isEmpty)

  test("isFocused returns true only for the current id"):
    val fm = FocusManager(Vector(A, B))
    assert(fm.isFocused(A))
    assert(!fm.isFocused(B))

  test("next cycles forward and wraps at the end"):
    val fm0 = FocusManager(Vector(A, B, C))
    val fm1 = fm0.next
    val fm2 = fm1.next
    val fm3 = fm2.next
    assert(fm0.current.contains(A))
    assert(fm1.current.contains(B))
    assert(fm2.current.contains(C))
    assert(fm3.current.contains(A)) // wrap

  test("previous cycles backward and wraps at the start"):
    val fm0 = FocusManager(Vector(A, B, C))
    val fm1 = fm0.previous
    val fm2 = fm1.previous
    assert(fm1.current.contains(C)) // wrap
    assert(fm2.current.contains(B))

  test("next on an empty manager is a no-op"):
    assert(FocusManager.empty.next == FocusManager.empty)
    assert(FocusManager.empty.previous == FocusManager.empty)

  test("focus(id) sets focus when id is in the order"):
    val fm = FocusManager(Vector(A, B, C)).focus(C)
    assert(fm.current.contains(C))

  test("focus(id) is a no-op for an unknown id"):
    val fm0 = FocusManager(Vector(A, B))
    val fm1 = fm0.focus(C)
    assert(fm1.current.contains(A)) // unchanged

  test("clear removes the current focus without removing ids"):
    val fm = FocusManager(Vector(A, B)).clear
    assert(fm.current.isEmpty)
    assert(fm.ids == Vector(A, B))

  test("withIds preserves the current focus when still present"):
    val fm  = FocusManager(Vector(A, B, C)).focus(B)
    val fm2 = fm.withIds(Vector(B, A))
    assert(fm2.current.contains(B))
    assert(fm2.ids == Vector(B, A))

  test("withIds resets focus to the first id when the previous current is gone"):
    val fm  = FocusManager(Vector(A, B, C)).focus(C)
    val fm2 = fm.withIds(Vector(A, B))
    assert(fm2.current.contains(A))

  test("withIds clears focus when given an empty vector"):
    val fm = FocusManager(Vector(A, B)).withIds(Vector.empty)
    assert(fm.current.isEmpty)
    assert(fm.ids.isEmpty)

  test("next falls back to the first id if current isn't in ids"):
    // Manually construct a manager whose current is stale relative to ids.
    val fm = FocusManager(Vector(A, B), Some(C))
    assert(fm.next.current.contains(A))

  test("previous falls back to the last id if current isn't in ids"):
    val fm = FocusManager(Vector(A, B), Some(C))
    assert(fm.previous.current.contains(B))

  test("FocusManager is purely immutable — all transitions return new values"):
    val fm0 = FocusManager(Vector(A, B))
    val fm1 = fm0.next
    val fm2 = fm0.focus(B)
    // Receiver unchanged across both transitions.
    assert(fm0.current.contains(A))
    assert(fm1.current.contains(B))
    assert(fm2.current.contains(B))
