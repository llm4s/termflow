package termflow.apps.catalog

import org.scalatest.funsuite.AnyFunSuite
import termflow.apps.catalog.CatalogDemoApp.*
import termflow.testkit.GoldenSupport
import termflow.testkit.TuiTestDriver
import termflow.tui.KeyDecoder.InputKey

class CatalogDemoAppSpec extends AnyFunSuite with GoldenSupport:

  private val Width  = 96
  private val Height = 24

  private def driver(): TuiTestDriver[Model, Msg] =
    val d = TuiTestDriver(App, width = Width, height = Height)
    d.init()
    d

  private def typeKeys(d: TuiTestDriver[Model, Msg], s: String): Unit =
    s.foreach(c => d.send(Msg.ConsoleInputKey(InputKey.CharKey(c))))

  // --- initial state --------------------------------------------------------

  test("initial state seeds 4 tasks, Medium priority, Task field focused"):
    val d = driver()
    assert(d.model.fm.isFocused(TaskFieldId))
    assert(d.model.tasks.size == 4)
    assert(d.model.priority.value.contains(Priority.Medium))
    assert(d.model.darkTheme)

  // --- focus cycle ---------------------------------------------------------

  test("Tab cycles focus through Task → Priority → Add → Clear → List → Task"):
    val d        = driver()
    val expected = Vector(PriorityId, AddId, ClearId, ListId, TaskFieldId)
    expected.foreach { id =>
      d.send(Msg.NextFocus)
      assert(d.model.fm.current.contains(id), s"expected $id, got ${d.model.fm.current}")
    }

  // --- TextField -----------------------------------------------------------

  test("typing in the Task field fills its buffer"):
    val d = driver()
    typeKeys(d, "hello world")
    assert(d.model.newTask.buffer == "hello world")

  test("Enter in the Task field adds it with the current priority and clears the buffer"):
    val d = driver()
    typeKeys(d, "feed cat")
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.newTask.buffer == "", "buffer should clear after add")
    assert(d.model.tasks.items.contains(Task("feed cat", Priority.Medium)))

  test("adding an empty-string task is ignored"):
    val d      = driver()
    val before = d.model.tasks.size
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.tasks.size == before)

  test("'t' inside the Task field is a printable char, not a theme toggle"):
    val d      = driver()
    val before = d.model.darkTheme
    typeKeys(d, "tomato")
    assert(d.model.newTask.buffer == "tomato")
    assert(d.model.darkTheme == before)

  // --- Select dropdown -----------------------------------------------------

  test("Space on the priority Select opens it; arrows navigate; Enter commits"):
    val d = driver()
    d.send(Msg.NextFocus) // -> Priority
    assert(!d.model.priority.open)
    d.send(Msg.ConsoleInputKey(InputKey.CharKey(' ')))
    assert(d.model.priority.open)
    d.send(Msg.ConsoleInputKey(InputKey.ArrowDown))
    assert(d.model.priority.value.contains(Priority.Low))
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(!d.model.priority.open)
    assert(d.model.priority.value.contains(Priority.Low))

  test("adding a task after changing priority records the new priority"):
    val d = driver()
    d.send(Msg.NextFocus)                         // -> Priority
    d.send(Msg.ConsoleInputKey(InputKey.Enter))   // open
    d.send(Msg.ConsoleInputKey(InputKey.ArrowUp)) // wait — already at Medium(1); Up goes to High(0)
    // Actually default is selectIndex(1) = Medium. Up → High.
    d.send(Msg.ConsoleInputKey(InputKey.Enter)) // commit -> High
    assert(d.model.priority.value.contains(Priority.High))
    d.send(Msg.PrevFocus) // -> Task field
    typeKeys(d, "urgent")
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.tasks.items.contains(Task("urgent", Priority.High)))

  // --- buttons -------------------------------------------------------------

  test("Enter on the Add button with buffer content adds the task"):
    val d = driver()
    typeKeys(d, "fix bug")
    d.send(Msg.NextFocus) // -> Priority
    d.send(Msg.NextFocus) // -> Add
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.tasks.items.exists(_.title == "fix bug"))

  test("Enter on the Clear button empties the tasks list"):
    val d = driver()
    (1 to 3).foreach(_ => d.send(Msg.NextFocus)) // -> Clear
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.tasks.isEmpty)

  // --- ListView ------------------------------------------------------------

  test("ArrowDown in the task list advances selection within the widget (not focus)"):
    val d = driver()
    (1 to 4).foreach(_ => d.send(Msg.NextFocus)) // -> List
    val before = d.model.tasks.selected
    d.send(Msg.ConsoleInputKey(InputKey.ArrowDown))
    assert(d.model.fm.isFocused(ListId), "focus should stay on the list")
    assert(d.model.tasks.selected == before + 1)

  test("Enter on the list removes the selected task"):
    val d = driver()
    (1 to 4).foreach(_ => d.send(Msg.NextFocus)) // -> List
    val targeted = d.model.tasks.selectedItem.get
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(!d.model.tasks.items.contains(targeted))

  test("Enter on the list removes only the selected row when duplicate tasks exist"):
    val d = driver()
    typeKeys(d, "buy groceries")
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    val duplicate = Task("buy groceries", Priority.Medium)
    assert(d.model.tasks.items.count(_ == duplicate) == 2)

    (1 to 4).foreach(_ => d.send(Msg.NextFocus)) // -> List, selected row 0
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.tasks.items.count(_ == duplicate) == 1)

  // --- globals -------------------------------------------------------------

  test("Ctrl+T toggles the theme from anywhere, including inside the Task field"):
    val d      = driver()
    val before = d.model.darkTheme
    d.send(Msg.ConsoleInputKey(InputKey.Ctrl('T')))
    assert(d.model.darkTheme == !before)

  test("Ctrl+C quits from anywhere"):
    val d = driver()
    d.send(Msg.ConsoleInputKey(InputKey.Ctrl('C')))
    assert(d.exited)

  test("'q' on a button row quits (non-text shortcut)"):
    val d = driver()
    (1 to 3).foreach(_ => d.send(Msg.NextFocus)) // -> Add
    d.send(Msg.ConsoleInputKey(InputKey.CharKey('q')))
    assert(d.exited)

  // --- Keymap wiring --------------------------------------------------------

  test("Globals bind Tab / Shift+Tab / Ctrl+T / Ctrl+C / Esc but NOT vertical arrows"):
    val g = CatalogDemoApp.Globals
    assert(g.lookup(InputKey.Ctrl('I')).contains(Msg.NextFocus))
    assert(g.lookup(InputKey.BackTab).contains(Msg.PrevFocus))
    assert(g.lookup(InputKey.Ctrl('T')).contains(Msg.ToggleTheme))
    assert(g.lookup(InputKey.Ctrl('C')).contains(Msg.Quit))
    assert(g.lookup(InputKey.Escape).contains(Msg.Quit))
    // Arrows are NOT global — the list + open Select need them.
    assert(g.lookup(InputKey.ArrowUp).isEmpty)
    assert(g.lookup(InputKey.ArrowDown).isEmpty)

  // --- golden snapshots ----------------------------------------------------

  test("initial frame matches the dark-theme golden"):
    val d = driver()
    assertGoldenFrame(d.frame, "initial-dark")

  test("after an Add cycle: type, Enter, shows updated summary"):
    val d = driver()
    typeKeys(d, "ship v1")
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assertGoldenFrame(d.frame, "after-add")

  test("after navigating to the List and selecting row 1"):
    val d = driver()
    (1 to 4).foreach(_ => d.send(Msg.NextFocus))
    d.send(Msg.ConsoleInputKey(InputKey.ArrowDown))
    assertGoldenFrame(d.frame, "list-row1-focused")
