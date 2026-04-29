package termflow.apps.editor

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.KeySim
import termflow.testkit.TuiTestDriver
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.Sub

import java.nio.file.Files
import java.nio.file.Path
import scala.compiletime.uninitialized

class EditorAppSpec extends AnyFunSuite with BeforeAndAfterAll:

  private var tmpDir: Path = uninitialized

  override def beforeAll(): Unit =
    tmpDir = Files.createTempDirectory("termflow-editor-spec-")
    val _ = Files.writeString(tmpDir.resolve("hello.txt"), "hello world\nsecond line\n")

  override def afterAll(): Unit =
    if tmpDir != null then
      Files.list(tmpDir).iterator.forEachRemaining { p =>
        val _ = Files.deleteIfExists(p)
      }
      val _ = Files.deleteIfExists(tmpDir)

  private def freshModel: EditorApp.Model =
    EditorApp.initialModel(width = 100, height = 30, input = Sub.NoSub)

  private def step(m: EditorApp.Model, key: InputKey): EditorApp.Model =
    EditorApp.step(m, EditorApp.Msg.KeyPressed(key)) match
      case EditorApp.StepResult.StayInModel(next) => next
      case EditorApp.StepResult.ExitNow(next)     => next

  // ---- Initial state ------------------------------------------------------

  test("initial model has a single untitled buffer in editor focus"):
    val m = freshModel
    assert(m.buffers.size == 1)
    assert(m.buffers.head.path.isEmpty)
    assert(m.active == 0)
    assert(m.focus == EditorApp.Focus.Editor)
    assert(m.dialog.isEmpty)

  // ---- Buffer ops ---------------------------------------------------------

  test("Ctrl+N creates a new buffer and focuses it"):
    val m  = freshModel
    val m1 = step(m, KeySim.ctrl('N'))
    assert(m1.buffers.size == 2)
    assert(m1.active == 1)
    assert(m1.buffers(1).path.isEmpty)

  test("Ctrl+I (Ctrl-Tab) cycles to the next buffer"):
    val m  = freshModel
    val m1 = step(m, KeySim.ctrl('N'))
    assert(m1.active == 1)
    val m2 = step(m1, KeySim.ctrl('I'))
    assert(m2.active == 0)

  test("Ctrl+W on the only buffer leaves a fresh untitled buffer"):
    val m = freshModel
    // Make it dirty first.
    val m1 = step(m, KeySim.char('x'))
    assert(m1.buffers.head.dirty)
    val m2 = step(m1, KeySim.ctrl('W'))
    assert(m2.buffers.size == 1)
    assert(!m2.buffers.head.dirty)

  // ---- Editor key forwarding ---------------------------------------------

  test("printable characters are forwarded to the active buffer"):
    val m  = freshModel
    val m1 = "hi".foldLeft(m)((acc, c) => step(acc, KeySim.char(c)))
    assert(m1.activeBuffer.state.text == "hi")
    assert(m1.activeBuffer.dirty)

  test("Enter inside the editor produces a new line, not a quit"):
    val m  = freshModel
    val m1 = step(m, KeySim.char('a'))
    val m2 = step(m1, KeySim.Enter)
    val m3 = step(m2, KeySim.char('b'))
    assert(m3.activeBuffer.state.text == "a\nb")

  // ---- Open / Save -------------------------------------------------------

  test("Ctrl+O opens the OpenPath dialog; typed text accumulates; Enter loads file"):
    val m  = freshModel
    val m1 = step(m, KeySim.ctrl('O'))
    assert(m1.dialog.exists(_.isInstanceOf[EditorApp.Dialog.OpenPath]))
    val path = tmpDir.resolve("hello.txt").toAbsolutePath.toString
    val m2   = path.foldLeft(m1)((acc, c) => step(acc, KeySim.char(c)))
    val m3   = step(m2, KeySim.Enter)
    assert(m3.dialog.isEmpty)
    assert(m3.buffers.size == 2)
    assert(m3.activeBuffer.path.isDefined)
    assert(m3.activeBuffer.state.text.contains("hello world"))
    assert(!m3.activeBuffer.dirty)

  test("Esc cancels the OpenPath dialog and leaves buffers untouched"):
    val m  = freshModel
    val m1 = step(m, KeySim.ctrl('O'))
    val m2 = step(m1, KeySim.char('x'))
    val m3 = step(m2, KeySim.Escape)
    assert(m3.dialog.isEmpty)
    assert(m3.buffers.size == 1)

  test("Ctrl+S on an untitled buffer keeps the dirty flag and reports an error"):
    val m  = freshModel
    val m1 = "abc".foldLeft(m)((acc, c) => step(acc, KeySim.char(c)))
    val m2 = step(m1, KeySim.ctrl('S'))
    assert(m2.activeBuffer.dirty, "untitled buffers can't save → dirty stays true")
    assert(m2.status.contains("untitled"))

  test("Ctrl+S on an opened buffer writes the file and clears the dirty flag"):
    // Open the existing test file first.
    val m       = freshModel
    val m1      = step(m, KeySim.ctrl('O'))
    val pathStr = tmpDir.resolve("hello.txt").toAbsolutePath.toString
    val m2      = pathStr.foldLeft(m1)((acc, c) => step(acc, KeySim.char(c)))
    val m3      = step(m2, KeySim.Enter)
    // Make a tiny edit.
    val m4 = step(m3, KeySim.char('!'))
    assert(m4.activeBuffer.dirty)
    val m5 = step(m4, KeySim.ctrl('S'))
    assert(!m5.activeBuffer.dirty)
    val onDisk = Files.readString(tmpDir.resolve("hello.txt"))
    assert(onDisk.contains("!"), s"expected file to contain edit '!', got: ${onDisk.take(60)}")

  // ---- Quit handling ------------------------------------------------------

  test("Ctrl+Q with no dirty buffers exits immediately via the App"):
    val d = TuiTestDriver(EditorApp.App, width = 80, height = 24)
    d.init()
    d.send(EditorApp.Msg.KeyPressed(KeySim.ctrl('Q')))
    assert(d.exited, "Ctrl+Q should exit when nothing's dirty")

  test("Ctrl+Q with a dirty buffer opens a confirm dialog instead of quitting"):
    val d = TuiTestDriver(EditorApp.App, width = 80, height = 24)
    d.init()
    d.send(EditorApp.Msg.KeyPressed(KeySim.char('x'))) // mark dirty
    d.send(EditorApp.Msg.KeyPressed(KeySim.ctrl('Q')))
    assert(!d.exited)
    assert(d.model.dialog.exists(_.isInstanceOf[EditorApp.Dialog.ConfirmQuit]))

  test("ConfirmQuit dialog: Tab flips focus, Enter on Yes commits and closes the dialog"):
    val d = TuiTestDriver(EditorApp.App, width = 80, height = 24)
    d.init()
    d.send(EditorApp.Msg.KeyPressed(KeySim.char('x')))
    d.send(EditorApp.Msg.KeyPressed(KeySim.ctrl('Q')))
    // Initial yesFocused = false; Tab → true.
    d.send(EditorApp.Msg.KeyPressed(KeySim.Tab))
    d.send(EditorApp.Msg.KeyPressed(KeySim.Enter))
    assert(d.model.dialog.isEmpty)
    assert(d.model.status == "quit")

  // ---- Menu integration --------------------------------------------------

  test("F2 moves focus to the menu bar; Esc returns to the editor"):
    val m  = freshModel
    val m1 = step(m, KeySim.f(2))
    assert(m1.focus == EditorApp.Focus.Menu)
    val m2 = step(m1, KeySim.Escape)
    assert(m2.focus == EditorApp.Focus.Editor)

  test("Menu File → New picks the Msg via MenuBar.handleKey and creates a buffer"):
    val m  = freshModel
    val m1 = step(m, KeySim.f(2))   // focus menu
    val m2 = step(m1, KeySim.Enter) // open File menu, cursor at item 0 (New)
    val m3 = step(m2, KeySim.Enter) // pick New
    assert(m3.buffers.size == 2)
    assert(m3.focus == EditorApp.Focus.Editor)
