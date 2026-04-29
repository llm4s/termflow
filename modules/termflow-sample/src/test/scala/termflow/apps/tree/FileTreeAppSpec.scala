package termflow.apps.tree

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.KeySim
import termflow.testkit.TuiTestDriver
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.Sub

import java.nio.file.Files
import java.nio.file.Path

class FileTreeAppSpec extends AnyFunSuite with BeforeAndAfterAll:

  // Build a temp tree:
  //   root/
  //     a.txt
  //     b.txt
  //     sub/
  //       c.txt
  private var root: Path = _

  override def beforeAll(): Unit =
    root = Files.createTempDirectory("termflow-tree-spec-")
    Files.writeString(root.resolve("a.txt"), "alpha\n")
    Files.writeString(root.resolve("b.txt"), "beta\n")
    val sub = Files.createDirectory(root.resolve("sub"))
    Files.writeString(sub.resolve("c.txt"), "gamma\n")

  override def afterAll(): Unit =
    deleteRecursively(root)

  private def deleteRecursively(p: Path): Unit =
    if Files.isDirectory(p) then
      val ds = Files.list(p)
      try ds.iterator.nn.asInstanceOf[java.util.Iterator[Path]].forEachRemaining(deleteRecursively(_))
      finally ds.close()
    Files.deleteIfExists(p)
    ()

  // ---- helpers ------------------------------------------------------------

  private def freshModel: FileTreeApp.Model =
    FileTreeApp.initialModel(root, width = 80, height = 24, input = Sub.NoSub)

  private def step(m: FileTreeApp.Model, key: InputKey): FileTreeApp.Model =
    FileTreeApp.step(m, FileTreeApp.Msg.KeyPressed(key))

  // ---- tests --------------------------------------------------------------

  test("initial model lists the root with the root entry expanded"):
    val m = freshModel
    assert(m.expanded.contains(root.toAbsolutePath.toString))
    val rows = m.visibleRows
    // Root + 3 children (sub/, a.txt, b.txt) — directories sort first.
    assert(rows.size == 4, s"expected 4 rows; got ${rows.size}")
    assert(rows.head.depth == 0)
    assert(rows(1).node.isDir, "first child should be a directory")

  test("ArrowDown / ArrowUp move the selection inside the visible rows"):
    val m  = freshModel
    val m1 = step(m, KeySim.ArrowDown)
    assert(m1.selectedIndex == 1)
    val m2 = step(m1, KeySim.ArrowDown)
    assert(m2.selectedIndex == 2)
    val m3 = step(m2, KeySim.ArrowUp)
    assert(m3.selectedIndex == 1)

  test("ArrowDown clamps at the last visible row"):
    val m    = freshModel
    val rows = m.visibleRows.size
    val m1   = (1 to rows + 5).foldLeft(m)((acc, _) => step(acc, KeySim.ArrowDown))
    assert(m1.selectedIndex == rows - 1)

  test("Enter on a directory expands it and the cache is populated"):
    val m   = freshModel
    val m1  = step(m, KeySim.ArrowDown) // select sub/
    val m2  = step(m1, KeySim.Enter)
    val key = root.resolve("sub").toAbsolutePath.toString
    assert(m2.expanded.contains(key))
    assert(m2.childrenCache.contains(key))
    // sub/ is now visible-expanded → c.txt appears underneath.
    val labels = m2.visibleRows.map(row => Option(row.node.path.getFileName).map(_.toString).getOrElse(""))
    assert(labels.contains("c.txt"))

  test("Enter on a directory toggles back when invoked twice"):
    val m   = freshModel
    val m1  = step(m, KeySim.ArrowDown)
    val m2  = step(m1, KeySim.Enter)
    val m3  = step(m2, KeySim.Enter)
    val key = root.resolve("sub").toAbsolutePath.toString
    assert(!m3.expanded.contains(key))

  test("ArrowLeft on an expanded directory collapses it"):
    val m   = freshModel
    val m1  = step(m, KeySim.ArrowDown)   // sub/
    val m2  = step(m1, KeySim.ArrowRight) // expand
    val key = root.resolve("sub").toAbsolutePath.toString
    assert(m2.expanded.contains(key))
    val m3 = step(m2, KeySim.ArrowLeft)
    assert(!m3.expanded.contains(key))

  test("ArrowLeft on a child jumps focus to its parent"):
    val m  = freshModel
    val m1 = step(m, KeySim.ArrowDown)   // sub/
    val m2 = step(m1, KeySim.ArrowRight) // expand sub
    val m3 = step(m2, KeySim.ArrowDown)  // step into c.txt
    assert(m3.selectedIndex == 2)
    val m4 = step(m3, KeySim.ArrowLeft)
    // After collapse-or-ascend on a leaf, focus returns to its parent.
    assert(m4.selectedIndex == 1, s"expected to land on sub/ (idx 1); got ${m4.selectedIndex}")

  test("'q' triggers Cmd.Exit when driven through the App"):
    val d = TuiTestDriver(FileTreeApp.App, width = 80, height = 24)
    d.init()
    d.send(FileTreeApp.Msg.KeyPressed(KeySim.char('q')))
    assert(d.exited)

  test("Mouse press on the root chevron toggles expand on the root"):
    val m = freshModel
    // Root is rendered at row 4, indented 0 cells, glyph occupies cols 2..5
    // ([+]/[-] is 4 chars but treeOrigin starts at col 2 so chevron is col 2..5).
    val click   = MouseSimRecreate.press(col = 3, row = 4)
    val m1      = step(m, click)
    val rootKey = root.toAbsolutePath.toString
    // Root started expanded; chevron click collapses it.
    assert(!m1.expanded.contains(rootKey))

  // tiny re-export so the test file doesn't depend on MouseSim's package
  private object MouseSimRecreate:
    import termflow.testkit.MouseSim
    def press(col: Int, row: Int): InputKey =
      MouseSim.press(col, row)
