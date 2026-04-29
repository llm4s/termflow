package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

class TreeSpec extends AnyFunSuite:

  given Theme = Theme.dark

  /** A tiny tree shape for the tests. */
  sealed trait Node:
    def name: String
  final case class Branch(name: String, kids: Vector[Node]) extends Node
  final case class Leaf(name: String)                       extends Node

  given Tree.Children[Node, String] with
    def id(n: Node): String = n.name
    def kids(n: Node): Vector[Node] = n match
      case Branch(_, k) => k
      case _: Leaf      => Vector.empty

  private val sampleTree: Vector[Node] = Vector(
    Branch(
      "src",
      Vector(
        Leaf("Main.scala"),
        Branch("util", Vector(Leaf("Helpers.scala")))
      )
    ),
    Leaf("README.md")
  )

  private def textsOf(nodes: List[VNode]): List[String] = nodes.map {
    case TextNode(_, _, runs) => runs.map(_.txt).mkString
    case other                => fail(s"expected TextNode, got $other")
  }

  // ---- visibleRows --------------------------------------------------------

  test("visibleRows shows only roots when nothing is expanded") {
    val rows = Tree.visibleRows(sampleTree, expanded = Set.empty[String])
    assert(rows.map(_.id) == Vector("src", "README.md"))
    assert(rows.head.depth == 0)
    assert(rows.head.hasChildren)
    assert(!rows.head.expanded)
  }

  test("visibleRows expands a single node when its id is in the expanded set") {
    val rows = Tree.visibleRows(sampleTree, expanded = Set("src"))
    assert(rows.map(_.id) == Vector("src", "Main.scala", "util", "README.md"))
    assert(rows(2).depth == 1)
    assert(rows(2).hasChildren)
    assert(!rows(2).expanded)
  }

  test("visibleRows expands transitively when ancestors are open") {
    val rows = Tree.visibleRows(sampleTree, expanded = Set("src", "util"))
    assert(rows.map(_.id) == Vector("src", "Main.scala", "util", "Helpers.scala", "README.md"))
    assert(rows.find(_.id == "Helpers.scala").exists(_.depth == 2))
  }

  test("visibleRows reports leaf nodes as hasChildren = false") {
    val rows = Tree.visibleRows(Vector[Node](Leaf("only")), expanded = Set.empty[String])
    assert(!rows.head.hasChildren)
  }

  // ---- apply --------------------------------------------------------------

  test("apply renders one row per visible node with the right indentation + glyph") {
    val nodes = Tree(
      roots = sampleTree,
      expanded = Set("src"),
      selectedIndex = -1,
      render = (_: Node).name
    )
    val texts = textsOf(nodes)
    // `[-] ` for expanded internal, `[+] ` for collapsed, four spaces for leaves.
    assert(texts(0) == "[-] src")
    assert(texts(1) == "      Main.scala", s"expected depth-1 leaf indented + glyph, got: '${texts(1)}'")
    assert(texts(2) == "  [+] util")
    // README.md is at depth 0 — no per-depth pad, just the 4-cell leaf glyph.
    assert(texts(3) == "    README.md")
  }

  test("unicode flag is ignored — glyphs are now ASCII by default") {
    val nodes = Tree(
      roots = sampleTree,
      expanded = Set("src"),
      selectedIndex = -1,
      render = (_: Node).name,
      unicode = false
    )
    val texts = textsOf(nodes)
    assert(texts(0).startsWith("[-] src"))
    assert(texts(2).contains("[+] util"), s"got: '${texts(2)}'")
  }

  test("selectedIndex highlights exactly one row in the theme primary slot") {
    val nodes = Tree(
      roots = sampleTree,
      expanded = Set.empty[String],
      selectedIndex = 1,
      render = (_: Node).name
    )
    val styles = nodes.map {
      case TextNode(_, _, runs) => runs.head.style
      case _                    => fail("expected TextNode")
    }
    assert(!styles(0).bold, "non-selected row stays unbold")
    assert(styles(1).bold, "selected row is bold")
  }

  test("selectedIndex out of range produces no highlighted rows") {
    val nodes = Tree(
      roots = Vector[Node](Leaf("only")),
      expanded = Set.empty[String],
      selectedIndex = 99,
      render = (_: Node).name
    )
    nodes.foreach {
      case TextNode(_, _, runs) => assert(!runs.head.style.bold)
      case _                    => fail("expected TextNode")
    }
  }

  test("rows are positioned at `at` and one row apart") {
    val nodes = Tree(
      roots = sampleTree,
      expanded = Set.empty[String],
      selectedIndex = -1,
      render = (_: Node).name,
      at = Coord(5.x, 9.y)
    )
    val ys = nodes.map { case TextNode(_, y, _) => y.value; case _ => fail() }
    val xs = nodes.map { case TextNode(x, _, _) => x.value; case _ => fail() }
    assert(ys == List(9, 10))
    assert(xs.forall(_ == 5))
  }

  test("custom indentWidth changes the per-level padding") {
    val nodes = Tree(
      roots = sampleTree,
      expanded = Set("src"),
      selectedIndex = -1,
      render = (_: Node).name,
      indentWidth = 4
    )
    val texts = textsOf(nodes)
    // Level 1 nodes (Main.scala, util) should now have 4 spaces of indent.
    assert(texts(1).startsWith("    "), s"expected 4-space indent, got: '${texts(1)}'")
  }

  // ---- companions ---------------------------------------------------------

  test("Tree.rowWidth = depth*indent + marker + label") {
    // Marker is now 4 cells wide (`[+] ` / `[-] ` / four spaces).
    assert(Tree.rowWidth("name", depth = 0) == 0 + 4 + 4)
    assert(Tree.rowWidth("name", depth = 2, indentWidth = 4) == 8 + 4 + 4)
  }

  // ---- hitTest ------------------------------------------------------------

  test("hitTest returns Chevron for a click on the [+] / [-] glyph cells") {
    val rows = Tree.visibleRows(sampleTree, Set("src"))
    // First row at (1,1) — chevron occupies cols 1..4. Try col 2.
    val r = Tree.hitTest(rows, at = Coord(1.x, 1.y), indentWidth = 2, col = 2, row = 1)
    assert(r.contains(Tree.HitResult.Chevron(0)), s"got $r")
  }

  test("hitTest returns Label for a click past the chevron") {
    val rows = Tree.visibleRows(sampleTree, Set("src"))
    // First row label starts at col 5 ("[-] " is 4 cells from col 1).
    val r = Tree.hitTest(rows, at = Coord(1.x, 1.y), indentWidth = 2, col = 6, row = 1, labelLength = 30)
    assert(r.contains(Tree.HitResult.Label(0)), s"got $r")
  }

  test("hitTest returns None for clicks outside any row") {
    val rows = Tree.visibleRows(sampleTree, Set("src"))
    val r    = Tree.hitTest(rows, at = Coord(1.x, 1.y), indentWidth = 2, col = 2, row = 99)
    assert(r.isEmpty)
  }

  test("hitTest treats leaves as Label-only — no Chevron region") {
    val rows = Tree.visibleRows(sampleTree, Set("src"))
    // Row 1 = "Main.scala" leaf at depth 1; chevron region is empty.
    val rChevron = Tree.hitTest(rows, at = Coord(1.x, 1.y), indentWidth = 2, col = 4, row = 2)
    // Col 4 sits inside the leading padding of the leaf row, before the
    // label column begins — so neither Chevron nor Label fires.
    assert(rChevron.isEmpty || rChevron.exists(_.isInstanceOf[Tree.HitResult.Label]))
    val rLabel = Tree.hitTest(rows, at = Coord(1.x, 1.y), indentWidth = 2, col = 8, row = 2, labelLength = 30)
    assert(rLabel.contains(Tree.HitResult.Label(1)), s"got $rLabel")
  }
