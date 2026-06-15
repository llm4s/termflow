package termflow.tui

import org.scalacheck.{ Gen, Shrink }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import termflow.tui.ScreenPrelude.*

/**
 * Property-based companion to [[LayoutSpec]] (issue #142).
 *
 * `LayoutSpec` pins down hand-picked cases; this suite hammers the resolver
 * across arbitrary trees built from the real [[Layout]] surface
 * (`Elem` / `Row` / `Column` / `Spacer` / `Fill` / `Zone` / `Grid` /
 * `Border`) to flush out edge cases — empty children, negative gaps,
 * extreme/zero budgets, deeply nested wrappers — that are easy to miss by
 * enumeration.
 *
 * The brief's invariant sketch named some constructors that don't exist in
 * the v1 DSL (`Pad` / `Clip` / `Scroll` / `Overlay`); the properties below
 * are the faithful equivalents over the constructors that do exist. A
 * custom [[Shrink]] reduces a failing tree toward a minimal subtree.
 */
class LayoutPropSpec extends AnyFunSuite with ScalaCheckPropertyChecks:

  // Run a healthy number of cases per property — layout composition has a
  // wide combinatorial surface and the trees are cheap to resolve.
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 300, maxDiscardedFactor = 5.0)

  // --- leaf VNode generators ---------------------------------------------
  // Leaves are authored at (1,1) with no nested children: their own
  // coordinate is the minimum coordinate in the subtree, which keeps the
  // origin-containment property (P3) clean to reason about.

  private val genText: Gen[VNode] =
    Gen.alphaStr.map(s => TextNode(1.x, 1.y, List(Text(s.take(8), Style()))))

  private val genBox: Gen[VNode] =
    for
      w <- Gen.choose(-3, 16)
      h <- Gen.choose(-3, 8)
    yield BoxNode(1.x, 1.y, w, h, children = Nil)

  private val genInput: Gen[VNode] =
    for
      p  <- Gen.alphaStr.map(_.take(6))
      lw <- Gen.choose(0, 16)
    yield InputNode(1.x, 1.y, p, Style(), lineWidth = lw)

  private val genVNode: Gen[VNode] = Gen.oneOf(genText, genBox, genInput)

  // --- Layout tree generator (bounded depth) -----------------------------

  private val genElem: Gen[Layout] = genVNode.map(Layout.Elem.apply)

  private val genSpacer: Gen[Layout] =
    for
      w <- Gen.choose(-4, 12)
      h <- Gen.choose(-4, 8)
    yield Layout.Spacer(w, h)

  private val genLeaf: Gen[Layout] = Gen.oneOf(genElem, genSpacer)

  private def genGridCell(depth: Int): Gen[GridCell] =
    for
      content <- genLayout(depth - 1)
      cs      <- Gen.choose(1, 3)
      rs      <- Gen.choose(1, 2)
    yield GridCell(content, cs, rs)

  /** Arbitrary layout tree with depth bounded by `depth`. */
  private def genLayout(depth: Int): Gen[Layout] =
    if depth <= 0 then genLeaf
    else
      Gen.frequency(
        4 -> genLeaf,
        3 -> genContainer(genLayout(depth - 1), Layout.Row.apply),
        3 -> genContainer(genLayout(depth - 1), Layout.Column.apply),
        2 -> genLayout(depth - 1).map(Layout.Fill.apply),
        1 -> (for
          id <- Gen.alphaStr.map(_.take(4))
          c  <- genLayout(depth - 1)
        yield Layout.Zone(id, c)),
        2 -> (for
          cols  <- Gen.choose(1, 4)
          rg    <- Gen.choose(-1, 3)
          cg    <- Gen.choose(-1, 3)
          n     <- Gen.choose(0, 5)
          cells <- Gen.listOfN(n, genGridCell(depth))
        yield Layout.Grid(cols, rg, cg, cells)),
        2 -> genBorder(depth)
      )

  private def genContainer(childGen: Gen[Layout], mk: (Int, List[Layout]) => Layout): Gen[Layout] =
    for
      gap <- Gen.choose(-3, 5)
      n   <- Gen.choose(0, 4)
      cs  <- Gen.listOfN(n, childGen)
    yield mk(gap, cs)

  private def genBorder(depth: Int): Gen[Layout] =
    def zone: Gen[Option[Layout]] = Gen.option(genLayout(depth - 1))
    for
      t   <- zone
      l   <- zone
      c   <- zone
      r   <- zone
      b   <- zone
      gap <- Gen.choose(-1, 4)
    yield Layout.Border(t, l, c, r, b, gap)

  private val genTree: Gen[Layout] = genLayout(depth = 3)

  /**
   * Flow-only tree: `Row` / `Column` / `Fill` / `Zone` over leaves, with no
   * `Grid` or `Border`. These primitives only ever advance their layout
   * cursors forward from the origin, so they guarantee origin-containment
   * (P3) — unlike `Grid` (no column compaction for spanning cells) and
   * `Border` (right/bottom-edge pinning), which can emit coordinates before
   * the origin under a starving budget. See DECISIONS.md.
   */
  private def genFlow(depth: Int): Gen[Layout] =
    if depth <= 0 then genLeaf
    else
      Gen.frequency(
        4 -> genLeaf,
        3 -> genContainer(genFlow(depth - 1), Layout.Row.apply),
        3 -> genContainer(genFlow(depth - 1), Layout.Column.apply),
        2 -> genFlow(depth - 1).map(Layout.Fill.apply),
        1 -> (for
          id <- Gen.alphaStr.map(_.take(4))
          c  <- genFlow(depth - 1)
        yield Layout.Zone(id, c))
      )

  private val genFlowTree: Gen[Layout] = genFlow(depth = 4)

  // --- shrinking ----------------------------------------------------------
  // Reduce a failing tree toward a minimal subtree: prefer replacing a
  // container with one of its children, then dropping a single child.

  private def dropOne[A](xs: List[A]): LazyList[List[A]] =
    xs.indices.to(LazyList).map(i => xs.patch(i, Nil, 1))

  given Shrink[Layout] = Shrink.withLazyList {
    case Layout.Row(g, cs)    => cs.to(LazyList) #::: dropOne(cs).map(Layout.Row(g, _))
    case Layout.Column(g, cs) => cs.to(LazyList) #::: dropOne(cs).map(Layout.Column(g, _))
    case Layout.Fill(c)       => LazyList(c)
    case Layout.Zone(_, c)    => LazyList(c)
    case Layout.Grid(cols, rg, cg, cells) =>
      cells.to(LazyList).map(_.content) #::: dropOne(cells).map(Layout.Grid(cols, rg, cg, _))
    case Layout.Border(t, l, c, r, b, _) =>
      List(t, l, c, r, b).flatten.to(LazyList)
    case _ => LazyList.empty
  }

  // --- properties ---------------------------------------------------------

  test("P1: measure is total and non-negative for any generated tree"):
    forAll(genTree) { layout =>
      val (w, h) = layout.measure
      assert(w >= 0, s"width $w < 0 for $layout")
      assert(h >= 0, s"height $h < 0 for $layout")
    }

  test("P2: resolve / resolveTo / resolveTracked never throw for any tree and budget"):
    forAll(genTree, Gen.choose(-2, 200), Gen.choose(-2, 200), Gen.choose(1, 50), Gen.choose(1, 50)) {
      (layout, availW, availH, ox, oy) =>
        val at = Coord(XCoord(ox), YCoord(oy))
        // Unbudgeted and budgeted resolves, plus the hit-test-tracked path.
        val a      = layout.resolve(at)
        val b      = Layout.resolveTo(layout, at, availW, availH)
        val (c, _) = Layout.resolveTracked[Any](layout, at, availW, availH)
        assert(a != null && b != null && c != null)
    }

  test("P3: a flow layout (Row/Column/Fill/Zone) never places a node before the requested origin"):
    // Flow primitives only ever advance their cursors forward from the
    // origin (gaps clamp to >= 0, measured sizes are >= 0), so resolved
    // nodes are contained on the top/left edge under any budget. Grid and
    // Border are deliberately excluded: a spanning Grid cell can be starved
    // to near-zero width and Border pins its right/bottom zone to the far
    // edge, so either can emit a coordinate before the origin — documented
    // overflow / no-compaction territory, out of scope for v1 (DECISIONS.md).
    forAll(genFlowTree, Gen.choose(-2, 80), Gen.choose(-2, 80), Gen.choose(1, 50), Gen.choose(1, 50)) {
      (layout, availW, availH, ox, oy) =>
        val at = Coord(XCoord(ox), YCoord(oy))

        def assertContained(nodes: List[VNode]): Unit = nodes.foreach { n =>
          assert(n.x.value >= ox, s"node ${n.x.value} placed left of origin $ox: $n")
          assert(n.y.value >= oy, s"node ${n.y.value} placed above origin $oy: $n")
        }

        assertContained(layout.resolve(at))                           // unbudgeted
        assertContained(Layout.resolveTo(layout, at, availW, availH)) // budgeted
    }

  // A Row/Column whose children are all Fill: every resolved BoxNode is a
  // flex allocation, so the main-axis sizes are directly observable.
  private val genFlexBox: Gen[Layout] =
    for
      w <- Gen.choose(0, 20)
      h <- Gen.choose(0, 12)
    yield Layout.Fill(Layout.Elem(BoxNode(1.x, 1.y, w, h, children = Nil)))

  test("P4: sum of flex (Fill) children's main-axis sizes never exceeds the parent's main-axis budget"):
    forAll(Gen.listOf(genFlexBox), Gen.choose(-3, 6), Gen.choose(0, 300)) { (children, gap, budget) =>
      // Row: major axis is width.
      val rowBoxes = Layout
        .resolveTo(Layout.Row(gap, children), Coord(XCoord(1), YCoord(1)), budget, 10)
        .collect { case b: BoxNode => b.width }
      assert(rowBoxes.sum <= budget, s"Row flex widths ${rowBoxes.sum} > budget $budget")
      assert(rowBoxes.forall(_ >= 0))

      // Column: major axis is height.
      val colBoxes = Layout
        .resolveTo(Layout.Column(gap, children), Coord(XCoord(1), YCoord(1)), 10, budget)
        .collect { case b: BoxNode => b.height }
      assert(colBoxes.sum <= budget, s"Column flex heights ${colBoxes.sum} > budget $budget")
      assert(colBoxes.forall(_ >= 0))
    }

  test("P5: Zone wrapping is transparent — same measure and same resolved nodes as its content"):
    forAll(genTree, Gen.choose(-2, 200), Gen.choose(-2, 200)) { (inner, availW, availH) =>
      val wrapped = Layout.Zone("id", inner)
      assert(wrapped.measure == inner.measure)
      val at = Coord(XCoord(1), YCoord(1))
      assert(Layout.resolveTo(wrapped, at, availW, availH) == Layout.resolveTo(inner, at, availW, availH))
    }
