package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey

class DevtoolsPanelSpec extends AnyFunSuite:

  given Theme = Theme.dark

  enum DemoMsg:
    case Inc, Dec, Reset

  private def buildHistory(n: Int): Devtools.History[DemoMsg, Int] =
    (1 to n).foldLeft(
      Devtools.History.empty[DemoMsg, Int](capacity = 32).snapshot(0, atMillis = 1000L)
    )((h, i) => h.record(DemoMsg.Inc, i, atMillis = 1000L + i * 50L))

  // --- handleKey -----------------------------------------------------------

  test("handleKey on an empty history is a no-op"):
    val h        = Devtools.History.empty[DemoMsg, Int](8)
    val (c, idx) = Devtools.Panel.handleKey(0, h, InputKey.ArrowDown)
    assert(c == 0 && idx.isEmpty)

  test("ArrowDown advances cursor, clamped to history.size - 1"):
    val h       = buildHistory(3) // frames: snapshot + 3 records -> size 4
    val (c1, _) = Devtools.Panel.handleKey(0, h, InputKey.ArrowDown)
    assert(c1 == 1)
    val (c4, _) = (1 to 10).foldLeft((c1, Option.empty[Int])) { case ((acc, _), _) =>
      Devtools.Panel.handleKey(acc, h, InputKey.ArrowDown)
    }
    assert(c4 == h.size - 1, s"clamped at last row; size=${h.size}")

  test("ArrowUp retreats cursor, clamped to 0"):
    val h = buildHistory(3)
    val (c0, _) = (1 to 5).foldLeft((3, Option.empty[Int])) { case ((acc, _), _) =>
      Devtools.Panel.handleKey(acc, h, InputKey.ArrowUp)
    }
    assert(c0 == 0)

  test("Home / End jump to endpoints"):
    val h       = buildHistory(5)
    val (c0, _) = Devtools.Panel.handleKey(3, h, InputKey.Home)
    assert(c0 == 0)
    val (cN, _) = Devtools.Panel.handleKey(0, h, InputKey.End)
    assert(cN == h.size - 1)

  test("Enter returns the selected frame's lifetime-unique index"):
    val h        = buildHistory(3) // Frames indexed 0,1,2,3
    val (_, idx) = Devtools.Panel.handleKey(cursor = 2, h, InputKey.Enter)
    assert(idx.contains(h.frames(2).index))

  test("Space returns the selected frame's index (same as Enter)"):
    val h        = buildHistory(3)
    val (_, idx) = Devtools.Panel.handleKey(cursor = 1, h, InputKey.CharKey(' '))
    assert(idx.contains(h.frames(1).index))

  test("Enter on an empty history returns None"):
    val h        = Devtools.History.empty[DemoMsg, Int](8)
    val (c, idx) = Devtools.Panel.handleKey(0, h, InputKey.Enter)
    assert(c == 0 && idx.isEmpty)

  test("cursor passed in out-of-range is clamped for both navigation and Enter"):
    val h       = buildHistory(2) // size 3
    val (c1, _) = Devtools.Panel.handleKey(cursor = 99, h, InputKey.ArrowUp)
    // Clamped to (size - 1) = 2 first, then up to 1.
    assert(c1 == 1)
    val (c2, _) = Devtools.Panel.handleKey(cursor = -5, h, InputKey.ArrowDown)
    // Clamped to 0 first, then down to 1.
    assert(c2 == 1)

  test("unknown keys return the (clamped) cursor unchanged and no msg"):
    val h        = buildHistory(3)
    val (c, idx) = Devtools.Panel.handleKey(cursor = 1, h, InputKey.F1)
    assert(c == 1 && idx.isEmpty)

  // --- rewind integration pattern ------------------------------------------

  test("Enter + rewindTo pattern restores the selected frame's model"):
    // Simulate the integration path an app would follow: user picks a
    // frame, app calls history.rewindTo with the returned index.
    val h           = buildHistory(4) // frames: snapshot(0) + Inc(1..4)
    val cursor      = 2               // points at Frame(index=2, msg=Inc, model=2)
    val (_, picked) = Devtools.Panel.handleKey(cursor, h, InputKey.Enter)
    assert(picked.contains(2))
    val rewound = h.rewindTo(picked.get).get
    assert(rewound.size == 3)
    assert(rewound.latest.get.model == 2)
    // Continuing from here gives a fresh index.
    val next = rewound.record(DemoMsg.Reset, 0, atMillis = 9999L)
    assert(next.latest.get.index == 3)

  // --- helpers -------------------------------------------------------------

  test("height is 2 + visibleRows with a clamp of visibleRows >= 1"):
    assert(Devtools.Panel.height(5) == 7)
    assert(Devtools.Panel.height(0) == 3) // clamped to 1
    assert(Devtools.Panel.height(-2) == 3)

  test("width clamps to a minimum of 10"):
    assert(Devtools.Panel.width(40) == 40)
    assert(Devtools.Panel.width(5) == 10)
    assert(Devtools.Panel.width(-1) == 10)

  // --- rendering -----------------------------------------------------------

  private def render(v: VNode, width: Int, height: Int): (Array[Array[Char]], Array[Array[Style]]) =
    val frame = AnsiRenderer.buildFrame(RootNode(width, height, List(v), None))
    val chars = Array.tabulate(height, width)((r, c) => frame.cells(r)(c).ch)
    val style = Array.tabulate(height, width)((r, c) => frame.cells(r)(c).style)
    (chars, style)

  test("view draws title, divider, and frame rows in order"):
    val h       = buildHistory(2)
    val v       = Devtools.Panel.view(h, cursor = 0, width = 40, visibleRows = 3, focused = false)
    val (cs, _) = render(v, 40, Devtools.Panel.height(3))
    // Row 0: title header (starts with the supplied title).
    assert(cs(0).mkString.startsWith("Devtools History"))
    assert(cs(0).mkString.contains(s"3/32")) // 3 frames in a capacity-32 history
    // Row 1: divider.
    assert(cs(1).forall(_ == '─'))
    // Rows 2..4: frame rows — first is the initial snapshot.
    assert(cs(2).mkString.contains("(initial)"))
    // Subsequent rows show the Inc msg and relative timestamps.
    assert(cs(3).mkString.contains("Inc"))
    assert(cs(4).mkString.contains("Inc"))

  test("focused render highlights the cursor row in inverse video"):
    val h            = buildHistory(3)
    val v            = Devtools.Panel.view(h, cursor = 1, width = 40, visibleRows = 4, focused = true)
    val (cs, styles) = render(v, 40, Devtools.Panel.height(4))
    // Row 3 in frame space (body rows start at y=2; cursor=1 means second body row = y=3).
    val cursorRow = 3
    assert(cs(cursorRow).mkString.startsWith("▸ "))
    (0 until 40).foreach(c =>
      assert(styles(cursorRow)(c).bg == Theme.dark.primary, s"cursor row col $c expected inverse bg")
    )
    // Other body rows stay default.
    assert(styles(2)(0).bg == Color.Default)
    assert(styles(4)(0).bg == Color.Default)

  test("unfocused render omits the cursor marker"):
    val h       = buildHistory(3)
    val v       = Devtools.Panel.view(h, cursor = 1, width = 40, visibleRows = 4, focused = false)
    val (cs, _) = render(v, 40, Devtools.Panel.height(4))
    // No ▸ anywhere.
    assert(!cs.exists(_.contains('▸')))

  test("empty history renders the title + blank body"):
    val h       = Devtools.History.empty[DemoMsg, Int](8)
    val v       = Devtools.Panel.view(h, cursor = 0, width = 40, visibleRows = 3, focused = true)
    val (cs, _) = render(v, 40, Devtools.Panel.height(3))
    // Title row shows "(retained/capacity)" — retained is 0.
    assert(cs(0).mkString.contains("0/8"))
    // Body rows should all be blank (40 spaces each).
    (2 until 5).foreach(r => assert(cs(r).mkString == " " * 40))

  test("viewport scrolls when cursor is past the visible window"):
    // 10 frames (size 11 incl. snapshot), visibleRows = 4, cursor at last.
    val h       = buildHistory(10)
    val v       = Devtools.Panel.view(h, cursor = h.size - 1, width = 30, visibleRows = 4, focused = true)
    val (cs, _) = render(v, 30, Devtools.Panel.height(4))
    // The last row of the viewport should be the focused frame with the ▸ marker.
    val lastBodyRow = cs(5).mkString
    assert(lastBodyRow.startsWith("▸ "))
    // The first body row should NOT be the initial snapshot (it scrolled away).
    assert(!cs(2).mkString.contains("(initial)"))

  test("view composes inside Layout.column with the expected measure"):
    val h = buildHistory(3)
    val v = Devtools.Panel.view(h, cursor = 0, width = 40, visibleRows = 5)
    val c = Layout.column(gap = 0)(v)
    assert(c.measure == (40, Devtools.Panel.height(5)))
