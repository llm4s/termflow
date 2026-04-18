package termflow.testkit

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.AnsiRenderer.RenderCell
import termflow.tui.AnsiRenderer.RenderFrame

class GoldenFrameSpec extends AnyFunSuite:

  private def makeFrame(rows: Array[String], cursor: Option[Coord] = None): RenderFrame =
    val height = rows.length
    val width  = if height == 0 then 0 else rows(0).length
    val cells  = Array.tabulate(height, width)((r, c) => RenderCell(rows(r).charAt(c), Style()))
    RenderFrame(width, height, cells, cursor)

  test("serialize emits a width/height header and pipe-wrapped rows"):
    val frame = makeFrame(Array("ab", "cd"))
    val text  = GoldenFrame.serialize(frame)
    assert(text == "# width=2 height=2\n|ab|\n|cd|\n")

  test("serialize includes the cursor line when cursor is set"):
    val frame = makeFrame(Array("xy"), cursor = Some(Coord(XCoord(1), YCoord(1))))
    val text  = GoldenFrame.serialize(frame)
    assert(text == "# width=2 height=1\n# cursor=1,1\n|xy|\n")

  test("serialize preserves trailing spaces inside pipe markers"):
    val frame = makeFrame(Array("a ", "  "))
    val text  = GoldenFrame.serialize(frame)
    assert(text == "# width=2 height=2\n|a |\n|  |\n")

  test("describeDiff flags the first differing row and column"):
    val expected = "# width=3 height=2\n|abc|\n|def|\n"
    val actual   = "# width=3 height=2\n|abc|\n|dXf|\n"
    val diff     = GoldenFrame.describeDiff(expected, actual)
    // split('\n') drops the trailing empty string, so 4 lines become indices 0..2:
    //   0 "# width=3 height=2"
    //   1 "|abc|"
    //   2 "|def|"   <- differs, reported as "line 3"
    assert(diff.contains("line 3"))
    assert(diff.contains("expected: |def|"))
    assert(diff.contains("actual:   |dXf|"))
    assert(diff.contains("^"))

  test("describeDiff handles missing trailing lines"):
    val expected = "a\nb\nc\n"
    val actual   = "a\nb\n"
    val diff     = GoldenFrame.describeDiff(expected, actual)
    assert(diff.contains("line 3"))
    assert(diff.contains("<missing>"))

  test("serialize accepts a frame built by AnsiRenderer.buildFrame"):
    // Sanity check: the real renderer's output round-trips through serialize
    // without exceptions, and produces a consistent shape.
    val root = RootNode(
      width = 6,
      height = 3,
      children = List(TextNode(XCoord(1), YCoord(1), List(Text("hello ", Style())))),
      input = None
    )
    val frame = AnsiRenderer.buildFrame(root)
    val text  = GoldenFrame.serialize(frame)
    assert(text.startsWith("# width=6 height=3\n"))
    assert(text.contains("|hello |"))
