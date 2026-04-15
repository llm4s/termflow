package termflow.testkit

import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.AnsiRenderer
import termflow.tui.AnsiRenderer.RenderCell
import termflow.tui.AnsiRenderer.RenderFrame
import termflow.tui.Style

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

class GoldenSupportSpec extends AnyFunSuite with GoldenSupport:

  // Route all goldens into a scratch directory so this test suite never
  // touches committed fixtures.
  private val tmpRoot: Path                      = Files.createTempDirectory("tf-golden-support-")
  override protected def goldenDir: String       = tmpRoot.toString
  override protected def goldenSuiteName: String = "scratch"

  // Allow individual tests to flip update mode on demand without touching
  // system properties (and without leaking across tests).
  @volatile private var forceUpdate: Boolean = false
  override protected def updateMode: Boolean = forceUpdate

  private def frameWithText(text: String): RenderFrame =
    val cells = Array.tabulate(1, text.length)((_, c) => RenderCell(text.charAt(c), Style()))
    RenderFrame(text.length, 1, cells, None)

  test("missing golden in read mode fails with guidance"):
    forceUpdate = false
    val ex = intercept[TestFailedException] {
      assertGoldenFrame(frameWithText("abc"), "missing")
    }
    assert(ex.getMessage.contains("golden file not found"))
    assert(ex.getMessage.contains("-Dtermflow.update-goldens=true"))

  test("update mode writes a new golden and passes"):
    forceUpdate = true
    try
      assertGoldenFrame(frameWithText("hello"), "create")
      val path = goldenPath("create")
      assert(Files.exists(path))
      val body = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      assert(body.contains("|hello|"))
    finally forceUpdate = false

  test("matching golden passes in read mode"):
    // First write, then flip back to read mode and re-assert.
    forceUpdate = true
    assertGoldenFrame(frameWithText("match"), "match")
    forceUpdate = false
    assertGoldenFrame(frameWithText("match"), "match")

  test("mismatching golden in read mode fails with diff detail"):
    forceUpdate = true
    assertGoldenFrame(frameWithText("aaaa"), "mismatch")
    forceUpdate = false
    val ex = intercept[TestFailedException] {
      assertGoldenFrame(frameWithText("aaba"), "mismatch")
    }
    assert(ex.getMessage.contains("golden frame mismatch"))
    assert(ex.getMessage.contains("expected: |aaaa|"))
    assert(ex.getMessage.contains("actual:   |aaba|"))

  test("assertGoldenString supports bespoke single-row snapshots"):
    forceUpdate = true
    assertGoldenString("> hello|", "row")
    forceUpdate = false
    assertGoldenString("> hello|", "row")
    val ex = intercept[TestFailedException] {
      assertGoldenString("> world|", "row")
    }
    assert(ex.getMessage.contains("golden frame mismatch"))

  test("buildFrame output is stable through serialize"):
    // Defensive: guard against AnsiRenderer.buildFrame being made private.
    forceUpdate = true
    val frame = AnsiRenderer.buildFrame(
      termflow.tui.RootNode(
        width = 3,
        height = 1,
        children = List(
          termflow.tui.TextNode(
            termflow.tui.XCoord(1),
            termflow.tui.YCoord(1),
            List(termflow.tui.Text("abc", Style()))
          )
        ),
        input = None
      )
    )
    assertGoldenFrame(frame, "abc")
    forceUpdate = false
    assertGoldenFrame(frame, "abc")
