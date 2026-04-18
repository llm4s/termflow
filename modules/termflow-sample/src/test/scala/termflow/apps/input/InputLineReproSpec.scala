package termflow.apps.input

import org.scalatest.funsuite.AnyFunSuite
import termflow.apps.input.InputLineReproApp.Msg
import termflow.testkit.GoldenSupport
import termflow.testkit.TuiTestDriver
import termflow.tui.AnsiRenderer.RenderFrame
import termflow.tui.KeyDecoder.InputKey

/**
 * Focused coverage for the prompt-row repaint regressions (#73, #74).
 *
 * Instead of snapshotting the whole frame, each test asserts on individual
 * rows that the renderer has historically mis-painted: the prompt row, the
 * buffer row, and the cursor row. Narrow assertions mean unrelated cosmetic
 * changes elsewhere in the app won't knock these tests over.
 */
class InputLineReproSpec extends AnyFunSuite with GoldenSupport:

  private val Width  = 60
  private val Height = 14

  private def driver(): TuiTestDriver[InputLineReproApp.Model, InputLineReproApp.Msg] =
    val d = TuiTestDriver(InputLineReproApp.App, width = Width, height = Height)
    d.init()
    d

  /** Extract the serialized form of a single 1-indexed row from a frame. */
  private def row(frame: RenderFrame, rowIndex: Int): String =
    val cells = frame.cells(rowIndex - 1)
    val sb    = new StringBuilder("|")
    var c     = 0
    while c < frame.width do
      sb.append(cells(c).ch)
      c += 1
    sb.append("|").toString

  private def typeChars(d: TuiTestDriver[InputLineReproApp.Model, InputLineReproApp.Msg], s: String): Unit =
    s.foreach(ch => d.send(Msg.ConsoleInputKey(InputKey.CharKey(ch))))

  test("initial buffer row is empty"):
    val d = driver()
    // Row 6 is "Buffer : ''" per InputLineReproApp.view line 68
    assertGoldenString(row(d.frame, 6), "buffer-row-empty")

  test("after typing 'abc' buffer and cursor rows reflect the state"):
    val d = driver()
    typeChars(d, "abc")
    assertGoldenString(row(d.frame, 6), "buffer-row-abc")
    // Row 7 is "Cursor : <pos> / <len>"
    assertGoldenString(row(d.frame, 7), "cursor-row-after-abc")

  test("after typing 'abcdefghi' then Left x3 cursor row shows cursor 6"):
    val d = driver()
    // The sample's docstring is literally: type abcdefghi then press Left three times.
    typeChars(d, "abcdefghi")
    d.send(Msg.ConsoleInputKey(InputKey.ArrowLeft))
    d.send(Msg.ConsoleInputKey(InputKey.ArrowLeft))
    d.send(Msg.ConsoleInputKey(InputKey.ArrowLeft))
    assertGoldenString(row(d.frame, 6), "buffer-row-full")
    assertGoldenString(row(d.frame, 7), "cursor-row-mid")

  test("pressing Enter clears the buffer and records submission"):
    val d = driver()
    typeChars(d, "hello")
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.lastSubmitted == "hello")
    // Row 8 is "Submit : '<value>'"
    assertGoldenString(row(d.frame, 8), "submit-row-hello")
