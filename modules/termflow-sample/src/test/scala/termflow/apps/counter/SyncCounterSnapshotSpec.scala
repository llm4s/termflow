package termflow.apps.counter

import org.scalatest.funsuite.AnyFunSuite
import termflow.apps.counter.SyncCounter.Msg
import termflow.testkit.GoldenSupport
import termflow.testkit.TuiTestDriver
import termflow.tui.KeyDecoder.InputKey

/**
 * Snapshot coverage for the `SyncCounter` sample app.
 *
 * Uses `TuiTestDriver` to drive the app synchronously at a fixed terminal
 * size and compares each rendered frame against a checked-in golden file.
 * The intent is to catch repaint / layout regressions in future render
 * pipeline changes, not to pin down every cosmetic detail.
 *
 * To refresh after an intentional change:
 * {{{
 *   sbt -Dtermflow.update-goldens=true "termflowSample/testOnly *SyncCounter*"
 * }}}
 * and review the diff with `git diff`.
 */
class SyncCounterSnapshotSpec extends AnyFunSuite with GoldenSupport:

  private val Width  = 40
  private val Height = 13

  private def driver(): TuiTestDriver[SyncCounter.Model, SyncCounter.Msg] =
    val d = TuiTestDriver(SyncCounter.App, width = Width, height = Height)
    d.init()
    d

  test("initial state"):
    val d = driver()
    assert(d.model.counter.count == 0)
    assertGoldenFrame(d.frame, "initial")

  test("after one increment"):
    val d = driver()
    d.send(Msg.Increment)
    assert(d.model.counter.count == 1)
    assertGoldenFrame(d.frame, "incremented")

  test("after two decrements"):
    val d = driver()
    d.send(Msg.Decrement)
    d.send(Msg.Decrement)
    assert(d.model.counter.count == -2)
    assertGoldenFrame(d.frame, "negative")

  test("typing 'exit' through the prompt dispatches Cmd.Exit"):
    val d = driver()
    "exit".foreach(ch => d.send(Msg.ConsoleInputKey(InputKey.CharKey(ch))))
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.exited, s"driver should have observed Cmd.Exit, saw cmds=${d.cmds}")

  test("typing '+' then Enter increments the counter via toMsg"):
    val d = driver()
    d.send(Msg.ConsoleInputKey(InputKey.CharKey('+')))
    d.send(Msg.ConsoleInputKey(InputKey.Enter))
    assert(d.model.counter.count == 1)
