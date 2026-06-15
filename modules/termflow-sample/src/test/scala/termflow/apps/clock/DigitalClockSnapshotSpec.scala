package termflow.apps.clock

import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.GoldenSupport
import termflow.testkit.TuiTestDriver

import scala.concurrent.duration.*

/**
 * Time-advance coverage for the `Sub.Every`-driven `DigitalClock`.
 *
 * The app reads wall-clock time through `RuntimeCtx.clock`, which the testkit
 * backs with a `ManualClock` (UTC, starting at epoch 0). Driving
 * `advanceTime` fires the 1-second timer deterministically — no real sleeps —
 * so the rendered clock value is reproducible in goldens.
 */
class DigitalClockSnapshotSpec extends AnyFunSuite with GoldenSupport:

  private val Width  = 44
  private val Height = 20

  private def driver(): TuiTestDriver[DigitalClock.Model, DigitalClock.Msg] =
    val d = TuiTestDriver(DigitalClock.App, width = Width, height = Height)
    d.init()
    d

  test("initial frame shows the clock at the manual clock's start instant"):
    val d = driver()
    assert(d.model.clock.value == "00:00")
    assertGoldenFrame(d.frame, "initial")

  test("advancing one second produces exactly one tick"):
    val d = driver()
    d.advanceTime(1.second)
    assert(d.model.clock.value == "00:00:01")
    assertGoldenFrame(d.frame, "after-1s")

  test("advancing several seconds advances the displayed time accordingly"):
    val d = driver()
    d.advanceTime(1.second)
    d.advanceTime(3.seconds)
    assert(d.model.clock.value == "00:00:04")

  test("a fractional advance below the tick interval does not tick"):
    val d = driver()
    d.advanceTime(500.millis)
    assert(d.model.clock.value == "00:00") // still the initial value
    d.advanceTime(500.millis)
    assert(d.model.clock.value == "00:00:01") // boundary crossed once across the two advances

  test("StopClock halts ticking so further advances leave the time frozen"):
    val d = driver()
    d.advanceTime(2.seconds)
    assert(d.model.clock.value == "00:00:02")
    d.send(DigitalClock.Msg.StopClock)
    d.advanceTime(5.seconds)
    assert(d.model.clock.value == "00:00:02") // frozen after the clock was stopped
