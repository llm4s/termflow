package termflow.apps.stress

import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.GoldenSupport
import termflow.testkit.TuiTestDriver

import scala.concurrent.duration.*

/**
 * Time-advance coverage for the `Sub.Every`-driven `SineWaveApp` animation.
 *
 * Each 50ms tick advances the wave's phase by `step`. The testkit's
 * `ManualClock` lets `advanceTime` fire those ticks deterministically, so the
 * animation can be snapshotted at a known phase without real-time delays.
 */
class SineWaveAppSnapshotSpec extends AnyFunSuite with GoldenSupport:

  private val Width  = 60
  private val Height = 20

  private def driver(): TuiTestDriver[SineWaveApp.Model, SineWaveApp.Msg] =
    val d = TuiTestDriver(SineWaveApp.App, width = Width, height = Height)
    d.init()
    d

  test("initial frame renders the wave at phase 0"):
    val d = driver()
    assert(d.model.phase == 0.0)
    assertGoldenFrame(d.frame, "initial")

  test("one tick interval advances the phase by one step"):
    val d = driver()
    d.advanceTime(50.millis)
    assert(d.model.phase == d.model.step)

  test("advancing five intervals applies five steps and animates the wave"):
    val d = driver()
    d.advanceTime(250.millis)
    assert(math.abs(d.model.phase - 5 * 0.22) < 1e-9)
    assertGoldenFrame(d.frame, "after-5-ticks")

  test("Pause stops the animation so further advances leave the phase fixed"):
    val d = driver()
    d.advanceTime(100.millis)
    val pausedPhase = d.model.phase
    d.send(SineWaveApp.Msg.Pause)
    d.advanceTime(1.second)
    assert(d.model.phase == pausedPhase)
    assert(!d.model.running)
