package termflow.apps.tabs

import org.scalatest.funsuite.AnyFunSuite
import termflow.apps.tabs.TabsDemoApp.Msg
import termflow.testkit.GoldenSupport
import termflow.testkit.TuiTestDriver

/**
 * Snapshot coverage for the `TabsDemoApp` sample.
 *
 * Exercises tab switching, per-tab counter state, and note persistence so
 * future layout or rendering changes have to account for how the tab bar,
 * header, notes, and status line co-exist inside the box.
 */
class TabsDemoAppSnapshotSpec extends AnyFunSuite with GoldenSupport:

  private val Width  = 60
  private val Height = 20

  private def driver(): TuiTestDriver[TabsDemoApp.Model, TabsDemoApp.Msg] =
    val d = TuiTestDriver(TabsDemoApp.App, width = Width, height = Height)
    d.init()
    d

  test("initial tab (Home)"):
    val d = driver()
    assert(d.model.activeTab == 0)
    assertGoldenFrame(d.frame, "initial-home")

  test("after NextTab shows Work"):
    val d = driver()
    d.send(Msg.NextTab)
    assert(d.model.activeTab == 1)
    assert(d.model.tabs(1).name == "Work")
    assertGoldenFrame(d.frame, "work-tab")

  test("SelectTab(2) + Increment persists per-tab counter"):
    val d = driver()
    d.send(Msg.SelectTab(2))
    d.send(Msg.Increment)
    d.send(Msg.Increment)
    assert(d.model.tabs(2).counter == 2)
    // Other tabs untouched.
    assert(d.model.tabs(0).counter == 0)
    assert(d.model.tabs(1).counter == 0)
    assertGoldenFrame(d.frame, "notes-tab-counter-2")

  test("AddNote appears in the notes panel"):
    val d = driver()
    d.send(Msg.AddNote("hello world"))
    assert(d.model.tabs(0).notes == List("hello world"))
    assertGoldenFrame(d.frame, "home-with-note")
