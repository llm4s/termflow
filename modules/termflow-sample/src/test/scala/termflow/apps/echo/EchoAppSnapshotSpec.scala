package termflow.apps.echo

import org.scalatest.funsuite.AnyFunSuite
import termflow.apps.echo.EchoApp.Msg
import termflow.testkit.GoldenSupport
import termflow.testkit.TuiTestDriver

class EchoAppSnapshotSpec extends AnyFunSuite with GoldenSupport:

  private val Width  = 50
  private val Height = 20

  private def driver(): TuiTestDriver[EchoApp.Model, EchoApp.Msg] =
    val d = TuiTestDriver(EchoApp.App, width = Width, height = Height)
    d.init()
    d

  test("initial empty state"):
    val d = driver()
    assert(d.model.messages.isEmpty)
    assertGoldenFrame(d.frame, "initial")

  test("after adding a single message"):
    val d = driver()
    d.send(Msg.AddMessage("hello"))
    assert(d.model.messages == List("hello"))
    assertGoldenFrame(d.frame, "one-message")

  test("after adding three messages"):
    val d = driver()
    d.send(Msg.AddMessage("first"))
    d.send(Msg.AddMessage("second"))
    d.send(Msg.AddMessage("third"))
    assert(d.model.messages == List("first", "second", "third"))
    assertGoldenFrame(d.frame, "three-messages")

  test("Clear returns to empty state"):
    val d = driver()
    d.send(Msg.AddMessage("one"))
    d.send(Msg.AddMessage("two"))
    d.send(Msg.Clear)
    assert(d.model.messages.isEmpty)
    assertGoldenFrame(d.frame, "cleared")
