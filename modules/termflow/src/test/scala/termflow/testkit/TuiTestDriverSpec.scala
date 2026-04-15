package termflow.testkit

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

import scala.concurrent.Future
import scala.concurrent.Promise

class TuiTestDriverSpec extends AnyFunSuite:

  // A minimal app whose message ADT exercises every Cmd variant the driver
  // has to handle. `Model` is a plain Int to keep assertions obvious.
  private object TinyApp extends TuiApp[Int, TinyApp.Msg]:
    enum Msg:
      case Inc
      case Plus(delta: Int)
      case Chain(a: Msg, b: Msg)
      case Async(value: Int)
      case AsyncUnresolved
      case ReportError(text: String)
      case Quit
      case Noop

    override def init(ctx: RuntimeCtx[Msg]): Tui[Int, Msg] = Tui(0)

    override def update(model: Int, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Int, Msg] =
      msg match
        case Msg.Inc         => Tui(model + 1)
        case Msg.Plus(d)     => Tui(model + d)
        case Msg.Chain(a, _) => Tui(model, Cmd.GCmd(a))
        case Msg.Async(v) =>
          Tui(
            model,
            Cmd.FCmd[Int, Msg](
              task = Future.successful(v),
              toCmd = r => Cmd.GCmd(Msg.Plus(r)),
              onEnqueue = None
            )
          )
        case Msg.AsyncUnresolved =>
          val p = Promise[Int]()
          Tui(
            model,
            Cmd.FCmd[Int, Msg](
              task = p.future,
              toCmd = r => Cmd.GCmd(Msg.Plus(r)),
              onEnqueue = None
            )
          )
        case Msg.ReportError(text) =>
          Tui(model, Cmd.TermFlowErrorCmd(TermFlowError.Validation(text)))
        case Msg.Quit => Tui(model, Cmd.Exit)
        case Msg.Noop => Tui(model, Cmd.NoCmd)

    override def view(model: Int): RootNode =
      RootNode(
        width = 10,
        height = 1,
        children = List(
          TextNode(XCoord(1), YCoord(1), List(Text(s"n=$model", Style())))
        ),
        input = None
      )

    override def toMsg(input: PromptLine): Result[Msg] = Right(Msg.Noop)

  private def driver(): TuiTestDriver[Int, TinyApp.Msg] =
    val d = TuiTestDriver(TinyApp, width = 10, height = 1)
    d.init()
    d

  test("init captures the initial model and renders a frame"):
    val d = driver()
    assert(d.model == 0)
    assert(!d.exited)
    val text = GoldenFrame.serialize(d.frame)
    assert(text.contains("|n=0       |"))

  test("model accessor throws before init"):
    val d  = TuiTestDriver(TinyApp, width = 10, height = 1)
    val ex = intercept[IllegalStateException](d.model)
    assert(ex.getMessage.contains("before init()"))

  test("init called twice throws"):
    val d  = driver()
    val ex = intercept[IllegalStateException](d.init())
    assert(ex.getMessage.contains("called twice"))

  test("send applies a pure update and updates the frame"):
    val d = driver()
    d.send(TinyApp.Msg.Inc)
    assert(d.model == 1)
    val text = GoldenFrame.serialize(d.frame)
    assert(text.contains("|n=1       |"))

  test("Cmd.Exit sets the exited flag and blocks further sends"):
    val d = driver()
    d.send(TinyApp.Msg.Quit)
    assert(d.exited)
    val ex = intercept[IllegalStateException](d.send(TinyApp.Msg.Inc))
    assert(ex.getMessage.contains("after Cmd.Exit"))

  test("Cmd.GCmd is dispatched as a follow-up message"):
    val d = driver()
    d.send(TinyApp.Msg.Chain(TinyApp.Msg.Plus(3), TinyApp.Msg.Noop))
    assert(d.model == 3)

  test("Cmd.FCmd with Future.successful resolves eagerly"):
    val d = driver()
    d.send(TinyApp.Msg.Async(5))
    assert(d.model == 5)

  test("Cmd.FCmd with an unresolved Future throws with guidance"):
    val d  = driver()
    val ex = intercept[IllegalStateException](d.send(TinyApp.Msg.AsyncUnresolved))
    assert(ex.getMessage.contains("non-completed Future"))
    assert(ex.getMessage.contains("Future.successful"))

  test("Cmd.TermFlowErrorCmd is captured without exiting"):
    val d = driver()
    d.send(TinyApp.Msg.ReportError("nope"))
    assert(!d.exited)
    assert(d.observedErrors == List(TermFlowError.Validation("nope")))

  test("Cmd.NoCmd is a no-op"):
    val d = driver()
    d.send(TinyApp.Msg.Noop)
    assert(d.model == 0)

  test("cmds records every applied Cmd in order"):
    val d = driver()
    d.send(TinyApp.Msg.Inc)  // produces Cmd.NoCmd (default Tui cmd)
    d.send(TinyApp.Msg.Quit) // produces Cmd.Exit
    // The init call produced a Cmd.NoCmd as well, so the sequence is
    // NoCmd (init), NoCmd (Inc), Exit (Quit).
    assert(d.cmds == List(Cmd.NoCmd, Cmd.NoCmd, Cmd.Exit))
