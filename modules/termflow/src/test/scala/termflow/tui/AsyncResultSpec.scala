package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.TuiPrelude.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.*

/**
 * Pins the `AsyncResult` constructors and `Cmd.asyncResult` ergonomics
 * — these are the public bridge between application code and the runtime
 * for "async work that can fail with a domain error", and we want their
 * shape to stay stable.
 */
class AsyncResultSpec extends AnyFunSuite:

  private def await[A](task: AsyncResult[A]): Result[A] = Await.result(task, 1.second)

  // ---- AsyncResult constructors -------------------------------------------

  test("AsyncResult.success wraps a value as a completed Right"):
    assert(await(AsyncResult.success(42)) == Right(42))

  test("AsyncResult.failure wraps a TermFlowError as a completed Left"):
    val err = TermFlowError.Validation("bad")
    assert(await(AsyncResult.failure(err)) == Left(err))

  test("AsyncResult.fromResult preserves Right and Left without scheduling work"):
    assert(await(AsyncResult.fromResult(Right(1))) == Right(1))
    val err = TermFlowError.ConfigError("missing")
    assert(await(AsyncResult.fromResult(Left(err))) == Left(err))

  test("AsyncResult.fromFuture maps a successful Future to Right"):
    assert(await(AsyncResult.fromFuture(Future.successful("ok"))) == Right("ok"))

  test("AsyncResult.fromFuture maps a failed Future to a TermFlowError.Unexpected"):
    val boom = new RuntimeException("boom")
    val r    = await(AsyncResult.fromFuture(Future.failed[Int](boom)))
    r match
      case Left(TermFlowError.Unexpected(msg, Some(cause))) =>
        assert(msg == "boom")
        assert(cause eq boom)
      case other => fail(s"expected Unexpected with cause, got $other")

  // ---- Cmd.asyncResult dispatch -------------------------------------------

  enum TestMsg:
    case Loaded(value: Int)
    case Failed(err: TermFlowError)

  /**
   * Dispatch the `toCmd` of an FCmd against a synthetic value to read the
   *  resulting message — exercises the wiring without spinning the runtime.
   */
  private def runFCmd[A, M](cmd: Cmd[M], value: A): M =
    cmd match
      case Cmd.FCmd(_, toCmd, _) =>
        toCmd.asInstanceOf[A => Cmd[M]](value) match
          case Cmd.GCmd(msg) => msg
          case other         => fail(s"expected GCmd, got $other")
      case other => fail(s"expected FCmd, got $other")

  test("Cmd.asyncResult routes Right values through onSuccess"):
    val cmd = Cmd.asyncResult(
      task = AsyncResult.success(10),
      onSuccess = TestMsg.Loaded.apply,
      onError = TestMsg.Failed.apply
    )
    assert(runFCmd[Result[Int], TestMsg](cmd, Right(10)) == TestMsg.Loaded(10))

  test("Cmd.asyncResult routes Left values through onError"):
    val err = TermFlowError.Validation("bad")
    val cmd = Cmd.asyncResult(
      task = AsyncResult.failure[Int](err),
      onSuccess = TestMsg.Loaded.apply,
      onError = TestMsg.Failed.apply
    )
    assert(runFCmd[Result[Int], TestMsg](cmd, Left(err)) == TestMsg.Failed(err))

  test("Cmd.asyncResult forwards onEnqueue unchanged"):
    val pending = TestMsg.Loaded(0)
    val cmd = Cmd.asyncResult(
      task = AsyncResult.success(1),
      onSuccess = TestMsg.Loaded.apply,
      onError = TestMsg.Failed.apply,
      onEnqueue = Some(pending)
    )
    cmd match
      case Cmd.FCmd(_, _, Some(`pending`)) => ()
      case other                           => fail(s"expected onEnqueue=$pending, got $other")
