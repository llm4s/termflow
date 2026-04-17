package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class SubSpec extends AnyFunSuite:

  private class TestSink[Msg] extends EventSink[Msg]:
    private val received = new java.util.concurrent.ConcurrentLinkedQueue[Cmd[Msg]]()
    private val latch    = new CountDownLatch(1)
    override def publish(cmd: Cmd[Msg]): Unit =
      received.add(cmd)
      latch.countDown()

    def awaitFirst(timeoutMs: Long): Boolean =
      latch.await(timeoutMs, TimeUnit.MILLISECONDS)

    def messages: List[Cmd[Msg]] =
      import scala.jdk.CollectionConverters._
      received.asScala.toList

  test("Sub.Every emits messages at the specified interval"):
    val sink    = new TestSink[String]
    val counter = new AtomicInteger(0)
    val sub     = Sub.Every(50, () => s"tick-${counter.incrementAndGet()}", sink)

    assert(sub.isActive)
    // Wait for at least 2 ticks
    Thread.sleep(150)
    sub.cancel()

    assert(!sub.isActive)
    val msgs = sink.messages
    assert(msgs.nonEmpty)
    assert(msgs.exists:
      case Cmd.GCmd(s: String) => s.startsWith("tick-")
      case _                   => false
    )

  test("Sub.Every can be cancelled and stops emitting"):
    val sink    = new TestSink[String]
    val counter = new AtomicInteger(0)
    val sub     = Sub.Every(20, () => s"tick-${counter.incrementAndGet()}", sink)

    Thread.sleep(50)
    sub.cancel()
    val countAtCancel = counter.get()

    Thread.sleep(100)
    // Should not have increased after cancel
    assert(counter.get() == countAtCancel || counter.get() == countAtCancel + 1) // allow 1 in-flight

  test("Sub.NoSub is not active and cancel is a no-op"):
    val noSub = Sub.NoSub
    assert(!noSub.isActive)
    noSub.cancel() // should not throw
    assert(!noSub.isActive)

  test("InputKeyFromSource publishes mapped key and mapped error"):
    final class StubSource(results: List[InputRead]) extends TerminalKeySource:
      private val queue  = new java.util.concurrent.ConcurrentLinkedQueue[InputRead](results.asJava)
      private val closed = new AtomicInteger(0)
      override def next(): InputRead =
        Option(queue.poll()).getOrElse(InputRead.Failed(new InterruptedException("done")))
      override def close(): Try[Unit] = Success(closed.incrementAndGet(): Unit)
      def closeCount: Int             = closed.get()

    val source = new StubSource(
      List(InputRead.Key(KeyDecoder.InputKey.CharKey('a')), InputRead.Failed(new RuntimeException("boom")))
    )
    val sink = new TestSink[String]
    val sub = Sub.InputKeyFromSource[String](
      source,
      key => s"key:$key",
      err => s"err:${err.getClass.getSimpleName}",
      sink
    )

    assert(sink.awaitFirst(500))
    Thread.sleep(50)
    sub.cancel()
    assert(source.closeCount == 1)
    val msgs = sink.messages.collect { case Cmd.GCmd(v: String) => v }
    assert(msgs.exists(_.startsWith("key:")))
    assert(msgs.exists(_.startsWith("err:")))

  test("RandomSourceAtFixedRate emits and can be cancelled"):
    val sink    = new TestSink[Int]
    val counter = new AtomicInteger(0)
    val source  = new RandomUtil.RandomSourceAtFixedRate[Int, Int](20, () => counter.incrementAndGet(), identity)
    val sub     = source.asEventSource(sink)

    assert(sink.awaitFirst(500))
    val beforeCancel = sink.messages.size
    sub.cancel()
    Thread.sleep(80)
    assert(!sub.isActive)
    assert(sink.messages.size == beforeCancel)

  test("InputKeyFromSource cancel does not emit interruption error"):
    final class BlockingSource(started: CountDownLatch) extends TerminalKeySource:
      override def next(): InputRead =
        try
          started.countDown()
          Thread.sleep(10_000)
          InputRead.Key(KeyDecoder.InputKey.CharKey('x'))
        catch case e: InterruptedException => InputRead.Failed(e)
      override def close(): Try[Unit] = Success(())

    val started = new CountDownLatch(1)
    val source  = new BlockingSource(started)
    val sink    = new TestSink[String]
    val sub = Sub.InputKeyFromSource[String](
      source,
      key => s"key:$key",
      err => s"err:${err.getClass.getSimpleName}",
      sink
    )

    assert(started.await(1, TimeUnit.SECONDS))
    sub.cancel()
    Thread.sleep(50)
    val msgs = sink.messages.collect { case Cmd.GCmd(v: String) => v }
    assert(msgs.isEmpty)

  test("InputKeyFromSource cancel remains safe if source.close throws"):
    final class ThrowingCloseSource(started: CountDownLatch) extends TerminalKeySource:
      override def next(): InputRead =
        try
          started.countDown()
          Thread.sleep(10_000)
          InputRead.Key(KeyDecoder.InputKey.CharKey('x'))
        catch case e: InterruptedException => InputRead.Failed(e)
      override def close(): Try[Unit] = Failure(new RuntimeException("close-failed"))

    val started = new CountDownLatch(1)
    val source  = new ThrowingCloseSource(started)
    val sink    = new TestSink[String]
    val sub = Sub.InputKeyFromSource[String](
      source,
      key => s"key:$key",
      err => s"err:${err.getClass.getSimpleName}",
      sink
    )

    assert(started.await(1, TimeUnit.SECONDS))
    try sub.cancel()
    catch {
      case e: Throwable => fail(s"cancel should not throw, but got: ${e.getMessage}")
    }
    assert(!sub.isActive)

  test("InputKeyFromSource stops quietly on End"):
    final class EndSource extends TerminalKeySource:
      private val queue = new java.util.concurrent.ConcurrentLinkedQueue[InputRead](
        List(InputRead.Key(KeyDecoder.InputKey.CharKey('a')), InputRead.End).asJava
      )
      override def next(): InputRead  = Option(queue.poll()).getOrElse(InputRead.End)
      override def close(): Try[Unit] = Success(())

    val sink = new TestSink[String]
    val sub = Sub.InputKeyFromSource[String](
      new EndSource,
      key => s"key:$key",
      err => s"err:${err.getClass.getSimpleName}",
      sink
    )

    assert(sink.awaitFirst(500))
    Thread.sleep(50)
    assert(!sub.isActive)
    val msgs = sink.messages.collect { case Cmd.GCmd(v: String) => v }
    assert(msgs.count(_.startsWith("key:")) == 1)
    assert(!msgs.exists(_.startsWith("err:")))

  // --- Sub.start() / deferred-start contract --------------------------------

  /** Stub RuntimeCtx that records subs but never calls `start()`. */
  private class CapturingCtx[Msg] extends RuntimeCtx[Msg]:
    private val captured                              = new java.util.concurrent.ConcurrentLinkedQueue[Cmd[Msg]]()
    private val subs                                  = new java.util.concurrent.ConcurrentLinkedQueue[Sub[Msg]]()
    override def terminal: TerminalBackend            = throw new UnsupportedOperationException("not used in test")
    override def config: TermFlowConfig               = throw new UnsupportedOperationException("not used in test")
    override def publish(cmd: Cmd[Msg]): Unit         = captured.add(cmd): Unit
    override def registerSub(sub: Sub[Msg]): Sub[Msg] = { subs.add(sub): Unit; sub }
    def messages: List[Cmd[Msg]]                      = captured.iterator().asScala.toList
    def registered: List[Sub[Msg]]                    = subs.iterator().asScala.toList

  test("Sub.Every does not tick when registered into a RuntimeCtx that omits start()"):
    val ctx = new CapturingCtx[String]
    val sub = Sub.Every(20, () => "tick", ctx)
    assert(ctx.registered == List(sub))
    Thread.sleep(80) // enough wall-clock for several missed intervals
    assert(ctx.messages.isEmpty, s"expected no ticks but saw ${ctx.messages}")
    sub.cancel()

  test("Sub.Every starts ticking once start() is invoked explicitly"):
    val ctx = new CapturingCtx[String]
    val sub = Sub.Every(20, () => "tick", ctx)
    assert(ctx.messages.isEmpty)
    sub.start()
    try
      // Wait for at least one tick.
      val deadline = System.currentTimeMillis() + 500
      while ctx.messages.isEmpty && System.currentTimeMillis() < deadline do Thread.sleep(10)
      assert(ctx.messages.nonEmpty, "expected a tick after start()")
      assert(ctx.messages.head == Cmd.GCmd("tick"))
    finally sub.cancel()

  test("Sub.Every.start() is idempotent — multiple calls do not spawn extra schedulers"):
    val ctx = new CapturingCtx[String]
    val sub = Sub.Every(50, () => "tick", ctx)
    sub.start()
    sub.start()
    sub.start()
    Thread.sleep(120)
    sub.cancel()
    // We only assert determinism via "did not blow up"; concretely, double-start
    // would have leaked extra scheduler threads. The cancel above should still
    // tear everything down cleanly; this test is primarily about not throwing.
    assert(!sub.isActive)

  test("Sub.Every with a bare EventSink (non-RuntimeCtx) still starts eagerly"):
    // Back-compat: when caller passes a plain EventSink, the factory must
    // start the sub itself so existing code keeps working.
    val sink = new TestSink[String]
    val sub  = Sub.Every(20, () => "tick", sink)
    try
      assert(sink.awaitFirst(500))
      assert(sink.messages.nonEmpty)
    finally sub.cancel()

  test("Sub trait default start() is a no-op"):
    val sub = new Sub[Nothing]:
      override def isActive: Boolean = true
      override def cancel(): Unit    = ()
      // Inherit default start, which is a no-op.
    // Should not throw and should leave isActive untouched.
    sub.start()
    assert(sub.isActive)
    sub.start()
    assert(sub.isActive)

  // --- Sub.InputKey deferred-start contract (issue #110) -------------------

  /**
   * Stub `TerminalKeySource` that hands out a scripted sequence of
   * `InputRead`s and records how many times it's been polled. Blocks on
   * `latch` after exhausting its script so the consumer thread stays
   * parked (simulating a live terminal that has no more keys yet).
   */
  private class ScriptedSource(script: List[InputRead]) extends TerminalKeySource:
    private val queue = new java.util.concurrent.ConcurrentLinkedQueue[InputRead](script.asJava)
    val polls         = new AtomicInteger(0)
    private val done  = new CountDownLatch(1)
    override def next(): InputRead =
      polls.incrementAndGet(): Unit
      Option(queue.poll()) match
        case Some(r) => r
        case None =>
          try done.await(5, TimeUnit.SECONDS): Unit
          catch { case _: InterruptedException => () }
          InputRead.End
    override def close(): Try[Unit] =
      done.countDown()
      Success(())

  test("InputKeyFromSource does not read when registered into a RuntimeCtx that omits start()"):
    val ctx = new CapturingCtx[String]
    val source = new ScriptedSource(
      List(InputRead.Key(KeyDecoder.InputKey.CharKey('a')))
    )
    val sub = Sub.InputKeyFromSource[String](source, k => s"key:$k", e => s"err:$e", ctx)
    assert(ctx.registered == List(sub))
    // Wait well past any plausible race window — if the thread were running
    // it would have called `next` at least once.
    Thread.sleep(80)
    assert(source.polls.get() == 0, s"expected zero polls; saw ${source.polls.get()}")
    assert(ctx.messages.isEmpty, s"expected no published messages; saw ${ctx.messages}")
    sub.cancel()

  test("InputKeyFromSource starts consuming once start() is called explicitly"):
    val ctx = new CapturingCtx[String]
    val source = new ScriptedSource(
      List(InputRead.Key(KeyDecoder.InputKey.CharKey('z')))
    )
    val sub = Sub.InputKeyFromSource[String](source, k => s"key:$k", e => s"err:$e", ctx)
    assert(ctx.messages.isEmpty)
    sub.start()
    try
      // Wait for the 'z' to be published.
      val deadline = System.currentTimeMillis() + 1000
      while ctx.messages.isEmpty && System.currentTimeMillis() < deadline do Thread.sleep(10)
      val msgs = ctx.messages.collect { case Cmd.GCmd(v: String) => v }
      assert(msgs.exists(_.startsWith("key:")), s"expected key message; saw ${ctx.messages}")
    finally sub.cancel()

  test("InputKeyFromSource.start() is idempotent — multiple calls do not spawn extra threads"):
    val ctx    = new CapturingCtx[String]
    val source = new ScriptedSource(List(InputRead.Key(KeyDecoder.InputKey.CharKey('x'))))
    val sub    = Sub.InputKeyFromSource[String](source, k => s"key:$k", e => s"err:$e", ctx)
    sub.start()
    sub.start()
    sub.start()
    // Wait for the single 'x' to arrive.
    val deadline = System.currentTimeMillis() + 1000
    while ctx.messages.isEmpty && System.currentTimeMillis() < deadline do Thread.sleep(10)
    sub.cancel()
    assert(!sub.isActive)

  test("InputKeyFromSource cancel before start does not leave a dangling thread"):
    val ctx    = new CapturingCtx[String]
    val source = new ScriptedSource(List(InputRead.Key(KeyDecoder.InputKey.CharKey('a'))))
    val sub    = Sub.InputKeyFromSource[String](source, k => s"key:$k", e => s"err:$e", ctx)
    sub.cancel()
    // A subsequent start() must NOT spin up a thread for a cancelled sub.
    sub.start()
    Thread.sleep(50)
    assert(source.polls.get() == 0, "cancelled sub must not poll after start()")
    assert(!sub.isActive)

  test("InputKeyFromSource with a bare EventSink (non-RuntimeCtx) still starts eagerly"):
    // Back-compat: when caller passes a plain EventSink, the factory must
    // start the sub itself so existing code keeps working.
    val sink = new TestSink[String]
    val source = new ScriptedSource(
      List(InputRead.Key(KeyDecoder.InputKey.CharKey('a')))
    )
    val sub = Sub.InputKeyFromSource[String](source, k => s"key:$k", e => s"err:$e", sink)
    try
      assert(sink.awaitFirst(500))
      val msgs = sink.messages.collect { case Cmd.GCmd(v: String) => v }
      assert(msgs.exists(_.startsWith("key:")))
    finally sub.cancel()
