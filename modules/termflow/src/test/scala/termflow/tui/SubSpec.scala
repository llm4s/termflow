package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters._
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
    final class StubSource(results: List[Try[KeyDecoder.InputKey]]) extends TerminalKeySource:
      private val queue  = new java.util.concurrent.ConcurrentLinkedQueue[Try[KeyDecoder.InputKey]](results.asJava)
      private val closed = new AtomicInteger(0)
      override def next(): Try[KeyDecoder.InputKey] =
        Option(queue.poll()).getOrElse(Failure(new InterruptedException("done")))
      override def close(): Unit = closed.incrementAndGet(): Unit
      def closeCount: Int        = closed.get()

    val source = new StubSource(List(Success(KeyDecoder.InputKey.CharKey('a')), Failure(new RuntimeException("boom"))))
    val sink   = new TestSink[String]
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
      override def next(): Try[KeyDecoder.InputKey] =
        Try {
          started.countDown()
          Thread.sleep(10_000)
          KeyDecoder.InputKey.CharKey('x')
        }
      override def close(): Unit = ()

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
