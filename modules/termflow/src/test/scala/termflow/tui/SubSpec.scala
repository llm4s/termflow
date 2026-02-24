package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

class SubSpec extends AnyFunSuite {

  private class TestSink[Msg] extends EventSink[Msg] {
    private val received = new java.util.concurrent.ConcurrentLinkedQueue[Cmd[Msg]]()
    private val latch    = new CountDownLatch(1)
    override def publish(cmd: Cmd[Msg]): Unit = {
      received.add(cmd)
      latch.countDown()
    }
    def awaitFirst(timeoutMs: Long): Boolean = latch.await(timeoutMs, TimeUnit.MILLISECONDS)
    def messages: List[Cmd[Msg]] = {
      import scala.jdk.CollectionConverters._
      received.asScala.toList
    }
  }

  test("Sub.Every emits messages at the specified interval") {
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
    assert(msgs.exists {
      case Cmd.GCmd(s: String) => s.startsWith("tick-")
      case _                   => false
    })
  }

  test("Sub.Every can be cancelled and stops emitting") {
    val sink    = new TestSink[String]
    val counter = new AtomicInteger(0)
    val sub     = Sub.Every(20, () => s"tick-${counter.incrementAndGet()}", sink)

    Thread.sleep(50)
    sub.cancel()
    val countAtCancel = counter.get()

    Thread.sleep(100)
    // Should not have increased after cancel
    assert(counter.get() == countAtCancel || counter.get() == countAtCancel + 1) // allow 1 in-flight
  }

  test("Sub.NoSub is not active and cancel is a no-op") {
    val noSub = Sub.NoSub
    assert(!noSub.isActive)
    noSub.cancel() // should not throw
    assert(!noSub.isActive)
  }
}
