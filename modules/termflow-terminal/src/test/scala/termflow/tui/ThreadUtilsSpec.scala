package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

class ThreadUtilsSpec extends AnyFunSuite:

  test("newThreadFactory returns a usable ThreadFactory"):
    val factory = ThreadUtils.newThreadFactory()
    val latch   = new CountDownLatch(1)
    val t       = factory.newThread(() => latch.countDown())
    t.start()
    assert(latch.await(2, TimeUnit.SECONDS), "factory thread should run")
    t.join(2_000L)

  test("startThread runs the runnable to completion"):
    val latch  = new CountDownLatch(1)
    val thread = ThreadUtils.startThread(() => latch.countDown())
    assert(latch.await(2, TimeUnit.SECONDS), "started thread should run")
    thread.join(2_000L)
    assert(!thread.isAlive)
