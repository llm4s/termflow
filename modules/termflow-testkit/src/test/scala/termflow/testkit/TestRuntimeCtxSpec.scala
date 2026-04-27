package termflow.testkit

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.Cmd
import termflow.tui.Sub

class TestRuntimeCtxSpec extends AnyFunSuite:

  test("publish queues commands in FIFO order and drainCmds clears the buffer"):
    val ctx = TestRuntimeCtx[Int](width = 40, height = 10)
    ctx.publish(Cmd.GCmd(1))
    ctx.publish(Cmd.NoCmd)
    ctx.publish(Cmd.GCmd(2))
    val drained = ctx.drainCmds()
    assert(drained == List(Cmd.GCmd(1), Cmd.NoCmd, Cmd.GCmd(2)))
    assert(ctx.drainCmds().isEmpty)

  test("registerSub records the subscription without driving it"):
    val ctx                 = TestRuntimeCtx[Int](width = 40, height = 10)
    @volatile var cancelled = false
    val sub = new Sub[Int]:
      override def isActive: Boolean = true
      override def cancel(): Unit    = cancelled = true
    val returned = ctx.registerSub(sub)
    assert(returned eq sub)
    assert(ctx.registeredSubs == List(sub))
    assert(!cancelled)

  test("cancelSubs cancels and clears all registered subs"):
    val ctx = TestRuntimeCtx[Int](width = 40, height = 10)
    val c1  = new java.util.concurrent.atomic.AtomicBoolean(false)
    val c2  = new java.util.concurrent.atomic.AtomicBoolean(false)
    ctx.registerSub(new Sub[Int]:
      override def isActive: Boolean = true
      override def cancel(): Unit    = c1.set(true)
    )
    ctx.registerSub(new Sub[Int]:
      override def isActive: Boolean = true
      override def cancel(): Unit    = c2.set(true)
    )
    ctx.cancelSubs()
    assert(c1.get() && c2.get())
    assert(ctx.registeredSubs.isEmpty)

  test("cancelSubs swallows exceptions from individual subs"):
    val ctx     = TestRuntimeCtx[Int](width = 40, height = 10)
    val reached = new java.util.concurrent.atomic.AtomicBoolean(false)
    ctx.registerSub(new Sub[Int]:
      override def isActive: Boolean = true
      override def cancel(): Unit    = throw new RuntimeException("boom")
    )
    ctx.registerSub(new Sub[Int]:
      override def isActive: Boolean = true
      override def cancel(): Unit    = reached.set(true)
    )
    ctx.cancelSubs() // should not throw
    assert(reached.get())

  test("fake terminal backend reports the configured dimensions"):
    val ctx = TestRuntimeCtx[Int](width = 120, height = 42)
    assert(ctx.terminal.width == 120)
    assert(ctx.terminal.height == 42)
