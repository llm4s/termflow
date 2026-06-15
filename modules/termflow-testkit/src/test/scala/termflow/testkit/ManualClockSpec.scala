package termflow.testkit

import org.scalatest.funsuite.AnyFunSuite

import java.time.ZoneOffset
import scala.collection.mutable
import scala.concurrent.duration.*

class ManualClockSpec extends AnyFunSuite:

  test("instant reflects the start millis and advances with the clock"):
    val clock = new ManualClock(startMillis = 1000L)
    assert(clock.instant().toEpochMilli == 1000L)
    clock.advance(500L)
    assert(clock.instant().toEpochMilli == 1500L)
    assert(clock.zone == ZoneOffset.UTC)

  test("a periodic task fires once per elapsed period"):
    val clock = new ManualClock()
    var ticks = 0
    clock.schedulePeriodic(100L, () => ticks += 1)
    clock.advance(100L)
    assert(ticks == 1)
    clock.advance(300L)
    assert(ticks == 4)

  test("no fire occurs before a full period elapses"):
    val clock = new ManualClock()
    var ticks = 0
    clock.schedulePeriodic(100L, () => ticks += 1)
    clock.advance(99L)
    assert(ticks == 0)
    clock.advance(1L) // crosses the boundary
    assert(ticks == 1)

  test("tasks observe instant() at their own fire time"):
    val clock = new ManualClock()
    val seen  = mutable.ListBuffer.empty[Long]
    clock.schedulePeriodic(100L, () => seen += clock.currentMillis)
    clock.advance(300L)
    assert(seen.toList == List(100L, 200L, 300L))

  test("multiple tasks fire in chronological order"):
    val clock = new ManualClock()
    val order = mutable.ListBuffer.empty[String]
    clock.schedulePeriodic(100L, () => order += s"a@${clock.currentMillis}")
    clock.schedulePeriodic(150L, () => order += s"b@${clock.currentMillis}")
    clock.advance(300L)
    // At the t=300 tie, tasks fire in scheduling order (a registered before b).
    assert(order.toList == List("a@100", "b@150", "a@200", "a@300", "b@300"))

  test("cancel stops further ticks"):
    val clock  = new ManualClock()
    var ticks  = 0
    val handle = clock.schedulePeriodic(100L, () => ticks += 1)
    clock.advance(100L)
    handle.cancel()
    clock.advance(500L)
    assert(ticks == 1)

  test("advance accepts a FiniteDuration"):
    val clock = new ManualClock()
    var ticks = 0
    clock.schedulePeriodic(50L, () => ticks += 1)
    clock.advance(250.millis)
    assert(ticks == 5)

  test("schedulePeriodic rejects a non-positive period"):
    val clock = new ManualClock()
    assertThrows[IllegalArgumentException](clock.schedulePeriodic(0L, () => ()))

  test("advance rejects a negative duration"):
    val clock = new ManualClock()
    assertThrows[IllegalArgumentException](clock.advance(-1L))
