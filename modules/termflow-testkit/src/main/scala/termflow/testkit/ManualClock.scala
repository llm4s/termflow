package termflow.testkit

import termflow.tui.Clock

import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

/**
 * Deterministic [[Clock]] for snapshot tests: virtual time only moves forward
 * when [[advance]] is called, and scheduled tasks fire synchronously on the
 * advancing thread. No real timers, no sleeps.
 *
 * `Sub.Every` subscriptions registered against a `TestRuntimeCtx` (whose
 * [[termflow.tui.RuntimeCtx.clock]] is a `ManualClock`) schedule their ticks
 * here. [[TuiTestDriver.advanceTime]] then advances this clock to fire those
 * ticks, so an app driven by a timer can be observed frame-by-frame.
 *
 * Tick semantics: a task scheduled with period `p` first fires `p`
 * milliseconds after it was scheduled, then every `p` thereafter — so
 * `advance(N * p)` produces exactly `N` ticks. Unlike the real
 * [[termflow.tui.SystemClock]], there is no immediate fire at scheduling
 * time; this gives tests an exact advance→tick correspondence.
 *
 * @param startMillis Initial epoch-millis value reported by [[instant]].
 * @param zone        Zone used when deriving local date/time from [[instant]];
 *                    defaults to UTC so goldens are machine-independent.
 */
final class ManualClock(startMillis: Long = 0L, override val zone: ZoneId = ZoneOffset.UTC) extends Clock:

  final private class Task(val periodMillis: Long, val run: () => Unit):
    var nextFireAt: Long = 0L
    var active: Boolean  = true

  private val lock      = new Object
  private var nowMillis = startMillis
  private val tasks     = mutable.ArrayBuffer.empty[Task]

  /** Current virtual time in epoch milliseconds. */
  def currentMillis: Long = lock.synchronized(nowMillis)

  override def instant(): Instant = Instant.ofEpochMilli(currentMillis)

  override def schedulePeriodic(periodMillis: Long, task: () => Unit): Clock.Cancelable =
    require(periodMillis > 0, s"periodMillis must be > 0, was $periodMillis")
    val t = new Task(periodMillis, task)
    lock.synchronized {
      t.nextFireAt = nowMillis + periodMillis
      val _ = tasks.append(t)
    }
    new Clock.Cancelable:
      override def cancel(): Unit =
        lock.synchronized {
          t.active = false
          val _ = tasks.subtractOne(t)
        }

  /** Advance virtual time by `duration`. See [[advance(deltaMillis:Long)*]]. */
  def advance(duration: FiniteDuration): Unit = advance(duration.toMillis)

  /**
   * Advance virtual time by `deltaMillis`, firing every scheduled task once
   * for each whole period that elapses, in chronological order across all
   * tasks. Each task's thunk runs while [[instant]] reports that task's own
   * fire time; after the advance completes the clock reads `start + delta`.
   */
  def advance(deltaMillis: Long): Unit =
    require(deltaMillis >= 0, s"deltaMillis must be >= 0, was $deltaMillis")
    val target = lock.synchronized(nowMillis + deltaMillis)
    var more   = true
    while more do
      // Pick the earliest task due at or before `target`. Tasks may publish to
      // the bus when fired, but never schedule new tasks mid-advance (the
      // driver applies model updates only after advance returns), so a fresh
      // scan each iteration is safe and simple.
      val due = lock.synchronized {
        tasks.iterator.filter(t => t.active && t.nextFireAt <= target).minByOption(_.nextFireAt)
      }
      due match
        case None => more = false
        case Some(t) =>
          lock.synchronized {
            nowMillis = t.nextFireAt
            t.nextFireAt += t.periodMillis
          }
          t.run()
    lock.synchronized {
      if nowMillis < target then nowMillis = target
    }
