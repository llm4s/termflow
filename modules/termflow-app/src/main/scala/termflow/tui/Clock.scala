package termflow.tui

import java.time.Instant
import java.time.ZoneId
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

/**
 * Abstraction over wall-clock time and the periodic scheduler that drives
 * timer subscriptions ([[Sub.Every]]).
 *
 * Two concerns are deliberately bundled behind one interface because both are
 * "the passage of time" from an app's point of view:
 *
 *   - reading the current instant (apps such as `DigitalClock` display it), and
 *   - scheduling a callback to run once per elapsed period (how `Sub.Every`
 *     ticks).
 *
 * Production code uses [[SystemClock]], which reads the real system time and
 * schedules ticks on a background executor. Tests use
 * `termflow.testkit.ManualClock`, whose virtual time only advances on explicit
 * `advance` calls — making `Sub.Every`-driven apps snapshot-testable without
 * real-time delays.
 *
 * @note SPI. A clock is obtained from [[RuntimeCtx.clock]]; apps and
 *       subscriptions read it rather than calling `System`/`java.time`
 *       directly so that the testkit can substitute virtual time.
 */
trait Clock:

  /** Current instant. Apps that display wall-clock time read this. */
  def instant(): Instant

  /** Zone used to derive local date/time values from [[instant]]. */
  def zone: ZoneId

  /**
   * Schedule `task` to run repeatedly, once per `periodMillis` of elapsed
   * time. Returns a handle that cancels the schedule.
   *
   * `periodMillis` must be strictly positive.
   */
  def schedulePeriodic(periodMillis: Long, task: () => Unit): Clock.Cancelable

object Clock:

  /** Handle returned by [[Clock.schedulePeriodic]] used to stop a schedule. */
  trait Cancelable:
    def cancel(): Unit

/**
 * Default [[Clock]] backed by the real system clock and a per-schedule
 * single-threaded executor — the production scheduler behind [[Sub.Every]].
 *
 * Each call to [[schedulePeriodic]] owns one scheduler thread, matching the
 * previous inline `Executors.newSingleThreadScheduledExecutor` wiring; the
 * first tick fires immediately (initial delay `0`) exactly as before.
 */
object SystemClock extends Clock:

  override def instant(): Instant = Instant.now()

  override def zone: ZoneId = ZoneId.systemDefault()

  override def schedulePeriodic(periodMillis: Long, task: () => Unit): Clock.Cancelable =
    val scheduler = Executors.newSingleThreadScheduledExecutor(ThreadUtils.newThreadFactory())
    val handle    = scheduler.scheduleAtFixedRate(() => task(), 0L, periodMillis, TimeUnit.MILLISECONDS)
    new Clock.Cancelable:
      override def cancel(): Unit =
        handle.cancel(true): Unit
        scheduler.shutdownNow(): Unit
        try scheduler.awaitTermination(200L, TimeUnit.MILLISECONDS): Unit
        catch {
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
        }
