package termflow.tui

import termflow.tui.KeyDecoder.InputKey

import java.io.Reader
import java.util.concurrent.{ Executors, TimeUnit }

/**
 * A subscription represents a long-running background task that emits messages.
 *
 * Subscriptions are used for:
 *  - Keyboard input (`Sub.InputKey`)
 *  - Timers (`Sub.Every`)
 *  - Terminal resize detection (`Sub.TerminalResize`)
 *
 * Subscriptions should be cancelled when no longer needed to free resources.
 * Register subscriptions with `RuntimeCtx.registerSub` for automatic cleanup on exit.
 *
 * @tparam Msg The message type emitted by this subscription
 */
trait Sub[+Msg] {

  /** Returns true if the subscription is still active. */
  def isActive: Boolean

  /** Cancel the subscription and release associated resources. */
  def cancel(): Unit
}

object Sub {
  case object NoSub extends Sub[Nothing] {
    override def isActive: Boolean = false
    override def cancel(): Unit    = ()
  }

  def Every[Msg](millis: Long, msg: () => Msg, sink: EventSink[Msg]): Sub[Msg] =
    new Sub[Msg] {
      @volatile private var active = true
      private val scheduler        = Executors.newSingleThreadScheduledExecutor(ThreadUtils.newThreadFactory())
      private val handle =
        scheduler.scheduleAtFixedRate(
          new Runnable {
            override def run(): Unit =
              sink.publish(Cmd.GCmd[Msg](msg()))
          },
          0L,
          millis,
          TimeUnit.MILLISECONDS
        )

      override def isActive: Boolean = active

      override def cancel(): Unit = {
        active = false
        handle.cancel(true)
        scheduler.shutdownNow()
      }
    }

  /** Create an InputKey subscription from a TerminalKeySource. */
  def InputKeyFromSource[Msg](
    source: TerminalKeySource,
    msg: InputKey => Msg,
    onError: Throwable => Msg,
    sink: EventSink[Msg]
  ): Sub[Msg] =
    new Sub[Msg] {
      @volatile private var active = true

      private val thread = ThreadUtils.startThread(new Runnable {
        override def run(): Unit =
          try
            while (active)
              source
                .next()
                .fold(
                  err => sink.publish(Cmd.GCmd(onError(err))),
                  key => sink.publish(Cmd.GCmd(msg(key)))
                )
          catch {
            case _: InterruptedException =>
              ()
            case e: Throwable =>
              sink.publish(Cmd.GCmd(onError(e)))
          }
      })

      override def isActive: Boolean = active

      override def cancel(): Unit = {
        active = false
        thread.interrupt()
      }
    }

  /** Poll terminal dimensions and emit a message when they change. */
  def TerminalResize[Msg](
    millis: Long,
    mkMsg: (Int, Int) => Msg,
    ctx: RuntimeCtx[Msg]
  ): Sub[Msg] =
    new Sub[Msg] {
      @volatile private var active = true
      private val scheduler        = Executors.newSingleThreadScheduledExecutor(ThreadUtils.newThreadFactory())
      @volatile private var w0     = ctx.terminal.width
      @volatile private var h0     = ctx.terminal.height

      private val handle =
        scheduler.scheduleAtFixedRate(
          new Runnable {
            override def run(): Unit =
              if (active) {
                val w = ctx.terminal.width
                val h = ctx.terminal.height
                if (w != w0 || h != h0) {
                  w0 = w
                  h0 = h
                  ctx.publish(Cmd.GCmd(mkMsg(w, h)))
                }
              }
          },
          0L,
          millis,
          TimeUnit.MILLISECONDS
        )

      override def isActive: Boolean = active

      override def cancel(): Unit = {
        active = false
        handle.cancel(true)
        scheduler.shutdownNow()
      }
    }

  /** Create an InputKey subscription with a console reader (JLine-based by default). */
  def InputKeyWithReader[Msg](
    msg: InputKey => Msg,
    onError: Throwable => Msg,
    sink: EventSink[Msg],
    reader: Reader = ConsoleKeyPressSource.JLineReader()
  ): Sub[Msg] =
    InputKeyFromSource(ConsoleKeyPressSource(reader), msg, onError, sink)

  /** Create an InputKey subscription using the shared terminal backend from the runtime context. */
  def InputKey[Msg](msg: InputKey => Msg, onError: Throwable => Msg, ctx: RuntimeCtx[Msg]): Sub[Msg] =
    InputKeyFromSource(ConsoleKeyPressSource(ctx.terminal.reader), msg, onError, ctx)
}

/**
 * Sink that receives commands from subscriptions and other sources.
 * Commands are queued and processed by the runtime loop.
 */
trait EventSink[Msg] {

  /** Publish a command to be processed by the runtime. */
  def publish(cmd: Cmd[Msg]): Unit
}

/**
 * Source that produces events and sends them to a sink.
 */
trait EventSource[Msg] {

  /** Start the source and return a subscription handle. */
  def start(sink: EventSink[Msg]): Sub[Msg]
}

object RandomUtil {
  final class RandomSourceAtFixedRate[A, Msg](period: Long, next: () => A, toMsg: A => Msg) {
    def asEventSource(sink: EventSink[Msg]): Sub[Msg] = {
      val scheduler = Executors.newSingleThreadScheduledExecutor(ThreadUtils.newThreadFactory())
      scheduler.scheduleAtFixedRate(
        new Runnable {
          override def run(): Unit = {
            val value = next()
            sink.publish(Cmd.GCmd[Msg](toMsg(value)))
          }
        },
        0L,
        period,
        TimeUnit.MILLISECONDS
      )
      new Sub[Msg] {
        @volatile private var active = true
        def isActive: Boolean        = active
        def cancel(): Unit = {
          scheduler.shutdownNow()
          active = false
        }
      }
    }
  }
}
