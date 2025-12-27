package termflow.tui

import termflow.tui.KeyDecoder.InputKey

import java.io.Reader
import java.util.concurrent.{ Executors, TimeUnit }

trait Sub[+Msg] {
  def isActive: Boolean
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
      private val scheduler        = Executors.newSingleThreadScheduledExecutor(Thread.ofVirtual().factory())
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

  def InputKey[Msg](
    source: TerminalKeySource,
    msg: InputKey => Msg,
    onError: Throwable => Msg,
    sink: EventSink[Msg]
  ): Sub[Msg] =
    new Sub[Msg] {
      @volatile private var active = true

      private val thread = Thread
        .ofVirtual()
        .start(new Runnable {
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
      private val scheduler        = Executors.newSingleThreadScheduledExecutor(Thread.ofVirtual().factory())
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

  /** Convenience constructor that uses a console reader (JLine-based by default). */
  def InputKey[Msg](
    msg: InputKey => Msg,
    onError: Throwable => Msg,
    sink: EventSink[Msg],
    reader: Reader = ConsoleKeyPressSource.JLineReader()
  ): Sub[Msg] =
    InputKey(ConsoleKeyPressSource(reader), msg, onError, sink)

  /** Convenience constructor that uses the shared terminal backend from the runtime context. */
  def InputKey[Msg](msg: InputKey => Msg, onError: Throwable => Msg, ctx: RuntimeCtx[Msg]): Sub[Msg] =
    InputKey(ConsoleKeyPressSource(ctx.terminal.reader), msg, onError, ctx)
}

trait EventSink[Msg] {
  def publish(cmd: Cmd[Msg]): Unit
}

trait EventSource[Msg] {
  def start(sink: EventSink[Msg]): Sub[Msg]
}

object RandomUtil {
  final class RandomSourceAtFixedRate[A, Msg](period: Long, next: () => A, toMsg: A => Msg) {
    def asEventSource(sink: EventSink[Msg]): Sub[Msg] = {
      val scheduler = Executors.newSingleThreadScheduledExecutor(Thread.ofVirtual().factory())
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
