package termflow.tui

import org.jline.terminal.Terminal
import org.jline.terminal.TerminalBuilder
import termflow.tui.AsciiControl._
import termflow.tui.KeyDecoder.InputKey

import java.io.InputStreamReader
import java.io.Reader
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import scala.annotation.tailrec
import scala.util.Success
import scala.util.Try

enum InputRead:
  case Key(key: InputKey)
  case End
  case Failed(cause: Throwable)

trait TerminalKeySource:
  def next(): InputRead
  def close(): Try[Unit]

object ConsoleKeyPressSource:
  private val EndOfStream = Int.MinValue

  /** Default JLine-based reader, mainly used in tests and tools. */
  def JLineReader(): Reader =
    val terminal: Terminal =
      TerminalBuilder
        .builder()
        .system(true)
        .jna(true)
        .build()
    terminal.enterRawMode()
    val input = terminal.input()
    new InputStreamReader(input)

  @tailrec
  private def processKey(c: Int, queueBridge: BlockingQueue[Integer], buffer: List[Int]): InputKey =
    (c, buffer) match
      case (x, Nil) if x != ESC =>
        KeyDecoder.decode(x)

      case (ESC, Nil) =>
        val next: Integer =
          queueBridge.poll(50, java.util.concurrent.TimeUnit.MILLISECONDS)
        Option(next).map(_.intValue()) match
          case None =>
            InputKey.Escape
          case Some(`EndOfStream`) =>
            queueBridge.put(EndOfStream)
            InputKey.Escape
          case Some(n) =>
            processKey(n, queueBridge, ESC :: Nil)

      case (O, ESC :: Nil) =>
        processKey(queueBridge.take(), queueBridge, buffer :+ c)
      case (`[`, ESC :: Nil) =>
        processKey(queueBridge.take(), queueBridge, buffer :+ c)

      case (P, List(ESC, O)) =>
        InputKey.F1
      case (Q, List(ESC, O)) =>
        InputKey.F2
      case (R, List(ESC, O)) =>
        InputKey.F3
      case (S, List(ESC, O)) =>
        InputKey.F4

      case (A, List(ESC, `[`)) =>
        InputKey.ArrowUp
      case (B, List(ESC, `[`)) =>
        InputKey.ArrowDown
      case (C, List(ESC, `[`)) =>
        InputKey.ArrowRight
      case (D, List(ESC, `[`)) =>
        InputKey.ArrowLeft

      case (H, List(ESC, `[`)) =>
        InputKey.Home
      case (F, List(ESC, `[`)) =>
        InputKey.End
      case (H, List(ESC, O)) =>
        InputKey.Home
      case (F, List(ESC, O)) =>
        InputKey.End

      // Delete key: ESC [ 3 ~
      case (`~`, List(ESC, `[`, `51`)) =>
        InputKey.Delete

      case (x, List(ESC, `[`)) =>
        processKey(queueBridge.take(), queueBridge, buffer :+ x)
      case (x, List(ESC, `[`, `49`)) =>
        processKey(queueBridge.take(), queueBridge, buffer :+ x)
      case (x, List(ESC, `[`, `50`)) =>
        processKey(queueBridge.take(), queueBridge, buffer :+ x)

      case (`~`, List(ESC, `[`, `49`, `53`)) =>
        InputKey.F5
      case (`~`, List(ESC, `[`, `49`, `55`)) =>
        InputKey.F6
      case (`~`, List(ESC, `[`, `49`, `56`)) =>
        InputKey.F7
      case (`~`, List(ESC, `[`, `49`, `57`)) =>
        InputKey.F8
      case (`~`, List(ESC, `[`, `50`, `48`)) =>
        InputKey.F9
      case (`~`, List(ESC, `[`, `50`, `49`)) =>
        InputKey.F10
      case (`~`, List(ESC, `[`, `50`, `51`)) =>
        InputKey.F11
      case (`~`, List(ESC, `[`, `50`, `52`)) =>
        InputKey.F12

      case _ =>
        InputKey.Unknown(c.toString)

  /** Create a `TerminalKeySource` from a reader. */
  def apply(reader: Reader = JLineReader()): TerminalKeySource =
    val bridge = new LinkedBlockingQueue[Integer]()

    // Producer: read raw ints from the reader
    val producerThread = ThreadUtils.startThread(() =>
      try
        @tailrec
        def loop(): Unit =
          val c = reader.read()
          if c == -1 then bridge.put(EndOfStream)
          else
            bridge.put(c)
            loop()
        loop()
      catch case _: InterruptedException => ()
    )

    val inputReads = new LinkedBlockingQueue[InputRead]()
    // Decoder: consume ints + escape sequences and emit InputKey
    val decoderThread = ThreadUtils.startThread(() =>
      try
        @tailrec
        def loop(continue: Boolean): Unit =
          if !continue then ()
          else
            val c = bridge.take().intValue()
            if c == EndOfStream then
              inputReads.put(InputRead.End)
              loop(false)
            else
              val key = processKey(c, bridge, Nil)
              inputReads.put(InputRead.Key(key))
              loop(true)
        loop(true)
      catch {
        case _: InterruptedException => ()
      }
    )

    new TerminalKeySource:
      @volatile private var closed = false

      private def joinQuietly(t: Thread): Unit =
        try t.join(200L)
        catch {
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
        }

      override def next(): InputRead =
        try inputReads.take()
        catch
          case e: InterruptedException =>
            InputRead.Failed(e)

      override def close(): Try[Unit] =
        if !closed then
          closed = true
          producerThread.interrupt()
          decoderThread.interrupt()
          val closedReader = Try(reader.close())
          joinQuietly(producerThread)
          joinQuietly(decoderThread)
          closedReader
        else Success(())
