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
import scala.util.Try

trait TerminalKeySource:
  def next(): Try[InputKey]
  def close(): Unit

object ConsoleKeyPressSource:

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
    val producerThread = ThreadUtils.startThread(new Runnable {
      override def run(): Unit =
        try
          def loop(): Unit =
            val c = reader.read()
            if c != -1 then
              bridge.put(c)
              loop()
          loop()
        catch case _: InterruptedException => ()
    })

    val inputKeys = new LinkedBlockingQueue[InputKey]()
    // Decoder: consume ints + escape sequences and emit InputKey
    val decoderThread = ThreadUtils.startThread(new Runnable {
      override def run(): Unit =
        try
          while true do {
            val c   = bridge.take().intValue()
            val key = processKey(c, bridge, Nil)
            inputKeys.put(key)
          }
        catch {
          case _: InterruptedException => ()
        }
    })

    new TerminalKeySource:
      @volatile private var closed = false

      override def next(): Try[InputKey] =
        Try(inputKeys.take())

      override def close(): Unit =
        if !closed then
          closed = true
          producerThread.interrupt()
          decoderThread.interrupt()
