package termflow.run.jline

import java.io.{ InputStreamReader, Reader }
import java.util.concurrent.{ LinkedBlockingQueue, TimeUnit }
import org.jline.terminal.TerminalBuilder
import org.jline.utils.InfoCmp.Capability

object AsyncKeyReader {
  def main(args: Array[String]): Unit = {
    val ESC            = 27
    val ESC_TIMEOUT_MS = 100L

    val queue    = new LinkedBlockingQueue[Integer]()
    val terminal = TerminalBuilder.builder().system(true).jna(true).build()
    terminal.enterRawMode()
    val reader: Reader = new InputStreamReader(terminal.input())

    val producer = new Thread(
      new Runnable {
        override def run(): Unit =
          try {
            var running = true
            while (running) {
              val c = reader.read()
              if (c == -1) running = false
              else queue.put(c)
            }
          } catch {
            case _: InterruptedException => ()
          }
      },
      "stdin-reader"
    )

    producer.setDaemon(true)
    producer.start()

    println("🧭  Async Key Inspector\nPress keys (Ctrl+C/D to quit)\n")

    var running = true
    while (running) {
      val c = queue.take().intValue()

      if (c == 3 || c == 4) {
        running = false
      } else if (c == ESC) {
        val next: Integer =
          queue.poll(ESC_TIMEOUT_MS, TimeUnit.MILLISECONDS)
        Option(next).map(_.intValue()) match {
          case None =>
            println("ESC pressed alone")
          case Some(n) =>
            println(s"Sequence start: [$ESC $n]")
            var done   = false
            var buffer = List(n)
            while (!done) {
              val maybeN: Integer =
                queue.poll(5, TimeUnit.MILLISECONDS)
              Option(maybeN).map(_.intValue()) match {
                case None =>
                  done = true
                case Some(v) =>
                  buffer = buffer :+ v
              }
            }
            println(s"ESC sequence bytes: ${buffer.mkString(" ")}")
        }
      } else {
        println(s"Normal key: ${c.toChar} ($c)")
      }
    }

    // --- Cleanup ---
    terminal.puts(Capability.cursor_visible)
    terminal.puts(Capability.clear_screen)
    terminal.flush()
    println("\n👋 Exiting raw mode, goodbye!")
  }
}
