package termflow.run.jline

import termflow.tui.ANSI
import termflow.tui.ConsoleKeyPressSource
import termflow.tui.InputRead
import termflow.tui.JLineTerminalBackend

object KeyPressInspector:
  def main(args: Array[String]): Unit =
    val _         = args
    val backend   = new JLineTerminalBackend()
    val inputKeys = ConsoleKeyPressSource(backend.reader)
    var running   = true
    print(ANSI.enterAltBuffer)
    println("🧭  Raw Input Inspector")
    println("Press any key to see its details, Ctrl+C or Ctrl+D to exit.\n")

    while running do
      inputKeys.next() match
        case InputRead.Key(key) =>
          println(key)
        case InputRead.Failed(err) =>
          err.printStackTrace()
        case InputRead.End =>
          running = false
    backend.close()
    print(ANSI.exitAltBuffer)
