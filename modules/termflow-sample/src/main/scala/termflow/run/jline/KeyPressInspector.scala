package termflow.run.jline

import termflow.tui.ACSUtils
import termflow.tui.ConsoleKeyPressSource
import termflow.tui.InputRead

object KeyPressInspectorMain:
  def main(args: Array[String]): Unit =
    val _         = args
    val inputKeys = ConsoleKeyPressSource()
    var running   = true
    ACSUtils.EnterAlternateBuffer()
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
    ACSUtils.EnterNormalBuffer()
