package termflow.run.jline

import termflow.tui.ACSUtils
import termflow.tui.ConsoleKeyPressSource

object KeyPressInspectorMain {
  def main(args: Array[String]): Unit = {
    val _         = args
    val inputKeys = ConsoleKeyPressSource()
    ACSUtils.EnterAlternatBuffer()
    println("🧭  Raw Input Inspector")
    println("Press any key to see its details, Ctrl+C or Ctrl+D to exit.\n")

    while (true)
      inputKeys
        .next()
        .fold(
          _.printStackTrace(),
          println
        )
      // exit condition is handled by Ctrl+C / Ctrl+D at the process level
    ACSUtils.EnterNormalBuffer()
  }
}
