package termflow.run.jline

import termflow.tui.ConsoleKeyPressSource
import termflow.tui.KeyDecoder

import scala.util.{ Failure, Success }

object KeyInspectorMain {
  def main(args: Array[String]): Unit = {
    val source  = ConsoleKeyPressSource()
    var running = true
    var history = List.empty[String]

    def clearScreen(): Unit = {
      // Clear screen and move cursor to home without using the alt buffer
      print("\u001b[2J")
      print("\u001b[H")
    }

    while (running) {
      source.next() match {
        case Failure(e) =>
          history = s"ERROR: ${e.getMessage}" :: history
        case Success(key) =>
          val line = key.toString
          history = line :: history.take(20)
          key match {
            case KeyDecoder.InputKey.Escape =>
              running = false
            case _ =>
              ()
          }
      }

      clearScreen()
      println("KeyInspector – press keys to see decoded InputKey")
      println("Press Escape to exit.")
      println()
      println("Last key:")
      history.headOption.foreach(k => println(s"  $k"))
      println()
      println("Recent keys:")
      history.foreach(k => println(s"  $k"))
    }

    println("Exiting KeyInspector.")
  }
}
