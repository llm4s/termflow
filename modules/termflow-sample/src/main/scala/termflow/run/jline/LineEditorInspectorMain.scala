package termflow.run.jline

import termflow.tui.{ ConsoleKeyPressSource, Prompt }
import termflow.tui.KeyDecoder

import scala.util.{ Failure, Success }

object LineEditorInspectorMain {
  def main(args: Array[String]): Unit = {
    val source  = ConsoleKeyPressSource()
    var state   = Prompt.State()
    var running = true

    def clearScreen(): Unit = {
      // Clear screen and move cursor to home without using the alt buffer
      print("\u001b[2J")
      print("\u001b[H")
    }

    def moveTo(x: Int, y: Int): Unit =
      print(s"\u001b[${y};${x}H")

    val prefix = "[]> "

    while (running) {
      source.next() match {
        case Failure(e) =>
          clearScreen()
          moveTo(1, 1)
          println("LineEditor Inspector – error reading key")
          println(s"Error: ${e.getMessage}")
          println("Press Escape to exit.")

        case Success(key) =>
          key match {
            case KeyDecoder.InputKey.Escape =>
              running = false

            case other =>
              // Feed key into the prompt state; ignore produced commands
              val (nextState, _) =
                Prompt.handleKey[Unit](state, other)(_ => Right(()))
              state = nextState

              val text   = Prompt.render(state)
              val cursor = math.max(0, math.min(state.cursor, text.length))

              clearScreen()

              // Render input line with inline visual cursor (underlined character)
              moveTo(1, 1)
              print("\u001b[2K")
              print(prefix)

              val (left, right) = text.splitAt(cursor)
              print(left)
              if (right.nonEmpty) {
                val ch   = right.head
                val tail = right.tail
                // Highlight the character at the cursor with reverse video
                print("\u001b[7m")
                print(ch)
                print("\u001b[0m")
                print(tail)
              } else {
                // Cursor at end: show a highlighted space
                print("\u001b[7m \u001b[0m")
              }

              // Debug info below
              moveTo(1, 3)
              print(s"TEXT   : '$text'")
              moveTo(1, 4)
              print(s"CURSOR : $cursor")
              moveTo(1, 5)
              print(s"LAST   : $other")
          }
      }

      clearScreen()
      println("Exiting LineEditor Inspector.")
    }
  }
}
