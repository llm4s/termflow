package termflow.tui

import org.jline.terminal.Terminal
import org.jline.terminal.TerminalBuilder

import java.io.InputStreamReader
import java.io.Reader

/** Basic read-only terminal information. */
trait TerminalInfo {
  def width: Int
  def height: Int
}

/** Backend that provides terminal dimensions and a reader for raw key input. */
trait TerminalBackend extends TerminalInfo {
  def reader: Reader
  def close(): Unit
}

/** Default JLine-backed terminal implementation. */
final class JLineTerminalBackend extends TerminalBackend {
  private val terminal: Terminal =
    TerminalBuilder
      .builder()
      .system(true)
      .jna(true)
      .build()

  // Enter raw mode once; Sub / ConsoleKeyPressSource will read from this input stream.
  terminal.enterRawMode()

  private val input = terminal.input()

  override def reader: Reader =
    new InputStreamReader(input)

  override def width: Int  = terminal.getWidth
  override def height: Int = terminal.getHeight

  override def close(): Unit =
    terminal.close()
}
