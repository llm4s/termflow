package termflow.tui

object KeyDecoder {

  /** Normalised key representation used by the TUI. */
  sealed trait InputKey

  object InputKey {
    final case class CharKey(ch: Char) extends InputKey
    final case class Ctrl(ch: Char)    extends InputKey

    case object Backspace                 extends InputKey
    case object Delete                    extends InputKey
    case object Home                      extends InputKey
    case object End                       extends InputKey
    case object Enter                     extends InputKey
    case object Escape                    extends InputKey
    case object ArrowUp                   extends InputKey
    case object ArrowDown                 extends InputKey
    case object ArrowLeft                 extends InputKey
    case object ArrowRight                extends InputKey
    case object F1                        extends InputKey
    case object F2                        extends InputKey
    case object F3                        extends InputKey
    case object F4                        extends InputKey
    case object F5                        extends InputKey
    case object F6                        extends InputKey
    case object F7                        extends InputKey
    case object F8                        extends InputKey
    case object F9                        extends InputKey
    case object F10                       extends InputKey
    case object F11                       extends InputKey
    case object F12                       extends InputKey
    final case class Unknown(seq: String) extends InputKey
    case object EndOfInput                extends InputKey
  }

  import InputKey._

  def decode(key: Int): InputKey =
    key match {
      case 10  => Enter
      case 127 => Backspace
      case code if code >= 1 && code <= 26 =>
        Ctrl(('A' + code - 1).toChar)
      case code if code >= 32 && code <= 126 =>
        CharKey(code.toChar)
      case _ =>
        Unknown(key.toString)
    }
}

object AsciiControl {
  val ESC     = 27
  val O       = 79
  val `[`     = 91
  val P       = 80
  val Q       = 81
  val R       = 82
  val S       = 83
  val A       = 65
  val B       = 66
  val C       = 67
  val D       = 68
  val F       = 70
  val H       = 72
  val `48`    = 48
  val `49`    = 49
  val `53`    = 53
  val `55`    = 55
  val `56`    = 56
  val `50`    = 50
  val `51`    = 51
  val `52`    = 52
  val `57`    = 57
  val `~`     = 126
  val `CTR+C` = 3
  val `CTR+D` = 4
}
