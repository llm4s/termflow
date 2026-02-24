package termflow.tui

object KeyDecoder:

  /** Normalised key representation used by the TUI. */
  enum InputKey:
    case CharKey(ch: Char)
    case Ctrl(ch: Char)
    case Backspace
    case Delete
    case Home
    case End
    case Enter
    case Escape
    case ArrowUp
    case ArrowDown
    case ArrowLeft
    case ArrowRight
    case F1
    case F2
    case F3
    case F4
    case F5
    case F6
    case F7
    case F8
    case F9
    case F10
    case F11
    case F12
    case Unknown(seq: String)
    case EndOfInput

  import InputKey._

  def decode(key: Int): InputKey =
    key match
      case 10  => Enter
      case 127 => Backspace
      case code if code >= 1 && code <= 26 =>
        Ctrl(('A' + code - 1).toChar)
      case code if code >= 32 && code <= 126 =>
        CharKey(code.toChar)
      case _ =>
        Unknown(key.toString)

object AsciiControl:
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
