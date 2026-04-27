package termflow.apps.themes

import termflow.tui.*
import termflow.tui.Theme.themed
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

/**
 * Tiny demo of theme + border-character switching at runtime.
 *
 * Press `Tab` to cycle through the shipped themes; press `q` to quit.
 * Demonstrates that the same `view` produces visibly different chrome
 * depending only on the ambient `Theme` value — no widget code is forked
 * to support the alternate visuals.
 */
object ThemeDemoApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  private val themes: Vector[(String, Theme)] = Vector(
    "dark (sharp)"   -> Theme.dark,
    "rounded"        -> Theme.rounded,
    "double"         -> Theme.dark.copy(chars = BorderChars.double),
    "ascii fallback" -> Theme.dark.copy(chars = BorderChars.ascii),
    "light"          -> Theme.light,
    "mono"           -> Theme.mono
  )

  final case class Model(themeIndex: Int, input: Sub[Msg])

  enum Msg:
    case Cycle
    case Quit
    case Key(k: KeyDecoder.InputKey)
    case KeyError(t: Throwable)

  import Msg._

  object App extends TuiApp[Model, Msg]:

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        themeIndex = 0,
        input = Sub.InputKey(k => Key(k), e => KeyError(e), ctx)
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Cycle       => m.copy(themeIndex = (m.themeIndex + 1) % themes.size).tui
        case Quit        => Tui(m, Cmd.Exit)
        case KeyError(_) => m.tui
        case Key(k) =>
          import KeyDecoder.InputKey.*
          k match
            case CharKey('\t') => Tui(m, Cmd.GCmd(Cycle))
            case CharKey(' ')  => Tui(m, Cmd.GCmd(Cycle))
            case CharKey('q')  => Tui(m, Cmd.GCmd(Quit))
            case CharKey('Q')  => Tui(m, Cmd.GCmd(Quit))
            case Escape        => Tui(m, Cmd.GCmd(Quit))
            case _             => m.tui

    override def view(m: Model): RootNode =
      val (name, theme)   = themes(m.themeIndex)
      given Theme         = theme
      val width           = 50
      val title           = s" Theme: $name "
      val titlePadded     = title + (" " * math.max(0, width - 2 - title.length))
      val instructionLine = "  Tab/Space → cycle theme    q/Esc → quit"
      val box             = Theme.box(2.x, 2.y, width, 6)
      val titleNode       = TextNode(3.x, 2.y, List(Text(title, Style(fg = theme.primary, bold = true))))
      val sampleNode = TextNode(
        4.x,
        4.y,
        List(
          "Status: ".text,
          "OK".themed(_.success),
          "  ".text,
          "Warning: ".text,
          "12".themed(_.warning),
          "  ".text,
          "Error: ".text,
          "0".themed(_.error)
        )
      )
      val helpNode = TextNode(2.x, 9.y, List(Text(instructionLine, Style(fg = theme.foreground))))
      val _        = titlePadded
      RootNode(
        width = math.max(width + 4, 60),
        height = 11,
        children = List(box, titleNode, sampleNode, helpNode),
        input = None
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      Left(TermFlowError.Validation("ThemeDemo has no prompt"))
