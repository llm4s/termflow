package termflow.apps.unicode

import termflow.tui.*
import termflow.tui.Color.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

/**
 * Smoke-test sample exercising the [[WCWidth]] code path: CJK glyphs,
 * fullwidth ASCII, emoji, and combining marks. Run via `sbt unicodeDemo`.
 *
 * The demo lays out a series of fixed-width "rules" (`|0123456789|`) below
 * lines of mixed Latin / wide / emoji content so the eye can verify that
 * each subsequent column lands where it should.
 */
object UnicodeDemoApp:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)

  final case class Model(
    width: Int,
    height: Int,
    input: Sub[Msg]
  )

  enum Msg:
    case KeyPressed(key: KeyDecoder.InputKey)
    case KeyError(t: Throwable)

  import Msg.*

  object App extends TuiApp[Model, Msg]:

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Model(
        width = ctx.terminal.width,
        height = ctx.terminal.height,
        input = Sub.InputKey(KeyPressed.apply, KeyError.apply, ctx)
      ).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case KeyPressed(KeyDecoder.InputKey.CharKey('q')) => Tui(m, Cmd.Exit)
        case KeyPressed(KeyDecoder.InputKey.Escape)       => Tui(m, Cmd.Exit)
        case KeyPressed(_)                                => m.tui
        case KeyError(_)                                  => m.tui

    override def view(m: Model): RootNode =
      val rule    = "|0123456789|0123456789|0123456789|"
      val ruler   = Text(rule, Style(fg = BrightBlack))
      val title   = Text("TermFlow — Unicode width demo (q to quit)", Style(fg = Yellow, bold = true))
      val ascii   = Text("hello world!", Style(fg = Cyan))
      val cjk     = Text("中文 日本語 한국어", Style(fg = Magenta))
      val mixed   = Text("a中b日c韓", Style(fg = Green))
      val full    = Text("ＡＢＣＤＥ", Style(fg = BrightBlue))
      val emoji   = Text("hello 🎉 world 🚀", Style(fg = BrightYellow))
      val combine = Text("café — naïve — schön", Style(fg = White))

      val column = Layout.Column(
        gap = 0,
        children = List(
          Layout.Elem(TextNode(1.x, 1.y, List(title))),
          Layout.Spacer(1, 1),
          Layout.Elem(TextNode(1.x, 1.y, List(ascii))),
          Layout.Elem(TextNode(1.x, 1.y, List(ruler))),
          Layout.Elem(TextNode(1.x, 1.y, List(cjk))),
          Layout.Elem(TextNode(1.x, 1.y, List(ruler))),
          Layout.Elem(TextNode(1.x, 1.y, List(mixed))),
          Layout.Elem(TextNode(1.x, 1.y, List(ruler))),
          Layout.Elem(TextNode(1.x, 1.y, List(full))),
          Layout.Elem(TextNode(1.x, 1.y, List(ruler))),
          Layout.Elem(TextNode(1.x, 1.y, List(emoji))),
          Layout.Elem(TextNode(1.x, 1.y, List(ruler))),
          Layout.Elem(TextNode(1.x, 1.y, List(combine))),
          Layout.Elem(TextNode(1.x, 1.y, List(ruler)))
        )
      )

      RootNode(
        width = m.width,
        height = m.height,
        children = column.resolve(Coord(2.x, 1.y)),
        input = None
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      Right(KeyPressed(KeyDecoder.InputKey.CharKey('q')))
