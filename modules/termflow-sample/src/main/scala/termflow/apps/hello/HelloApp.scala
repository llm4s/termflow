package termflow.apps.hello

import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

/**
 * Companion source for the docs site's "Hello, World" tutorial. Kept in
 * the sample module so the tutorial code path is exercised by the sbt
 * compile gate — if the API drifts, this file fails to compile before
 * the docs go stale.
 */
object HelloApp:

  final case class Model(message: String, input: Sub[Msg])

  enum Msg:
    case KeyPressed(key: KeyDecoder.InputKey)
    case Quit

  object App extends TuiApp[Model, Msg]:

    def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val keys = Sub.InputKey[Msg](
        msg = key => Msg.KeyPressed(key),
        onError = _ => Msg.Quit,
        ctx = ctx
      )
      Model("Hello, TermFlow!", keys).tui

    def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      msg match
        case Msg.KeyPressed(KeyDecoder.InputKey.CharKey('q')) => Tui(m, Cmd.Exit)
        case Msg.KeyPressed(KeyDecoder.InputKey.Ctrl('C'))    => Tui(m, Cmd.Exit)
        case Msg.KeyPressed(_)                                => m.tui
        case Msg.Quit                                         => Tui(m, Cmd.Exit)

    def view(m: Model): RootNode =
      RootNode(
        width = 40,
        height = 3,
        children = List(
          TextNode(2.x, 1.y, List(m.message.text))
        ),
        input = None
      )

    def toMsg(input: PromptLine): Result[Msg] =
      val _ = input
      Right(Msg.Quit)

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)
