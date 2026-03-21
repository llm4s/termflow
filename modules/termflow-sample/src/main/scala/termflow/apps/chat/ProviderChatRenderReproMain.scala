package termflow.apps.chat

import termflow.tui.SimpleANSIRenderer
import termflow.tui.TuiRuntime

object ProviderChatRenderReproMain:

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(ProviderChatRenderReproApp.App, renderer = SimpleANSIRenderer())
