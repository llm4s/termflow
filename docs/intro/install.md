# Install

TermFlow is published to Maven Central. You'll need:

- **JVM 21 or newer** (TermFlow uses `java.text.BreakIterator` and modern
  `Locale` APIs).
- **sbt** (or any tool that can resolve Maven artefacts).
- A **real interactive terminal**. Piping stdin/stdout into TermFlow
  drops JLine into dumb-terminal mode and the rendering breaks.

## sbt

```scala
libraryDependencies += "org.llm4s" %% "termflow" % "0.4.0"
```

The `termflow` umbrella pulls in all four modules. To depend on a single
layer instead:

```scala
libraryDependencies ++= Seq(
  "org.llm4s" %% "termflow-terminal" % "0.4.0",
  "org.llm4s" %% "termflow-screen"   % "0.4.0",
  "org.llm4s" %% "termflow-app"      % "0.4.0",
  "org.llm4s" %% "termflow-widgets"  % "0.4.0"
)
```

For tests, add the testkit on the test classpath:

```scala
libraryDependencies += "org.llm4s" %% "termflow-testkit" % "0.4.0" % Test
```

## Scala versions

The `main` branch is **Scala 3 only**. If you need Scala 2.13, use
artefacts published from the `legacy-213-track` branch instead.

## Verifying

Drop a minimum app into your project and run it:

```scala
import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

object Hello:
  case class Model(message: String, input: Sub[Msg])

  enum Msg:
    case Quit

  object App extends TuiApp[Model, Msg]:
    def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val keys = Sub.InputKey[Msg](_ => Msg.Quit, _ => Msg.Quit, ctx)
      Model("Hello, TermFlow!", keys).tui

    def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]) = Tui(m, Cmd.Exit)
    def view(m: Model): RootNode =
      RootNode(40, 3, List(TextNode(2.x, 1.y, List(m.message.text))), input = None)
    def toMsg(input: PromptLine): Result[Msg] = Right(Msg.Quit)

  def main(args: Array[String]): Unit = TuiRuntime.run(App)
```

If it draws, you're ready. The
[Hello, World tutorial](../tut/01-hello-world.md) takes this skeleton and
walks it line by line.
