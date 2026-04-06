package termflow.apps.chat

import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*

object ProviderChatRenderReproApp:

  enum Role:
    case System
    case User
    case Assistant

  final case class Entry(role: Role, content: String)

  final case class Model(
    terminalWidth: Int,
    terminalHeight: Int,
    prompt: PromptHistory.State,
    entries: Vector[Entry],
    seedRound: Int,
    status: String
  )

  enum Msg:
    case Resize(width: Int, height: Int)
    case ConsoleInputKey(key: KeyDecoder.InputKey)
    case ConsoleInputError(error: Throwable)
    case RunCommand(command: String)
    case ExitRequested

  import Msg.*

  object App extends TuiApp[Model, Msg]:
    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      Sub.InputKey(ConsoleInputKey.apply, ConsoleInputError.apply, ctx)
      Sub.TerminalResize(250L, Resize.apply, ctx)

      Model(
        terminalWidth = ctx.terminal.width,
        terminalHeight = ctx.terminal.height,
        prompt = PromptHistory.initial(termflow.tui.InMemoryHistoryStore(maxEntries = 100)),
        entries = seedEntries,
        seedRound = 0,
        status = "Type text to append a fake turn. Commands: seed, clear, help, quit."
      ).tui

    override def update(model: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val _ = ctx
      msg match
        case Resize(width, height) =>
          model.copy(terminalWidth = width, terminalHeight = height).tui

        case ConsoleInputError(error) =>
          model.copy(status = s"input error: ${Option(error.getMessage).getOrElse("unknown")}").tui

        case ExitRequested =>
          Tui(model, Cmd.Exit)

        case RunCommand(command) =>
          handleCommand(model, command)

        case ConsoleInputKey(key) =>
          val (nextPrompt, maybeCmd) = PromptHistory.handleKey[Msg](model.prompt, key)(toMsg)
          maybeCmd match
            case Some(cmd) => Tui(model.copy(prompt = nextPrompt), cmd)
            case None      => model.copy(prompt = nextPrompt).tui

    override def view(model: Model): RootNode =
      val width              = math.max(model.terminalWidth, 72)
      val height             = math.max(model.terminalHeight, 20)
      val outerWidth         = math.max(68, width - 4)
      val leftWidth          = weightedWidth(outerWidth, 1, Vector(42, 26), 0)
      val rightWidth         = weightedWidth(outerWidth, 1, Vector(42, 26), 1)
      val panelTop           = 6
      val panelHeight        = math.max(10, height - 12)
      val transcriptCapacity = math.max(1, panelHeight - 2)
      val promptRow          = panelTop + panelHeight + 3
      val renderedPrompt     = PromptHistory.renderWithPrefix(model.prompt, "repro> ")

      val transcriptLines =
        fixedPanelRows(renderVisibleTranscript(model.entries, leftWidth - 4, transcriptCapacity), transcriptCapacity)
      val sidebarLines =
        fixedPanelRows(
          clipPanel(renderSidebar(model, rightWidth - 4), transcriptCapacity, "sidebar"),
          transcriptCapacity
        )

      val children: List[VNode] =
        List(
          BoxNode(1.x, 1.y, outerWidth, 4, children = Nil, style = Style(border = true, fg = Color.Cyan)),
          TextNode(
            3.x,
            2.y,
            List("Provider Chat Render Repro".text(Style(fg = Color.Yellow, bold = true, underline = true)))
          ),
          TextNode(3.x, 3.y, List(fixedWidth(model.status, outerWidth - 4).text(Style(fg = Color.Green)))),
          BoxNode(
            1.x,
            panelTop.y,
            leftWidth,
            panelHeight,
            children = Nil,
            style = Style(border = true, fg = Color.Blue)
          ),
          BoxNode(
            (leftWidth + 2).x,
            panelTop.y,
            rightWidth,
            panelHeight,
            children = Nil,
            style = Style(border = true, fg = Color.Magenta)
          ),
          TextNode(3.x, (panelTop + 1).y, List("Transcript".text(Style(fg = Color.Yellow, bold = true)))),
          TextNode((leftWidth + 4).x, (panelTop + 1).y, List("Session".text(Style(fg = Color.Yellow, bold = true)))),
          TextNode(
            3.x,
            (promptRow - 1).y,
            List(
              fixedWidth(
                "Repro: seed, clear, help, quit | ArrowUp/Down recall prompt history",
                outerWidth - 4
              ).text(Style(fg = Color.Cyan))
            )
          )
        ) ++
          transcriptLines.zipWithIndex.map { case (line, idx) =>
            TextNode(3.x, (panelTop + 2 + idx).y, List(fixedWidth(line, leftWidth - 4).text))
          } ++
          sidebarLines.zipWithIndex.map { case (line, idx) =>
            TextNode((leftWidth + 4).x, (panelTop + 2 + idx).y, List(fixedWidth(line, rightWidth - 4).text))
          }

      RootNode(
        width = width,
        height = height,
        children = children,
        input = Some(
          InputNode(
            3.x,
            promptRow.y,
            renderedPrompt.text,
            Style(fg = Color.Green),
            cursor = renderedPrompt.cursorIndex,
            lineWidth = outerWidth - 4,
            prefixLength = renderedPrompt.prefixLength
          )
        )
      )

    override def toMsg(input: PromptLine): Result[Msg] =
      input.value.trim match
        case ""                     => Right(RunCommand("help"))
        case "quit" | "exit" | ":q" => Right(ExitRequested)
        case other                  => Right(RunCommand(other))

  private def handleCommand(model: Model, command: String): Tui[Model, Msg] =
    command.trim match
      case "help" =>
        model.copy(status = "Commands: seed, clear, help, quit. Any other text appends a fake user+assistant turn.").tui
      case "seed" =>
        val nextBurst = seedBurst(model.seedRound)
        model
          .copy(
            entries = model.entries ++ nextBurst,
            seedRound = model.seedRound + 1,
            status = s"Added ${nextBurst.length} seeded transcript entries in round ${model.seedRound + 1}."
          )
          .tui
      case "clear" =>
        model.copy(entries = Vector.empty, seedRound = 0, status = "Transcript cleared.").tui
      case other =>
        val nextEntries = model.entries ++ Vector(
          Entry(Role.User, other),
          Entry(
            Role.Assistant,
            s"This is a synthetic assistant reply for render debugging. It intentionally wraps across multiple lines and grows the transcript without involving any llm call. Echoed input: $other"
          )
        )
        model.copy(entries = nextEntries, status = s"Added a fake turn. Transcript entries=${nextEntries.length}.").tui

  private def renderVisibleTranscript(entries: Vector[Entry], width: Int, capacity: Int): List[String] =
    if entries.isEmpty then
      clipPanelFromEnd(wrap("No transcript entries yet. Type something or use `seed`.", width), capacity, "transcript")
    else
      val visibleLines = scala.collection.mutable.ListBuffer.empty[String]
      var hiddenLines  = 0

      entries.reverseIterator.foreach { entry =>
        val label = entry.role match
          case Role.System    => "system"
          case Role.User      => "you"
          case Role.Assistant => "assistant"
        val block = wrap(s"$label: ${entry.content}", width) ++ List("")
        if visibleLines.length + block.length <= capacity then visibleLines.prependAll(block.reverseIterator)
        else hiddenLines += block.length
      }

      val lines = visibleLines.toList
      if hiddenLines > 0 && capacity > 1 then
        overflowLineFromTop(hiddenLines, "transcript") +: lines.takeRight(capacity - 1)
      else lines.takeRight(capacity)

  private def renderSidebar(model: Model, width: Int): List[String] =
    List(
      "Mode: repro",
      "",
      "Purpose:",
      "Isolate redraw behavior",
      "for transcript + sidebar",
      "+ bottom prompt layout.",
      "",
      "Commands:",
      "help",
      "seed",
      "clear",
      "quit",
      "",
      "Prompt history:",
      "ArrowUp / ArrowDown",
      "",
      s"Seed round: ${model.seedRound}",
      "",
      s"Entries: ${model.entries.length}"
    ).flatMap(line => wrap(line, width))

  private def clipPanel(lines: List[String], capacity: Int, area: String): List[String] =
    val overflow = math.max(0, lines.length - capacity)
    if overflow > 0 && capacity > 1 then lines.take(capacity - 1) :+ overflowLine(overflow, area)
    else lines.take(capacity)

  private def clipPanelFromEnd(lines: List[String], capacity: Int, area: String): List[String] =
    val overflow = math.max(0, lines.length - capacity)
    if overflow > 0 && capacity > 1 then overflowLineFromTop(overflow, area) +: lines.takeRight(capacity - 1)
    else lines.takeRight(capacity)

  private def fixedPanelRows(lines: List[String], capacity: Int): List[String] =
    if capacity <= 0 then Nil
    else
      val filled = lines.take(capacity)
      filled ++ List.fill(math.max(0, capacity - filled.length))("")

  private def weightedWidth(totalWidth: Int, gapWidth: Int, weights: Vector[Int], index: Int): Int =
    val gaps       = math.max(0, weights.length - 1) * math.max(0, gapWidth)
    val usable     = math.max(weights.length, totalWidth - gaps)
    val totalShare = math.max(1, weights.sum)
    val raw        = weights.map(weight => usable.toDouble * weight.toDouble / totalShare.toDouble)
    val base       = raw.map(math.floor(_).toInt)
    val remainder  = math.max(0, usable - base.sum)
    val ranked =
      raw.zipWithIndex
        .sortBy { case (value, idx) => (-1.0 * (value - math.floor(value)), idx) }
        .take(remainder)
        .map(_._2)
        .toSet
    val widths = base.zipWithIndex.map { case (value, idx) =>
      value + (if ranked.contains(idx) then 1 else 0)
    }
    widths.lift(index).getOrElse(0)

  private def wrap(text: String, width: Int): List[String] =
    if width <= 0 then Nil
    else
      val words = text.split("\\s+").toList.filter(_.nonEmpty)
      if words.isEmpty then List("")
      else
        val lines   = scala.collection.mutable.ListBuffer.empty[String]
        val current = new StringBuilder
        words.foreach { word =>
          val candidate = if current.isEmpty then word else s"${current.toString} $word"
          if candidate.length <= width then
            current.clear()
            current.append(candidate)
          else if current.nonEmpty then
            lines += current.toString
            current.clear()
            if word.length <= width then current.append(word)
            else
              val chunks = word.grouped(math.max(1, width - 1)).toList
              chunks.dropRight(1).foreach(chunk => lines += chunk)
              current.append(chunks.last)
          else
            val chunks = word.grouped(math.max(1, width - 1)).toList
            chunks.dropRight(1).foreach(chunk => lines += chunk)
            current.append(chunks.last)
        }
        if current.nonEmpty then lines += current.toString
        lines.toList

  private def fixedWidth(text: String, width: Int): String =
    val clipped = truncate(text, width)
    if width <= 0 then "" else clipped.padTo(width, ' ')

  private def truncate(text: String, width: Int): String =
    if width <= 0 then ""
    else if text.length <= width then text
    else if width == 1 then "…"
    else text.take(width - 1) + "…"

  private def overflowLine(hiddenLines: Int, area: String): String =
    s"... $hiddenLines more line(s) hidden in $area; enlarge terminal for more"

  private def overflowLineFromTop(hiddenLines: Int, area: String): String =
    s"... $hiddenLines older line(s) hidden in $area; enlarge terminal for more"

  private val seedEntries: Vector[Entry] = Vector(
    Entry(Role.System, "Repro ready. Use this sample to observe redraw behavior without any real llm requests."),
    Entry(Role.System, "Resize the terminal, type quickly, and use `seed` to grow the transcript.")
  )

  private val seedBursts: Vector[Vector[Entry]] =
    Vector(
      Vector(
        Entry(Role.User, "ok"),
        Entry(Role.Assistant, "Short reply."),
        Entry(Role.User, "Give me one medium answer about storms in three sentences."),
        Entry(
          Role.Assistant,
          "Storms gather when warm and cold air masses collide, creating unstable rising currents. Pressure shifts and moisture feed clouds until rain bands organize. In a terminal view, this kind of medium response is useful because it wraps a little without dominating the panel."
        ),
        Entry(Role.User, "Now something much longer about clocks, thunder, and lanterns."),
        Entry(
          Role.Assistant,
          "The watchmaker kept three lanterns in the workshop window, one for clear weather, one for rain, and one for the kind of thunderstorm that made the brass tools hum before the first strike arrived. Every apprentice knew the ritual: close the shutters halfway, cover the smallest gears, and count the seconds between lightning and sound. This synthetic paragraph is deliberately long so it crosses multiple wrap boundaries and makes the transcript viewport behave more like a real conversation where answer lengths are uneven and not neatly predictable."
        )
      ),
      Vector(
        Entry(Role.User, "Tell me a story about a watchmaker in a storm."),
        Entry(
          Role.Assistant,
          "A long synthetic answer follows. It contains enough wrapped text to stress transcript rendering and repaint behavior in the sample app without involving network calls or provider configuration."
        ),
        Entry(Role.User, "Now make it a logic puzzle."),
        Entry(
          Role.Assistant,
          "Another long synthetic answer follows. It intentionally adds more wrapped content so the visible transcript must clip older lines while the prompt remains active at the bottom of the screen."
        ),
        Entry(Role.User, "Thanks, one more variation."),
        Entry(
          Role.Assistant,
          "A third long synthetic answer continues the same pattern for redraw testing. The goal is not semantic quality but predictable, repeatable transcript growth."
        )
      ),
      Vector(
        Entry(Role.User, "One sentence only."),
        Entry(Role.Assistant, "Tiny synthetic reply for short-line variance."),
        Entry(Role.User, "Explain repaint artifacts in a medium paragraph."),
        Entry(
          Role.Assistant,
          "Repaint artifacts often show up when the same region is redrawn with slightly different node shapes from frame to frame. Even if the content is correct, small differences in line count, padding, or cursor placement can make terminal output feel unstable."
        ),
        Entry(Role.User, "Now add a very long answer with awkward wrapping near the panel width."),
        Entry(
          Role.Assistant,
          "This extra-long synthetic block is designed to create wrapping that lands near the right edge of the transcript area, where clipped words, narrow gaps, and mixed sentence lengths can expose redraw issues that a more regular transcript would hide. It also varies rhythm by mixing short clauses with longer ones, which helps us avoid a steady-state pattern where every batch looks the same to the renderer."
        )
      )
    )

  private def seedBurst(round: Int): Vector[Entry] =
    val offset = math.floorMod(round, seedBursts.length)
    (seedBursts.drop(offset) ++ seedBursts.take(offset)).flatten
