package termflow.apps.chat

import termflow.tui.*
import termflow.tui.Tui.*
import termflow.tui.TuiPrelude.*
import termflow.tui.widgets

/**
 * Streaming chat sample (Stage 4 §3.2).
 *
 * Closes the "expand `chat/` to use streaming + scrollback" gap from
 * the roadmap. Where [[ProviderChatRenderReproApp]] is a render-pipeline
 * harness with whole replies, this app demonstrates:
 *
 *   - **Token-by-token streaming.** Pressing Enter on a message starts
 *     a `Sub.Every(40ms)` that appends one character per tick into the
 *     last assistant entry. The sub auto-cancels when the response is
 *     fully delivered.
 *   - **Real scrollback.** The transcript renders through
 *     `widgets.LogView` with an explicit `scrollOffset` and an
 *     `autoTail` flag. New tokens push the viewport to the bottom only
 *     while the user is auto-tailing; if they've scrolled up, the
 *     viewport stays put until they scroll back down.
 *   - **Layout.Border.** Title + help on top, transcript in the
 *     center, status + prompt on the bottom — no hand-rolled box
 *     coordinates.
 *
 * No real LLM calls. The "responses" are picked from a small canned
 * pool so the streaming behaviour is deterministic.
 *
 * ## Keys
 *
 *   - Type and press `Enter` to send a message; the assistant reply
 *     streams in.
 *   - `↑` / `↓`             — scroll the transcript by one line
 *   - `PageUp` / `PageDown` — scroll by a page
 *   - mouse wheel           — scroll when the cursor is over the transcript
 *                              (3 lines per detent; ignored elsewhere)
 *   - `End`                 — jump to the bottom and re-enable auto-tail
 *   - `Ctrl+L`              — clear the transcript
 *   - `Ctrl+C` / `Esc`      — quit
 *
 * Run with `sbt chatDemo`.
 */
object ChatStreamApp:

  // ---- Domain --------------------------------------------------------------

  enum Role:
    case System, User, Assistant

  final case class Entry(role: Role, content: String):
    def label: String = role match
      case Role.System    => "system"
      case Role.User      => "you"
      case Role.Assistant => "assistant"

  /** In-flight stream: bytes still to deliver and the entry index they go into. */
  final case class Streaming(remaining: String, intoIdx: Int)

  // ---- Model + Msg --------------------------------------------------------

  final case class Model(
    width: Int,
    height: Int,
    entries: Vector[Entry],
    scrollOffset: Int,
    autoTail: Boolean,
    prompt: Prompt.State,
    streaming: Option[Streaming],
    ticker: Sub[Msg],
    status: String,
    input: Sub[Msg]
  )

  enum Msg:
    case ConsoleInputKey(k: KeyDecoder.InputKey)
    case ConsoleInputError(t: Throwable)
    case TokenTick
    case Submit(text: String)
    case ScrollBy(delta: Int)
    case ScrollToEnd
    case Clear
    case Quit

  // ---- Initial model -------------------------------------------------------

  val WelcomeEntries: Vector[Entry] = Vector(
    Entry(Role.System, "Welcome. Type a message and press Enter — the reply streams in."),
    Entry(Role.System, "Scroll with ↑/↓ or PageUp/PageDown · End to re-tail · Ctrl+L to clear · Ctrl+C to quit.")
  )

  /**
   * Pure factory used by tests. The runtime calls this in `init` after
   * subscribing to keyboard input.
   */
  def initialModel(width: Int, height: Int, input: Sub[Msg]): Model =
    Model(
      width = width,
      height = height,
      entries = WelcomeEntries,
      scrollOffset = 0,
      autoTail = true,
      prompt = Prompt.State(),
      streaming = None,
      ticker = Sub.NoSub,
      status = "ready",
      input = input
    )

  // ---- Canned responses ---------------------------------------------------

  /**
   * Pick a pseudo-response based on the input. Deterministic so tests can
   * assert on the streamed output.
   */
  def responseFor(userText: String): String = userText.trim.toLowerCase match
    case ""             => "Say something — anything — and I'll stream a reply back."
    case "hi" | "hello" => "Hi! I'm a deterministic stand-in for a real LLM."
    case "ping"         => "pong"
    case "long" | "story" =>
      "The watchmaker kept three lanterns in the workshop window. " +
        "One for clear weather, one for rain, and one for the kind of thunderstorm " +
        "that made the brass tools hum before the first strike arrived. " +
        "Every apprentice knew the ritual: close the shutters halfway, cover the " +
        "smallest gears, and count the seconds between lightning and sound."
    case other =>
      s"Echo: $other. (No real LLM is connected; this is a streaming-rendering demo.)"

  // ---- Streaming ----------------------------------------------------------

  /** One streamed character into the entry at `s.intoIdx`. */
  private def consumeOneToken(m: Model): Model =
    m.streaming match
      case None                           => m
      case Some(s) if s.remaining.isEmpty =>
        // Done — cancel the ticker, drop streaming state.
        if m.ticker.isActive then m.ticker.cancel()
        m.copy(streaming = None, ticker = Sub.NoSub, status = "ready")
      case Some(s) =>
        val ch        = s.remaining.head
        val remaining = s.remaining.tail
        val target    = m.entries.lift(s.intoIdx)
        target match
          case None =>
            // Entry vanished (e.g. cleared mid-stream); cancel.
            if m.ticker.isActive then m.ticker.cancel()
            m.copy(streaming = None, ticker = Sub.NoSub, status = "interrupted")
          case Some(e) =>
            val updated     = e.copy(content = e.content + ch)
            val nextEntries = m.entries.updated(s.intoIdx, updated)
            m.copy(
              entries = nextEntries,
              streaming = Some(s.copy(remaining = remaining))
            )

  // ---- Scrolling ----------------------------------------------------------

  private def transcriptCapacity(m: Model): Int = math.max(3, m.height - 4)

  private def transcriptWidth(m: Model): Int = math.max(20, m.width - 2)

  /** Origin (top-left) of the transcript pane in absolute terminal cells. */
  private val transcriptOrigin: Coord = Coord(XCoord(2), YCoord(4))

  /** Mouse-wheel viewport for the transcript pane — see `LogView.scrollDelta`. */
  private def transcriptViewport(m: Model): widgets.LogView.Viewport =
    widgets.LogView.Viewport(transcriptOrigin, transcriptWidth(m), transcriptCapacity(m))

  /** Flatten the entries to display lines. Wrapping is delegated to LogView. */
  def transcriptLines(m: Model): Vector[String] =
    m.entries
      .flatMap { e =>
        val first = s"${e.label}: ${e.content}"
        Vector(first, "")
      }
      .dropRight(1)

  private def maxScroll(m: Model): Int =
    widgets.LogView.maxScroll(
      transcriptLines(m),
      width = transcriptWidth(m),
      height = transcriptCapacity(m),
      wrap = true
    )

  private def clampScroll(m: Model): Model =
    val mx     = maxScroll(m)
    val offset = math.max(0, math.min(mx, m.scrollOffset))
    val tail   = offset == mx
    if offset == m.scrollOffset && tail == m.autoTail then m
    else m.copy(scrollOffset = offset, autoTail = tail)

  /** After a model change that changed the buffer length, optionally re-tail. */
  private def afterTextChange(m: Model): Model =
    if m.autoTail then m.copy(scrollOffset = maxScroll(m))
    else clampScroll(m)

  private def scrollBy(m: Model, delta: Int): Model =
    val mx  = maxScroll(m)
    val nxt = math.max(0, math.min(mx, m.scrollOffset + delta))
    m.copy(scrollOffset = nxt, autoTail = nxt == mx)

  // ---- Pure step -----------------------------------------------------------

  enum StepResult:
    case StayInModel(model: Model)
    case ExitNow(model: Model)
    case StartStreaming(model: Model, intervalMs: Long)

  /**
   * Pure transition. The `StartStreaming` result asks the runtime to
   * register a `Sub.Every` and store the resulting `Sub[Msg]` on the
   * model — done in `update` because Sub construction needs `ctx`.
   */
  def step(m: Model, msg: Msg): StepResult =
    import KeyDecoder.InputKey.*
    msg match
      case Msg.Quit                 => StepResult.ExitNow(m)
      case Msg.ConsoleInputError(_) => StepResult.StayInModel(m)
      case Msg.TokenTick            => StepResult.StayInModel(afterTextChange(consumeOneToken(m)))
      case Msg.ScrollBy(d)          => StepResult.StayInModel(scrollBy(m, d))
      case Msg.ScrollToEnd =>
        StepResult.StayInModel(m.copy(scrollOffset = maxScroll(m), autoTail = true))
      case Msg.Clear =>
        if m.ticker.isActive then m.ticker.cancel()
        StepResult.StayInModel(
          m.copy(
            entries = WelcomeEntries,
            streaming = None,
            ticker = Sub.NoSub,
            scrollOffset = 0,
            autoTail = true,
            status = "cleared"
          )
        )
      case Msg.Submit(text) =>
        val trimmed = text.trim
        if trimmed.isEmpty then StepResult.StayInModel(m)
        else
          // Cancel any in-flight stream — replies don't queue up.
          if m.ticker.isActive then m.ticker.cancel()
          val response  = responseFor(trimmed)
          val withUser  = m.entries :+ Entry(Role.User, trimmed)
          val withReply = withUser :+ Entry(Role.Assistant, "")
          val replyIdx  = withReply.size - 1
          val nextModel = m.copy(
            entries = withReply,
            streaming = Some(Streaming(response, replyIdx)),
            status = "streaming…"
          )
          StepResult.StartStreaming(afterTextChange(nextModel), intervalMs = 40L)

      case Msg.ConsoleInputKey(k) =>
        // Scroll keys take priority over the prompt.
        k match
          case ArrowUp            => StepResult.StayInModel(scrollBy(m, -1))
          case ArrowDown          => StepResult.StayInModel(scrollBy(m, +1))
          case PageUp             => StepResult.StayInModel(scrollBy(m, -transcriptCapacity(m)))
          case PageDown           => StepResult.StayInModel(scrollBy(m, +transcriptCapacity(m)))
          case End                => StepResult.StayInModel(m.copy(scrollOffset = maxScroll(m), autoTail = true))
          case Ctrl('L')          => step(m, Msg.Clear)
          case Ctrl('C') | Escape => StepResult.ExitNow(m)
          case Mouse(ev)          =>
            // Mouse-wheel inside the transcript drives scrollback; scrolls
            // outside (over the prompt or status row) are dropped so the
            // user's wheel doesn't accidentally page through history while
            // hovering somewhere unrelated.
            widgets.LogView.scrollDelta(ev, transcriptViewport(m)) match
              case Some(d) => StepResult.StayInModel(scrollBy(m, d))
              case None    => StepResult.StayInModel(m)
          case _ =>
            val (nextPrompt, maybeCmd) = Prompt.handleKey[Msg](m.prompt, k)(toMsgFromPrompt)
            val withPrompt             = m.copy(prompt = nextPrompt)
            maybeCmd match
              case Some(Cmd.GCmd(submitMsg)) =>
                step(withPrompt, submitMsg)
              case Some(Cmd.Exit) => StepResult.ExitNow(withPrompt)
              case Some(_) | None => StepResult.StayInModel(withPrompt)

  /** `toMsg` shared by the runtime + by Prompt.handleKey inside `step`. */
  def toMsgFromPrompt(line: PromptLine): Result[Msg] =
    val text = line.value
    if text.trim.equalsIgnoreCase("exit") || text.trim.equalsIgnoreCase("quit") then Right(Msg.Quit)
    else Right(Msg.Submit(text))

  // ---- View ---------------------------------------------------------------

  private def renderRoot(m: Model): RootNode =
    given Theme = Theme.dark
    val w       = math.max(40, m.width)
    val h       = math.max(8, m.height)

    val title = TextNode(
      2.x,
      1.y,
      List(
        Text("TermFlow Streaming Chat", Style(fg = Theme.dark.primary, bold = true)),
        "  ".text,
        m.streaming match
          case Some(_) => Text("● streaming", Style(fg = Theme.dark.success))
          case None    => Text("○ idle", Style(fg = Theme.dark.secondary))
      )
    )

    val help = TextNode(
      2.x,
      2.y,
      List(
        " ↑/↓/wheel ".text(fg = Theme.dark.primary),
        "scroll  ".text,
        " End ".text(fg = Theme.dark.primary),
        "tail  ".text,
        " Ctrl+L ".text(fg = Theme.dark.primary),
        "clear  ".text,
        " Ctrl+C ".text(fg = Theme.dark.primary),
        "quit".text
      )
    )

    val tCap   = transcriptCapacity(m)
    val tWidth = transcriptWidth(m)
    val transcript = widgets.LogView(
      lines = transcriptLines(m),
      width = tWidth,
      height = tCap,
      scrollOffset = m.scrollOffset,
      at = transcriptOrigin,
      wrap = true
    )

    val statusRow = TextNode(
      2.x,
      (h - 1).y,
      List(
        Text(s" ${m.status} ", Style(fg = Theme.dark.background, bg = Theme.dark.primary)),
        if m.autoTail then "  (auto-tail)".text(fg = Theme.dark.secondary)
        else "  (paused — press End to tail)".text(fg = Theme.dark.warning)
      )
    )

    val rendered = Prompt.renderWithPrefix(m.prompt, "> ")

    RootNode(
      width = w,
      height = h,
      children = title :: help :: statusRow :: transcript,
      input = Some(
        InputNode(
          2.x,
          h.y,
          rendered.text,
          Style(fg = Theme.dark.success),
          cursor = rendered.cursorIndex,
          lineWidth = math.max(1, w - 2),
          prefixLength = rendered.prefixLength
        )
      )
    )

  // ---- App ---------------------------------------------------------------

  object App extends TuiApp[Model, Msg]:

    override def init(ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val keys = Sub.InputKey[Msg](
        msg = Msg.ConsoleInputKey.apply,
        onError = Msg.ConsoleInputError.apply,
        ctx = ctx
      )
      initialModel(ctx.terminal.width, ctx.terminal.height, keys).tui

    override def update(m: Model, msg: Msg, ctx: RuntimeCtx[Msg]): Tui[Model, Msg] =
      val sized = m.copy(width = ctx.terminal.width, height = ctx.terminal.height)
      step(sized, msg) match
        case StepResult.StayInModel(next)        => next.tui
        case StepResult.ExitNow(next)            => Tui(next, Cmd.Exit)
        case StepResult.StartStreaming(next, ms) =>
          // Cancel any pre-existing ticker, then register a fresh one.
          if next.ticker.isActive then next.ticker.cancel()
          val ticker = Sub.Every(ms, () => Msg.TokenTick, ctx)
          next.copy(ticker = ticker).tui

    override def view(m: Model): RootNode = renderRoot(m)

    override def toMsg(input: PromptLine): Result[Msg] = toMsgFromPrompt(input)

  def main(args: Array[String]): Unit =
    val _ = args
    TuiRuntime.run(App)
