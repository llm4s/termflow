package termflow.apps.chat

import org.scalatest.funsuite.AnyFunSuite
import termflow.testkit.KeySim
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.Sub

class ChatStreamAppSpec extends AnyFunSuite:

  private def freshModel: ChatStreamApp.Model =
    ChatStreamApp.initialModel(width = 80, height = 24, input = Sub.NoSub)

  private def stepKey(m: ChatStreamApp.Model, key: InputKey): ChatStreamApp.Model =
    ChatStreamApp.step(m, ChatStreamApp.Msg.ConsoleInputKey(key)) match
      case ChatStreamApp.StepResult.StayInModel(next)       => next
      case ChatStreamApp.StepResult.ExitNow(next)           => next
      case ChatStreamApp.StepResult.StartStreaming(next, _) => next

  private def submit(m: ChatStreamApp.Model, text: String): ChatStreamApp.Model =
    ChatStreamApp.step(m, ChatStreamApp.Msg.Submit(text)) match
      case ChatStreamApp.StepResult.StayInModel(next)       => next
      case ChatStreamApp.StepResult.ExitNow(next)           => next
      case ChatStreamApp.StepResult.StartStreaming(next, _) => next

  private def tick(m: ChatStreamApp.Model): ChatStreamApp.Model =
    ChatStreamApp.step(m, ChatStreamApp.Msg.TokenTick) match
      case ChatStreamApp.StepResult.StayInModel(next)       => next
      case ChatStreamApp.StepResult.ExitNow(next)           => next
      case ChatStreamApp.StepResult.StartStreaming(next, _) => next

  // ---- Initial state ------------------------------------------------------

  test("initial model contains the welcome entries and is auto-tailing"):
    val m = freshModel
    assert(m.entries == ChatStreamApp.WelcomeEntries)
    assert(m.autoTail)
    assert(m.streaming.isEmpty)
    assert(m.status == "ready")

  // ---- Submit + streaming -------------------------------------------------

  test("Submit appends a User entry and an empty Assistant entry, and starts streaming"):
    val m = freshModel
    val s = submit(m, "ping")
    assert(s.entries.size == m.entries.size + 2)
    assert(s.entries(s.entries.size - 2).role == ChatStreamApp.Role.User)
    assert(s.entries(s.entries.size - 2).content == "ping")
    val asst = s.entries.last
    assert(asst.role == ChatStreamApp.Role.Assistant)
    assert(asst.content.isEmpty)
    assert(s.streaming.exists(_.remaining == ChatStreamApp.responseFor("ping")))
    assert(s.status == "streaming…")

  test("StartStreaming is the StepResult for a Submit"):
    val m   = freshModel
    val res = ChatStreamApp.step(m, ChatStreamApp.Msg.Submit("hi"))
    res match
      case ChatStreamApp.StepResult.StartStreaming(_, ms) =>
        assert(ms == 40L)
      case other => fail(s"expected StartStreaming; got $other")

  test("TokenTick consumes one character per tick and accumulates into the assistant entry"):
    val m        = freshModel
    val s        = submit(m, "ping")
    val expected = ChatStreamApp.responseFor("ping")
    val s1       = tick(s)
    assert(s1.entries.last.content == expected.take(1))
    val s2 = tick(s1)
    assert(s2.entries.last.content == expected.take(2))
    val s3 = tick(s2)
    assert(s3.entries.last.content == expected.take(3))

  test("TokenTick continues until the response is fully delivered, then ends streaming"):
    val m        = freshModel
    val s        = submit(m, "ping")
    val response = ChatStreamApp.responseFor("ping")
    val finalM   = (1 to response.length + 1).foldLeft(s)((acc, _) => tick(acc))
    assert(finalM.entries.last.content == response)
    assert(finalM.streaming.isEmpty, "streaming should clear once tokens exhausted")
    assert(finalM.status == "ready")

  test("Empty submit is a no-op (no new entries, no streaming)"):
    val m = freshModel
    val s = submit(m, "   ")
    assert(s.entries == m.entries)
    assert(s.streaming.isEmpty)

  // ---- Scrollback behaviour ----------------------------------------------

  test("ArrowUp scroll past the auto-tail line disables auto-tail"):
    // Generate enough content to make the transcript scrollable.
    val m    = freshModel
    val full = (1 to 20).foldLeft(m)((acc, _) => submit(acc, "long"))
    // Drain all streaming so transcript is final.
    val drained = (1 to 1000).foldLeft(full)((acc, _) => tick(acc))
    assert(drained.streaming.isEmpty)
    assert(drained.autoTail)
    val scrolled = stepKey(drained, KeySim.ArrowUp)
    assert(!scrolled.autoTail)
    assert(scrolled.scrollOffset < drained.scrollOffset)

  test("End re-enables auto-tail"):
    val m       = freshModel
    val full    = (1 to 8).foldLeft(m)((acc, _) => submit(acc, "long"))
    val drained = (1 to 1000).foldLeft(full)((acc, _) => tick(acc))
    val paused  = stepKey(drained, KeySim.ArrowUp)
    assert(!paused.autoTail)
    val tailed = stepKey(paused, KeySim.End)
    assert(tailed.autoTail)
    assert(tailed.scrollOffset == drained.scrollOffset)

  test("mouse-wheel up over the transcript scrolls back and disables auto-tail"):
    val m       = freshModel
    val full    = (1 to 20).foldLeft(m)((acc, _) => submit(acc, "long"))
    val drained = (1 to 1000).foldLeft(full)((acc, _) => tick(acc))
    assert(drained.autoTail)
    // Origin is (col=2, row=4); scroll inside the transcript pane.
    val wheelEvents = termflow.testkit.MouseSim.scrollUp(col = 5, row = 8, ticks = 1)
    val scrolled    = wheelEvents.foldLeft(drained)((acc, ev) => stepKey(acc, ev))
    assert(!scrolled.autoTail)
    assert(scrolled.scrollOffset < drained.scrollOffset)

  test("mouse-wheel scroll outside the transcript pane is a no-op"):
    val m       = freshModel
    val full    = (1 to 20).foldLeft(m)((acc, _) => submit(acc, "long"))
    val drained = (1 to 1000).foldLeft(full)((acc, _) => tick(acc))
    // Wheel over the prompt row (last line) — should not affect scroll state.
    val outsideEvents = termflow.testkit.MouseSim.scrollUp(col = 5, row = drained.height, ticks = 3)
    val unchanged     = outsideEvents.foldLeft(drained)((acc, ev) => stepKey(acc, ev))
    assert(unchanged.autoTail)
    assert(unchanged.scrollOffset == drained.scrollOffset)

  // ---- Clear / Quit -------------------------------------------------------

  test("Ctrl+L clears the transcript back to the welcome entries"):
    val m       = freshModel
    val full    = (1 to 5).foldLeft(m)((acc, _) => submit(acc, "x"))
    val cleared = stepKey(full, KeySim.ctrl('L'))
    assert(cleared.entries == ChatStreamApp.WelcomeEntries)
    assert(cleared.streaming.isEmpty)
    assert(cleared.status == "cleared")

  test("Ctrl+C produces an ExitNow"):
    val m   = freshModel
    val res = ChatStreamApp.step(m, ChatStreamApp.Msg.ConsoleInputKey(KeySim.ctrl('C')))
    res match
      case ChatStreamApp.StepResult.ExitNow(_) => succeed
      case other                               => fail(s"expected ExitNow; got $other")

  test("Esc produces an ExitNow"):
    val m   = freshModel
    val res = ChatStreamApp.step(m, ChatStreamApp.Msg.ConsoleInputKey(KeySim.Escape))
    res match
      case ChatStreamApp.StepResult.ExitNow(_) => succeed
      case other                               => fail(s"expected ExitNow; got $other")

  test("toMsg routes 'quit' / 'exit' to Msg.Quit and other text to Submit"):
    import termflow.tui.TuiPrelude.PromptLine
    assert(ChatStreamApp.toMsgFromPrompt(PromptLine("quit")) == Right(ChatStreamApp.Msg.Quit))
    assert(ChatStreamApp.toMsgFromPrompt(PromptLine("exit")) == Right(ChatStreamApp.Msg.Quit))
    ChatStreamApp.toMsgFromPrompt(PromptLine("hello")) match
      case Right(ChatStreamApp.Msg.Submit("hello")) => succeed
      case other                                    => fail(s"expected Submit('hello'); got $other")
