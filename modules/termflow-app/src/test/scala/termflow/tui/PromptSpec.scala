package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.TuiPrelude.*

class PromptSpec extends AnyFunSuite:

  private def noopToMsg(line: PromptLine): Result[String] =
    Right(line.value)

  test("CharKey inserts and moves cursor right"):
    val (next, cmd) = Prompt.handleKey[String](Prompt.State(), InputKey.CharKey('a'))(noopToMsg)
    assert(cmd.isEmpty)
    assert(Prompt.render(next) == "a")
    assert(next.cursor == 1)

  test("Supplementary inserts a surrogate pair and advances by two"):
    val (next, cmd) = Prompt.handleKey[String](Prompt.State(), InputKey.Supplementary(0x1f600))(noopToMsg)
    assert(cmd.isEmpty)
    assert(Prompt.render(next) == "😀")
    assert(next.cursor == 2)

  test("Backspace removes the previous character when cursor > 0"):
    val state       = Prompt.State(buffer = Vector('a', 'b'), cursor = 2)
    val (next, cmd) = Prompt.handleKey[String](state, InputKey.Backspace)(noopToMsg)
    assert(cmd.isEmpty)
    assert(Prompt.render(next) == "a")
    assert(next.cursor == 1)

  test("Backspace at start is a no-op"):
    val state       = Prompt.State(buffer = Vector('a'), cursor = 0)
    val (next, cmd) = Prompt.handleKey[String](state, InputKey.Backspace)(noopToMsg)
    assert(cmd.isEmpty)
    assert(Prompt.render(next) == "a")
    assert(next.cursor == 0)

  test("Delete removes the character at the cursor"):
    val state       = Prompt.State(buffer = Vector('a', 'b', 'c'), cursor = 1)
    val (next, cmd) = Prompt.handleKey[String](state, InputKey.Delete)(noopToMsg)
    assert(cmd.isEmpty)
    assert(Prompt.render(next) == "ac")
    assert(next.cursor == 1)

  test("Delete at end is a no-op"):
    val state       = Prompt.State(buffer = Vector('a'), cursor = 1)
    val (next, cmd) = Prompt.handleKey[String](state, InputKey.Delete)(noopToMsg)
    assert(cmd.isEmpty)
    assert(Prompt.render(next) == "a")
    assert(next.cursor == 1)

  test("Insert in the middle keeps suffix intact"):
    val state       = Prompt.State(buffer = Vector('a', 'c'), cursor = 1)
    val (next, cmd) = Prompt.handleKey[String](state, InputKey.CharKey('b'))(noopToMsg)
    assert(cmd.isEmpty)
    assert(Prompt.render(next) == "abc")
    assert(next.cursor == 2)

  test("ArrowLeft and ArrowRight clamp cursor within bounds"):
    val start     = Prompt.State(buffer = Vector('a', 'b'), cursor = 0)
    val (left, _) = Prompt.handleKey[String](start, InputKey.ArrowLeft)(noopToMsg)
    assert(left.cursor == 0) // cannot go past start

    val (right, _) = Prompt.handleKey[String](left.copy(cursor = 2), InputKey.ArrowRight)(noopToMsg)
    assert(right.cursor == 2) // cannot go past end

  test("Home and End move cursor to start and end"):
    val state       = Prompt.State(buffer = Vector('a', 'b', 'c'), cursor = 1)
    val (atHome, _) = Prompt.handleKey[String](state, InputKey.Home)(noopToMsg)
    val (atEnd, _)  = Prompt.handleKey[String](state, InputKey.End)(noopToMsg)
    assert(atHome.cursor == 0)
    assert(atEnd.cursor == 3)

  test("Enter emits GCmd on success and clears the buffer"):
    val state          = Prompt.State(buffer = Vector('h', 'i'), cursor = 2)
    val (next, cmdOpt) = Prompt.handleKey[String](state, InputKey.Enter)(_ => Right("ok"))
    assert(Prompt.render(next).isEmpty)
    assert(next.cursor == 0)
    assert(cmdOpt.contains(Cmd.GCmd("ok")))

  test("Enter emits TermFlowErrorCmd on failure and clears the buffer"):
    val state = Prompt.State(buffer = Vector('h', 'i'), cursor = 2)
    val (next, cmdOpt) =
      Prompt.handleKey[String](state, InputKey.Enter)(_ => Left(TermFlowError.CommandError("bad")))
    assert(Prompt.render(next).isEmpty)
    assert(next.cursor == 0)
    assert(cmdOpt.contains(Cmd.TermFlowErrorCmd(TermFlowError.CommandError("bad"))))

  test("Ctrl+C exits and clears the buffer"):
    val state          = Prompt.State(buffer = Vector('x'), cursor = 1)
    val (next, cmdOpt) = Prompt.handleKey[String](state, InputKey.Ctrl('C'))(noopToMsg)
    assert(Prompt.render(next).isEmpty)
    assert(next.cursor == 0)
    assert(cmdOpt.contains(Cmd.Exit))

  test("renderWithPrefix shifts cursor index by prefix length"):
    val line = Prompt.renderWithPrefix(Prompt.State(buffer = Vector('a', 'b'), cursor = 1), ">> ")
    assert(line.text == ">> ab")
    assert(line.cursorIndex == 4)
    assert(line.prefixLength == 3)

  // ---- grapheme-aware navigation ------------------------------------------

  test("Backspace deletes a surrogate pair as one unit"):
    // 😀 = U+1F600, encoded as a high+low surrogate (length = 2).
    val emoji     = "a😀"
    val state     = Prompt.State(buffer = emoji.toVector, cursor = emoji.length)
    val (next, _) = Prompt.handleKey[String](state, InputKey.Backspace)(noopToMsg)
    assert(Prompt.render(next) == "a")
    assert(next.cursor == 1)

  test("Backspace deletes a combining mark with its base"):
    // "é" = e + COMBINING ACUTE ACCENT (one grapheme, two chars).
    val s         = "é"
    val state     = Prompt.State(buffer = s.toVector, cursor = s.length)
    val (next, _) = Prompt.handleKey[String](state, InputKey.Backspace)(noopToMsg)
    assert(Prompt.render(next).isEmpty)
    assert(next.cursor == 0)

  test("ArrowLeft steps over a surrogate pair"):
    val emoji     = "a😀b"
    val state     = Prompt.State(buffer = emoji.toVector, cursor = 3) // after the emoji
    val (next, _) = Prompt.handleKey[String](state, InputKey.ArrowLeft)(noopToMsg)
    assert(next.cursor == 1, s"expected cursor=1 after stepping over 😀, got ${next.cursor}")

  test("ArrowRight steps over a surrogate pair"):
    val emoji     = "a😀b"
    val state     = Prompt.State(buffer = emoji.toVector, cursor = 1) // after a
    val (next, _) = Prompt.handleKey[String](state, InputKey.ArrowRight)(noopToMsg)
    assert(next.cursor == 3)

  test("Delete removes a surrogate pair"):
    val emoji     = "a😀b"
    val state     = Prompt.State(buffer = emoji.toVector, cursor = 1)
    val (next, _) = Prompt.handleKey[String](state, InputKey.Delete)(noopToMsg)
    assert(Prompt.render(next) == "ab")
    assert(next.cursor == 1)

  test("cursorColumn counts wide characters as 2 columns"):
    // 你好 = two CJK chars, each width 2.
    val s     = "你好"
    val state = Prompt.State(buffer = s.toVector, cursor = 1) // after 你
    assert(Prompt.cursorColumn(state) == 2)
    val state2 = state.copy(cursor = 2) // after 好
    assert(Prompt.cursorColumn(state2) == 4)

  test("cursorColumn counts combining marks as 0 columns"):
    val state = Prompt.State(buffer = "é".toVector, cursor = 2)
    assert(Prompt.cursorColumn(state) == 1, "e=1, combining mark=0")

  // ---- bracketed paste ----------------------------------------------------

  test("Paste inserts text at the cursor and advances by its length"):
    val state       = Prompt.State(buffer = Vector('a', 'd'), cursor = 1)
    val (next, cmd) = Prompt.handleKey[String](state, InputKey.Paste("bc"))(noopToMsg)
    assert(cmd.isEmpty)
    assert(Prompt.render(next) == "abcd")
    assert(next.cursor == 3)

  test("Paste keeps only the first line of a multi-line payload"):
    val (next, _) = Prompt.handleKey[String](Prompt.State(), InputKey.Paste("one\ntwo\r\nthree"))(noopToMsg)
    assert(Prompt.render(next) == "one")
    assert(next.cursor == 3)

  test("Paste of empty text is a no-op"):
    val state     = Prompt.State(buffer = Vector('a'), cursor = 1)
    val (next, _) = Prompt.handleKey[String](state, InputKey.Paste(""))(noopToMsg)
    assert(Prompt.render(next) == "a")
    assert(next.cursor == 1)
