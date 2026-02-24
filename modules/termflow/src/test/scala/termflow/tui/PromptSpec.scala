package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.TuiPrelude._

class PromptSpec extends AnyFunSuite:

  private def noopToMsg(line: PromptLine): Result[String] =
    Right(line.value)

  test("CharKey inserts and moves cursor right"):
    val (next, cmd) = Prompt.handleKey[String](Prompt.State(), InputKey.CharKey('a'))(noopToMsg)
    assert(cmd.isEmpty)
    assert(Prompt.render(next) == "a")
    assert(next.cursor == 1)

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
