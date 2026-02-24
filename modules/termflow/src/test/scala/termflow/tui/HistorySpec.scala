package termflow.tui

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.KeyDecoder.InputKey
import termflow.tui.TuiPrelude._

class HistorySpec extends AnyFunSuite {

  final private class StubStore(initial: Vector[String] = Vector.empty) extends HistoryStore {
    private val buf                 = scala.collection.mutable.ArrayBuffer.from(initial)
    def load(): Vector[String]      = buf.toVector
    def append(entry: String): Unit = buf += entry
    def contents: Vector[String]    = buf.toVector
  }

  private def renderLine(state: PromptHistory.State): String =
    Prompt.render(state.prompt)

  private def toMsg(line: String): Result[String] = Right(line)

  private def step(
    state: PromptHistory.State,
    key: InputKey
  ): (PromptHistory.State, Option[Cmd[String]]) =
    PromptHistory.handleKey[String](state, key)(toMsg)

  test("ArrowUp navigates backward through history and ArrowDown restores current line") {
    val store = new StubStore(Vector("one", "two"))
    val start = PromptHistory.initial(store)

    val (afterUp1, _) = step(start, InputKey.ArrowUp)
    assert(renderLine(afterUp1) == "two")

    val (afterUp2, _) = step(afterUp1, InputKey.ArrowUp)
    assert(renderLine(afterUp2) == "one")

    val (afterDown, _) = step(afterUp2, InputKey.ArrowDown)
    assert(renderLine(afterDown) == "two")

    val (restoreCurrent, _) = step(afterDown, InputKey.ArrowDown)
    assert(renderLine(restoreCurrent) == "")
  }

  test("Enter appends to history and emits a GCmd") {
    val store = new StubStore()
    val start = PromptHistory.initial(store)

    val (withChar, _)        = step(start, InputKey.CharKey('x'))
    val (afterEnter, cmdOpt) = step(withChar, InputKey.Enter)

    assert(cmdOpt.contains(Cmd.GCmd("x")))
    assert(renderLine(afterEnter) == "")
    assert(store.contents.contains("x"))
  }

  test("Prefix search uses typed prefix when navigating with ArrowUp") {
    val store = new StubStore(Vector("foo", "bar", "fizz"))
    val start = PromptHistory.initial(store)

    val (typedF, _)   = step(start, InputKey.CharKey('f'))
    val (afterUp1, _) = step(typedF, InputKey.ArrowUp)
    assert(renderLine(afterUp1) == "fizz") // last matching prefix f

    val (afterUp2, _) = step(afterUp1, InputKey.ArrowUp)
    assert(renderLine(afterUp2) == "foo") // next match with same prefix

    val (afterDown, _) = step(afterUp2, InputKey.ArrowDown)
    assert(renderLine(afterDown) == "fizz") // back down within prefix matches
  }

  test("ArrowUp on empty history is a no-op") {
    val store        = new StubStore(Vector.empty)
    val start        = PromptHistory.initial(store)
    val (after, cmd) = step(start, InputKey.ArrowUp)
    assert(cmd.isEmpty)
    assert(renderLine(after).isEmpty)
    assert(after.index.isEmpty)
  }

  test("Enter with empty line does not append to history") {
    val store       = new StubStore()
    val start       = PromptHistory.initial(store)
    val (_, cmdOpt) = step(start, InputKey.Enter)
    assert(cmdOpt.contains(Cmd.GCmd("")))
    assert(store.contents.isEmpty)
  }

  test("FileHistoryStore respects maxEntries and round-trips") {
    import java.nio.file.Files
    val tmp   = Files.createTempDirectory("history-test")
    val path  = tmp.resolve("hist.log")
    val store = FileHistoryStore(path, maxEntries = 2)
    store.append("one")
    store.append("two")
    store.append("three")
    assert(store.load() == Vector("two", "three"))
    Files.deleteIfExists(path)
    Files.deleteIfExists(tmp)
  }

  test("FileHistoryStore creates parent directories when missing") {
    import java.nio.file.Files
    val tmp    = Files.createTempDirectory("history-missing-parent")
    val parent = tmp.resolve("nested")
    val path   = parent.resolve("hist.log")
    val store  = FileHistoryStore(path, maxEntries = 10)

    assert(!Files.exists(parent))
    store.append("entry")

    assert(Files.exists(parent))
    assert(store.load().contains("entry"))

    Files.deleteIfExists(path)
    Files.deleteIfExists(parent)
    Files.deleteIfExists(tmp)
  }
}
