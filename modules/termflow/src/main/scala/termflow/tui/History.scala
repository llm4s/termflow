package termflow.tui

import termflow.tui.KeyDecoder.InputKey
import termflow.tui.TuiPrelude._

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path, StandardOpenOption }
import scala.jdk.CollectionConverters._

/** Abstraction for loading and persisting command history. */
trait HistoryStore {
  def load(): Vector[String]
  def append(entry: String): Unit
}

/** Simple file-backed implementation of HistoryStore. */
final case class FileHistoryStore(path: Path, maxEntries: Int = 200) extends HistoryStore {

  private def ensureDir(): Unit = {
    val parent = path.getParent
    if (parent != null && !Files.exists(parent)) {
      Files.createDirectories(parent): Unit
    }
  }

  override def load(): Vector[String] =
    try {
      ensureDir()
      if (Files.exists(path)) {
        val all = Files.readAllLines(path).asScala.toVector
        // Keep the most recent entries, preserving full chronological history.
        all.takeRight(maxEntries)
      } else Vector.empty
    } catch {
      case _: Throwable => Vector.empty
    }

  override def append(entry: String): Unit =
    if (entry.trim.nonEmpty) {
      try {
        ensureDir()
        val line = entry + System.lineSeparator()
        Files.write(
          path,
          line.getBytes(StandardCharsets.UTF_8),
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        ): Unit
      } catch {
        case _: Throwable => ()
      }
    }
}

/** Prompt + history wrapper with ArrowUp/ArrowDown support. */
object PromptHistory {

  final case class State(
    prompt: Prompt.State,
    history: Vector[String],
    index: Option[Int],
    store: HistoryStore,
    savedCurrent: Option[String],
    searchPrefix: Option[String]
  )

  def initial(store: HistoryStore): State =
    State(
      prompt = Prompt.State(),
      history = store.load(),
      index = None,
      store = store,
      savedCurrent = None,
      searchPrefix = None
    )

  def render(state: State): String =
    Prompt.render(state.prompt)

  def renderWithPrefix(state: State, prefix: String): Prompt.RenderedLine =
    Prompt.renderWithPrefix(state.prompt, prefix)

  def handleKey[G](state: State, k: InputKey)(toMsg: String => Result[G]): (State, Option[Cmd[G]]) =
    k match {
      // History navigation
      case KeyDecoder.InputKey.ArrowUp =>
        (historyUp(state), None)

      case KeyDecoder.InputKey.ArrowDown =>
        (historyDown(state), None)

      // Clean exit via Ctrl+C / Ctrl+D (do not record in history)
      case KeyDecoder.InputKey.Ctrl('C') | KeyDecoder.InputKey.Ctrl('D') =>
        val cleared = state.copy(
          prompt = Prompt.State(),
          index = None,
          savedCurrent = None,
          searchPrefix = None
        )
        (cleared, Some(Cmd.Exit))

      // Enter: commit line to history and parse
      case KeyDecoder.InputKey.Ctrl('M') | KeyDecoder.InputKey.Enter =>
        val line                     = Prompt.render(state.prompt)
        val (nextHistory, nextStore) = addToHistory(state.history, state.store, line)
        val result                   = toMsg(line)
        val cmd                      = result.fold(err => Cmd.TermFlowErrorCmd(err), g => Cmd.GCmd(g))
        val nextState =
          state.copy(
            prompt = Prompt.State(),
            history = nextHistory,
            index = None,
            store = nextStore,
            savedCurrent = None,
            searchPrefix = None
          )
        (nextState, Some(cmd))

      // All other keys delegate to the underlying Prompt editor
      case other =>
        val (nextPrompt, maybeCmd) = Prompt.handleKey[G](state.prompt, other)(toMsg)
        val nextState =
          state.copy(
            prompt = nextPrompt,
            index = None,
            savedCurrent = None,
            searchPrefix = None
          )
        (nextState, maybeCmd)
    }

  private def addToHistory(
    history: Vector[String],
    store: HistoryStore,
    line: String
  ): (Vector[String], HistoryStore) = {
    val trimmed = line.trim
    if (trimmed.isEmpty) (history, store)
    else if (history.lastOption.contains(trimmed)) (history, store)
    else {
      store.append(trimmed)
      (history :+ trimmed, store)
    }
  }

  private def historyUp(state: State): State =
    if (state.history.isEmpty) state
    else {
      val currentLine = Prompt.render(state.prompt)

      state.searchPrefix match {
        // Active prefix search: keep using the stored prefix, independent of current line content.
        case Some(prefix) =>
          val startFromIdx = state.index.map(_ - 1).getOrElse(state.history.length - 1)
          val nextIdxOpt   = searchUp(state.history, startFromIdx, prefix)
          nextIdxOpt match {
            case None => state
            case Some(newIndex) =>
              val line      = state.history(newIndex)
              val newPrompt = Prompt.State(buffer = line.toVector, cursor = line.length)
              state.copy(
                prompt = newPrompt,
                index = Some(newIndex)
              )
          }

        // No active prefix search: either start one (if at index=None and prefix typed), or do plain scrolling.
        case None =>
          state.index match {
            case None =>
              val typedPrefix = currentLine.take(state.prompt.cursor).trim
              if (typedPrefix.nonEmpty) {
                // Start a prefix-based search from the end.
                val startFromIdx = state.history.length - 1
                val nextIdxOpt   = searchUp(state.history, startFromIdx, typedPrefix)
                nextIdxOpt match {
                  case None => state
                  case Some(newIndex) =>
                    val line      = state.history(newIndex)
                    val newPrompt = Prompt.State(buffer = line.toVector, cursor = line.length)
                    state.copy(
                      prompt = newPrompt,
                      index = Some(newIndex),
                      savedCurrent = Some(currentLine),
                      searchPrefix = Some(typedPrefix)
                    )
                }
              } else {
                // First plain history step: jump to last entry.
                val newIndex  = state.history.length - 1
                val line      = state.history(newIndex)
                val newPrompt = Prompt.State(buffer = line.toVector, cursor = line.length)
                state.copy(
                  prompt = newPrompt,
                  index = Some(newIndex),
                  savedCurrent = Some(currentLine)
                )
              }

            case Some(i) =>
              // Already in plain history mode: move further up, ignoring any current line content.
              val newIndex  = math.max(0, i - 1)
              val line      = state.history(newIndex)
              val newPrompt = Prompt.State(buffer = line.toVector, cursor = line.length)
              state.copy(
                prompt = newPrompt,
                index = Some(newIndex)
              )
          }
      }
    }

  private def historyDown(state: State): State =
    state.index match {
      case None => state
      case Some(currentIdx) =>
        state.searchPrefix match {
          // Prefix search scrolling when a prefix is active.
          case Some(prefix) =>
            val startFromIdx = currentIdx + 1
            val nextIdxOpt   = searchDown(state.history, startFromIdx, prefix)

            nextIdxOpt match {
              case Some(newIndex) =>
                val line      = state.history(newIndex)
                val newPrompt = Prompt.State(buffer = line.toVector, cursor = line.length)
                state.copy(
                  prompt = newPrompt,
                  index = Some(newIndex)
                )
              case None =>
                // No further match; restore the original line if we have it, otherwise clear.
                val restoredLine   = state.savedCurrent.getOrElse("")
                val restoredPrompt = Prompt.State(buffer = restoredLine.toVector, cursor = restoredLine.length)
                state.copy(
                  prompt = restoredPrompt,
                  index = None,
                  savedCurrent = None,
                  searchPrefix = None
                )
            }

          // Plain scrolling when no prefix search is active.
          case None =>
            if (currentIdx >= state.history.length - 1) {
              // Move past the most recent entry: restore original line (if any) and exit history mode.
              val restoredLine   = state.savedCurrent.getOrElse("")
              val restoredPrompt = Prompt.State(buffer = restoredLine.toVector, cursor = restoredLine.length)
              state.copy(
                prompt = restoredPrompt,
                index = None,
                savedCurrent = None
              )
            } else {
              val newIndex  = currentIdx + 1
              val line      = state.history(newIndex)
              val newPrompt = Prompt.State(buffer = line.toVector, cursor = line.length)
              state.copy(
                prompt = newPrompt,
                index = Some(newIndex)
              )
            }
        }
    }

  private def searchUp(history: Vector[String], startFrom: Int, prefix: String): Option[Int] = {
    @annotation.tailrec
    def loop(i: Int): Option[Int] =
      if (i < 0) None
      else if (history(i).startsWith(prefix)) Some(i)
      else loop(i - 1)
    loop(startFrom)
  }

  private def searchDown(history: Vector[String], startFrom: Int, prefix: String): Option[Int] = {
    @annotation.tailrec
    def loop(i: Int): Option[Int] =
      if (i >= history.length) None
      else if (history(i).startsWith(prefix)) Some(i)
      else loop(i + 1)
    loop(startFrom)
  }
}
