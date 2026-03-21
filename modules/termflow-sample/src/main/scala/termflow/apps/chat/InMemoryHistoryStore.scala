package termflow.apps.chat

import termflow.tui.HistoryStore

final private case class InMemoryHistoryStore(
  initialEntries: Vector[String] = Vector.empty,
  maxEntries: Int = 200
) extends HistoryStore:

  private val entries = scala.collection.mutable.ArrayBuffer.from(initialEntries.takeRight(maxEntries))

  override def load(): Vector[String] =
    entries.toVector

  override def append(entry: String): Unit =
    val trimmed = entry.trim
    if trimmed.nonEmpty then
      entries += trimmed
      if entries.length > maxEntries then entries.remove(0, entries.length - maxEntries)
