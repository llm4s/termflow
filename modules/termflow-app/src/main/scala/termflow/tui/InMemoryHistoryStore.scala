package termflow.tui

final case class InMemoryHistoryStore(
  initialEntries: Vector[String] = Vector.empty,
  maxEntries: Int = 200
) extends HistoryStore:

  private val entries = scala.collection.mutable.ArrayBuffer.from(initialEntries.takeRight(maxEntries))

  // `ArrayBuffer` is not thread-safe. The runtime drives update on its loop
  // thread while input is decoded on a separate thread, so guard every access
  // with a lock — otherwise a concurrent append vs. load can throw
  // ArrayIndexOutOfBounds / ConcurrentModification or corrupt the buffer.
  private val lock = new Object

  override def load(): Vector[String] =
    lock.synchronized(entries.toVector)

  override def append(entry: String): Unit =
    val trimmed = entry.trim
    if trimmed.nonEmpty then
      lock.synchronized {
        entries += trimmed
        if entries.length > maxEntries then entries.remove(0, entries.length - maxEntries)
      }
