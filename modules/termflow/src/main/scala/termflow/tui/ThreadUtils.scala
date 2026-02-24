package termflow.tui

import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory

/** Thread utilities that provide virtual threads on Java 21+ with fallback to platform threads. */
object ThreadUtils {

  private val useVirtualThreads: Boolean =
    try {
      // Check if virtual threads are available (Java 21+)
      classOf[Thread].getMethod("ofVirtual")
      true
    } catch {
      case _: NoSuchMethodException => false
    }

  /** Returns a ThreadFactory that creates virtual threads on Java 21+, platform threads otherwise. */
  def newThreadFactory(): ThreadFactory =
    if (useVirtualThreads) {
      Thread.ofVirtual().factory()
    } else {
      Executors.defaultThreadFactory()
    }

  /** Starts a new thread (virtual on Java 21+, platform otherwise) and returns it. */
  def startThread(runnable: Runnable): Thread =
    if (useVirtualThreads) {
      Thread.ofVirtual().start(runnable)
    } else {
      val t = new Thread(runnable)
      t.start()
      t
    }
}
