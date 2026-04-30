package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files

class FrameworkLogSpec extends AnyFunSuite:

  private def withLog[A](f: (FrameworkLog, java.nio.file.Path) => A): A =
    val tmp = Files.createTempFile("framework-log", ".log")
    f(FrameworkLog(LoggingConfig(LogPath(tmp))), tmp)

  test("info / warn / error all append a line tagged with the level"):
    withLog { (log, path) =>
      assert(log.info("hello-info").isSuccess)
      assert(log.warn("hello-warn").isSuccess)
      assert(log.error("hello-error").isSuccess)
      val text = Files.readString(path)
      assert(text.contains("[INFO] hello-info"))
      assert(text.contains("[WARN] hello-warn"))
      assert(text.contains("[ERROR] hello-error"))
    }

  test("LogPath.appendUtf8Line creates the parent directory if missing"):
    val tmpDir = Files.createTempDirectory("framework-log-parent")
    val nested = tmpDir.resolve("a/b/c.log")
    val log    = FrameworkLog(LoggingConfig(LogPath(nested)))
    assert(log.info("nested").isSuccess)
    assert(Files.exists(nested))
    val text = Files.readString(nested)
    assert(text.contains("[INFO] nested"))

  test("LogPath.path round-trips back to the underlying Path"):
    val tmp = Files.createTempFile("logpath", ".log")
    val lp  = LogPath(tmp)
    assert(lp.path == tmp)
