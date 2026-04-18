package termflow.testkit

import org.scalatest.Assertions
import org.scalatest.Suite
import termflow.tui.AnsiRenderer.RenderFrame

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

/**
 * Mix-in for `Suite`s that want to assert rendered frames against on-disk
 * golden files.
 *
 * Usage:
 * {{{
 * class MyAppSnapshotSpec extends AnyFunSuite with GoldenSupport:
 *   test("initial frame"):
 *     val driver = TuiTestDriver(MyApp.App, width = 40, height = 10)
 *     driver.init()
 *     assertGoldenFrame(driver.frame, "initial")
 * }}}
 *
 * Goldens live under `src/test/resources/$goldenDir/<SuiteSimpleName>/<name>.golden`
 * relative to the module's base directory. Because sbt runs tests with
 * `Test / fork := true` and a per-module working directory, plain relative
 * paths resolve correctly.
 *
 * To refresh a golden after an intentional change:
 * {{{
 *   sbt -Dtermflow.update-goldens=true test
 * }}}
 * or set `UPDATE_GOLDENS=1` in the environment. Always review the resulting
 * diff in git before committing.
 */
trait GoldenSupport extends Assertions:
  self: Suite =>

  /** Root directory (relative to module) where golden files are stored. */
  protected def goldenDir: String = "src/test/resources/termflow/golden"

  /** Subdirectory name under `goldenDir`. Defaults to the suite's simple class name. */
  protected def goldenSuiteName: String =
    val sn = getClass.getSimpleName
    if sn.endsWith("$") then sn.dropRight(1) else sn

  /**
   * Whether to write goldens instead of reading. Overridable so tests of
   * `GoldenSupport` itself can force-enable without touching system props.
   */
  protected def updateMode: Boolean =
    sys.props.get("termflow.update-goldens").contains("true") ||
      sys.env.get("UPDATE_GOLDENS").exists(v => v == "1" || v.equalsIgnoreCase("true"))

  /** Absolute-ish path to a golden file for the given snapshot name. */
  protected def goldenPath(name: String): Path =
    Path.of(goldenDir, goldenSuiteName, s"$name.golden")

  /**
   * Assert that `frame`, when serialized, matches the stored golden file.
   *
   * In update mode (`-Dtermflow.update-goldens=true` or `UPDATE_GOLDENS=1`)
   * the golden is (re)written and the assertion passes. Otherwise:
   *   - missing file → fail with guidance to enable update mode
   *   - content mismatch → fail with a line-level diff
   */
  protected def assertGoldenFrame(frame: RenderFrame, name: String): Unit =
    val serialized = GoldenFrame.serialize(frame)
    val path       = goldenPath(name)

    if updateMode then writeGolden(path, serialized)
    else
      val file = path.toFile
      if !file.exists() then
        fail(
          s"golden file not found: ${path.toString}\n" +
            s"record it with: sbt -Dtermflow.update-goldens=true test"
        )
      else
        val expected = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
        if expected != serialized then
          val detail = GoldenFrame.describeDiff(expected, serialized)
          fail(s"$detail\npath: ${path.toString}")

  /** Assert a raw string snapshot. Useful for single-row prompt assertions. */
  protected def assertGoldenString(content: String, name: String): Unit =
    val path = goldenPath(name)
    if updateMode then writeGolden(path, content)
    else
      val file = path.toFile
      if !file.exists() then
        fail(
          s"golden file not found: ${path.toString}\n" +
            s"record it with: sbt -Dtermflow.update-goldens=true test"
        )
      else
        val expected = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
        if expected != content then
          val detail = GoldenFrame.describeDiff(expected, content)
          fail(s"$detail\npath: ${path.toString}")

  private def writeGolden(path: Path, content: String): Unit =
    val parent = path.getParent
    if parent != null then
      val parentFile = parent.toFile
      if !parentFile.exists() then {
        val _ = parentFile.mkdirs()
      }
    val _ = Files.write(path, content.getBytes(StandardCharsets.UTF_8))
    ()
