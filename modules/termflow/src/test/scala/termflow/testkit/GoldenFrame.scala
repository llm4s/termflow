package termflow.testkit

import termflow.tui.AnsiRenderer.RenderFrame

/**
 * Serialize a `RenderFrame` to a human-reviewable golden-file format.
 *
 * The format is deliberately line-oriented so `git diff` shows exactly which
 * row/column drifted when a snapshot fails. Each grid row is wrapped in
 * `|` markers so trailing whitespace survives editors and CI pipelines that
 * strip it.
 *
 * Example:
 * {{{
 * # width=10 height=3
 * # cursor=2,2
 * |┌────────┐|
 * |│ hello  │|
 * |└────────┘|
 * }}}
 *
 * v1 encodes characters only. Style and colour information is omitted on
 * purpose — keeping goldens character-only makes them reviewable, avoids
 * churn on cosmetic style tweaks, and matches what the recent render-pipeline
 * regressions (#73, #74) actually broke.
 */
object GoldenFrame:

  private val RowPrefix = "|"
  private val RowSuffix = "|"
  private val NL        = "\n"

  /** Serialize a frame. Safe to pass the result directly to `===`. */
  def serialize(frame: RenderFrame): String =
    val sb = new StringBuilder
    sb.append("# width=").append(frame.width)
    sb.append(" height=").append(frame.height)
    sb.append(NL)
    frame.cursor.foreach(c => sb.append("# cursor=").append(c.x.value).append(",").append(c.y.value).append(NL))
    var row = 0
    while row < frame.height do
      sb.append(RowPrefix)
      var col = 0
      while col < frame.width do
        sb.append(frame.cells(row)(col).ch)
        col += 1
      sb.append(RowSuffix)
      sb.append(NL)
      row += 1
    sb.toString

  /**
   * Produce a readable diff of two serialized frames.
   *
   * Matches on lines; when a mismatch is found, emits the first few differing
   * rows with an arrow pointing at the first differing column. Falls back to
   * a simple "lengths differ" message if the grids are different sizes.
   */
  def describeDiff(expected: String, actual: String): String =
    val eLines = expected.split('\n').toVector
    val aLines = actual.split('\n').toVector
    val sb     = new StringBuilder
    sb.append("golden frame mismatch:\n")
    val maxLines = math.max(eLines.size, aLines.size)
    var shown    = 0
    var row      = 0
    while row < maxLines && shown < 6 do
      val e = eLines.lift(row).getOrElse("<missing>")
      val a = aLines.lift(row).getOrElse("<missing>")
      if e != a then
        sb.append(s"  line ${row + 1}:\n")
        sb.append(s"    expected: $e\n")
        sb.append(s"    actual:   $a\n")
        val diffCol = firstDiffIndex(e, a)
        if diffCol >= 0 then
          // Anchor the arrow under the first differing character.
          val _ = sb.append("              ").append(" " * diffCol).append("^\n")
        shown += 1
      row += 1
    if shown == 0 then sb.append("  (diff present but not localized)\n")
    sb.toString

  private def firstDiffIndex(a: String, b: String): Int =
    val len = math.min(a.length, b.length)
    var i   = 0
    while i < len && a.charAt(i) == b.charAt(i) do i += 1
    if i < a.length || i < b.length then i else -1
