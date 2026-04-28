package termflow.tui

import java.text.BreakIterator
import java.util.Locale

/**
 * Grapheme-cluster boundary helpers, used by editing widgets so that
 * cursor navigation, backspace, and delete operate on user-perceived
 * characters rather than UTF-16 code units.
 *
 * Backed by `java.text.BreakIterator.getCharacterInstance`, which the
 * JDK implements per Unicode UAX #29 (extended grapheme clusters). That
 * covers the cases TUI input cares about — combining marks, surrogate
 * pairs, regional-indicator flag sequences (🇺🇸), and ZWJ-joined emoji
 * (👨‍👩‍👧).
 *
 * The API is index-based so it composes with the existing `Vector[Char]`
 * buffer in `Prompt.State`: indices are UTF-16 code-unit offsets into
 * the equivalent `String`, which is the same model `Vector[Char]` uses
 * (one slot per code unit, surrogates split across two slots).
 */
object Grapheme:

  /**
   * Index of the grapheme boundary immediately before `index`, or `0`
   * if `index` is already at the start.
   *
   * @param s     The current text.
   * @param index UTF-16 code-unit position to step back from. Clamped
   *              to `[0, s.length]`.
   */
  def previousBoundary(s: String, index: Int): Int =
    if s.isEmpty || index <= 0 then 0
    else
      val idx  = math.min(index, s.length)
      val it   = iterator(s)
      val prev = it.preceding(idx)
      if prev == BreakIterator.DONE then 0 else prev

  /**
   * Index of the grapheme boundary immediately after `index`, or
   * `s.length` if `index` is already at the end.
   *
   * @param s     The current text.
   * @param index UTF-16 code-unit position to step forward from.
   *              Clamped to `[0, s.length]`.
   */
  def nextBoundary(s: String, index: Int): Int =
    if s.isEmpty || index >= s.length then s.length
    else
      val idx = math.max(index, 0)
      val it  = iterator(s)
      // BreakIterator.following(idx) returns the boundary strictly after idx.
      val next = it.following(idx)
      if next == BreakIterator.DONE then s.length else next

  /**
   * Total number of graphemes in `s`. Convenience for callers that
   * want grapheme counts (e.g. truncate-to-N-graphemes display).
   */
  def count(s: String): Int =
    if s.isEmpty then 0
    else
      val it = iterator(s)
      var n  = 0
      it.first()
      var nxt = it.next()
      while nxt != BreakIterator.DONE do
        n += 1
        nxt = it.next()
      n

  private def iterator(s: String): BreakIterator =
    val it = BreakIterator.getCharacterInstance(Locale.ROOT)
    it.setText(s)
    it
