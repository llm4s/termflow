package termflow.tui

/**
 * Display-width calculator for Unicode code points and strings, mirroring
 * the venerable `wcwidth` API by Markus Kuhn.
 *
 * Returns the number of terminal cells a code point occupies:
 *   - `-1` for control characters (the caller decides what to do)
 *   - `0`  for combining marks, zero-width joiners, and other composition glue
 *   - `1`  for the common case of narrow / latin / typical text
 *   - `2`  for East Asian Wide / Fullwidth characters and emoji that round
 *          to two cells in monospace
 *
 * Supplementary-plane code points beyond `0xFFFF` (most emoji, including ZWJ
 * sequences) are still handled at the [[stringWidth]] level — the
 * `String.codePoints` view yields full 32-bit code points — but they cannot
 * round-trip through a single Java `Char`. The cell grid stores them as
 * surrogate pairs in two adjacent cells; rendering quality on those is
 * terminal-dependent.
 *
 * Ambiguous-width characters (UAX #11 category A) default to width 1
 * ("narrow"). The convention matches xterm in the locale we target most
 * often; CJK locales that prefer width-2 rendering would need a
 * locale-aware override that is currently out of scope.
 */
object WCWidth:

  /**
   * Display width of a single Unicode code point. Returns `-1` for `C0`/`C1`
   * control characters; the caller is expected to either drop them or render
   * a placeholder.
   */
  def codePointWidth(cp: Int): Int =
    if cp == 0 then 0
    else if cp < 32 || (cp >= 0x7f && cp < 0xa0) then -1
    else if isZeroWidth(cp) then 0
    else if isWide(cp) then 2
    else 1

  /**
   * Display width of a single Java `Char`, usable when the caller can
   * guarantee BMP-only input. Surrogate halves report width 1 because we
   * cannot resolve them in isolation.
   */
  def charWidth(ch: Char): Int =
    if ch.isSurrogate then 1
    else codePointWidth(ch.toInt)

  /**
   * Sum of [[codePointWidth]] across `s`, treating control characters as
   * width 0 (they would otherwise drag the total negative). Use this for
   * layout calculations — cell counts, viewport sizing, scroll offsets.
   */
  def stringWidth(s: String): Int =
    var i     = 0
    var total = 0
    while i < s.length do
      val cp = s.codePointAt(i)
      val w  = codePointWidth(cp)
      if w > 0 then total += w
      i += java.lang.Character.charCount(cp)
    total

  /**
   * Combining marks, zero-width joiner / non-joiner, and the variation
   * selectors. Sourced from Unicode general categories `Mn`, `Me`, `Cf` plus
   * the well-known explicit zero-width code points; not exhaustive but
   * covers the cases that matter for typical TUI content.
   */
  private def isZeroWidth(cp: Int): Boolean =
    cp == 0x200b ||                     // zero-width space
      cp == 0x200c ||                   // zero-width non-joiner
      cp == 0x200d ||                   // zero-width joiner
      cp == 0xfeff ||                   // BOM
      (cp >= 0x0300 && cp <= 0x036f) || // combining diacritical marks
      (cp >= 0x1ab0 && cp <= 0x1aff) || // combining marks ext.
      (cp >= 0x1dc0 && cp <= 0x1dff) || // combining marks supplement
      (cp >= 0x20d0 && cp <= 0x20ff) || // combining marks for symbols
      (cp >= 0xfe00 && cp <= 0xfe0f) || // variation selectors 1..16
      (cp >= 0xfe20 && cp <= 0xfe2f) || // combining half marks
      (cp >= 0xe0100 && cp <= 0xe01ef)  // variation selectors supp.

  /**
   * East Asian Wide / Fullwidth ranges plus the major emoji blocks. Derived
   * from Unicode UAX #11 (EastAsianWidth) — `W` and `F` rows — and a small
   * curated set of emoji ranges. Updates with newer Unicode revisions are
   * cheap (just append a range).
   */
  private def isWide(cp: Int): Boolean =
    // CJK Symbols and Punctuation .. CJK Unified Ideographs Extension A
    (cp >= 0x1100 && cp <= 0x115f) ||   // Hangul Jamo
      (cp >= 0x2e80 && cp <= 0x303e) || // CJK Radicals Supplement, Kangxi
      (cp >= 0x3041 && cp <= 0x33ff) || // Hiragana, Katakana, CJK Sym/Punc
      (cp >= 0x3400 && cp <= 0x4dbf) || // CJK Unified Ideographs Ext A
      (cp >= 0x4e00 && cp <= 0x9fff) || // CJK Unified Ideographs
      (cp >= 0xa000 && cp <= 0xa4cf) || // Yi Syllables
      (cp >= 0xac00 && cp <= 0xd7a3) || // Hangul Syllables
      (cp >= 0xf900 && cp <= 0xfaff) || // CJK Compatibility Ideographs
      (cp >= 0xfe30 && cp <= 0xfe4f) || // CJK Compatibility Forms
      (cp >= 0xff00 && cp <= 0xff60) || // Fullwidth Forms
      (cp >= 0xffe0 && cp <= 0xffe6) || // Fullwidth signs
      // Emoji / pictographs
      (cp >= 0x1f300 && cp <= 0x1f64f) || // Misc Symbols, Emoticons
      (cp >= 0x1f680 && cp <= 0x1f6ff) || // Transport & Map
      (cp >= 0x1f700 && cp <= 0x1f77f) || // Alchemical
      (cp >= 0x1f900 && cp <= 0x1f9ff) || // Supplemental Symbols and Pictographs
      (cp >= 0x1fa70 && cp <= 0x1faff) || // Symbols and Pictographs Ext-A
      (cp >= 0x20000 && cp <= 0x2fffd) || // CJK Ideographs Extension B..F
      (cp >= 0x30000 && cp <= 0x3fffd)    // CJK Ideographs Extension G+
