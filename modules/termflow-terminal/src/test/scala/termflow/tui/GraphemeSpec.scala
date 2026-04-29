package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class GraphemeSpec extends AnyFunSuite:

  // --- previousBoundary -----------------------------------------------------

  test("previousBoundary on plain ASCII steps back one char"):
    val s = "hello"
    assert(Grapheme.previousBoundary(s, 5) == 4)
    assert(Grapheme.previousBoundary(s, 1) == 0)
    assert(Grapheme.previousBoundary(s, 0) == 0)

  test("previousBoundary skips a combining mark with its base"):
    // "é" = e + COMBINING ACUTE ACCENT — one grapheme, two chars.
    val s = "éf"
    assert(s.length == 3)
    // Cursor at end; previous boundary should jump back over BOTH e and the
    // combining mark to land at 0 (or 1 depending on interpretation —
    // BreakIterator returns 1 for "before f").
    // From end-of-string the previous boundary is between the combining
    // mark and 'f' (idx 2). Two boundaries exist: 0, 2, and 3.
    assert(Grapheme.previousBoundary(s, 3) == 2)
    // From between the e+mark cluster and 'f' (idx 2), the previous
    // boundary is 0 — stepping over the combining mark + base together.
    assert(Grapheme.previousBoundary(s, 2) == 0, "step back over the cluster as one unit")

  test("previousBoundary handles a surrogate pair as one unit"):
    // U+1F600 GRINNING FACE — surrogate pair, one grapheme.
    val emoji = "a😀b"
    assert(emoji.length == 4)
    // From after b, previous boundary skips back to after the emoji.
    assert(Grapheme.previousBoundary(emoji, 4) == 3)
    // From after the emoji (idx 3), step back jumps the surrogate pair to idx 1.
    assert(Grapheme.previousBoundary(emoji, 3) == 1)

  test("previousBoundary clamps index to s.length"):
    val s = "abc"
    assert(Grapheme.previousBoundary(s, 999) == 2)

  // --- nextBoundary ---------------------------------------------------------

  test("nextBoundary on plain ASCII steps forward one char"):
    val s = "hello"
    assert(Grapheme.nextBoundary(s, 0) == 1)
    assert(Grapheme.nextBoundary(s, 4) == 5)
    assert(Grapheme.nextBoundary(s, 5) == 5)

  test("nextBoundary skips a surrogate pair"):
    val emoji = "a😀b"
    assert(Grapheme.nextBoundary(emoji, 1) == 3, "after a, jump to after the emoji")

  test("nextBoundary skips a combining mark"):
    val s = "éf"
    // After 'e' (idx 1), the combining mark at idx 1..2 attaches to e —
    // nextBoundary from idx 0 should land at idx 2.
    assert(Grapheme.nextBoundary(s, 0) == 2)

  // --- count ---------------------------------------------------------------

  test("count returns the grapheme count"):
    assert(Grapheme.count("") == 0)
    assert(Grapheme.count("hello") == 5)
    assert(Grapheme.count("éf") == 2, "é + f = 2 graphemes")
    assert(Grapheme.count("a😀b") == 3, "a + 😀 + b = 3 graphemes")
