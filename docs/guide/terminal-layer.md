# Terminal layer

> *Stub — full content lands in Phase C of the docs roll-out.*

This guide covers the lowest layer of TermFlow, published as
`termflow-terminal`. It is what you reach for when the higher-level
abstractions are in the way: capability detection, raw key reads,
direct ANSI emission.

Topics planned for this page:

- The `TerminalBackend` trait and `JLineTerminalBackend` default.
- `Capabilities` detection — true colour, bracketed paste, mouse,
  extended modifiers.
- `KeyDecoder` — the single source of truth for reading keystrokes,
  modifier handling, and the SGR-1006 mouse multiplex onto
  `InputKey.Mouse`.
- `WCWidth` — column-width arithmetic for CJK and emoji.
- `Grapheme` — UAX #29 cluster boundaries used by `Prompt` and
  `MultiLineInput`.
- When to depend on this module alone, and when to climb up.

Until the page is filled in, the
[contributor design doc](../contrib/DESIGN.md) is the most thorough
write-up.
