package termflow.tui

/**
 * Color depth supported by the underlying terminal.
 *
 * Ordered from least to most capable. `supports` is monotonic: a backend
 * advertising `Truecolor` is treated as supporting any lesser depth, and
 * the renderer downgrades higher-depth `Color` values to fit.
 */
enum ColorDepth:
  case Mono
  case Ansi8
  case Ansi16
  case Indexed256
  case Truecolor

  /** True if this depth is at least as capable as `other`. */
  def supports(other: ColorDepth): Boolean = this.ordinal >= other.ordinal

/**
 * Best-effort terminal capabilities, populated either from environment
 * variables (see [[Capabilities.detect]]) or supplied explicitly by tests.
 *
 * @param colorDepth Maximum color depth the renderer should emit. Higher-depth
 *                   colors are downgraded to this depth at render time.
 * @param unicode    True if the terminal claims a UTF-8 locale.
 * @param mouse      True if the terminal advertises mouse support
 *                   (`xterm`-family `$TERM` values, `screen`, `tmux`,
 *                   `alacritty`, `rxvt`, `linux`).
 */
final case class Capabilities(
  colorDepth: ColorDepth,
  unicode: Boolean,
  mouse: Boolean
)

object Capabilities:

  /**
   * Conservative default used by trait `TerminalBackend` if a backend does not
   * override [[TerminalBackend.capabilities]]. ANSI8 is safe almost everywhere
   * and matches what TermFlow emitted before color depth detection existed.
   */
  val default: Capabilities = Capabilities(
    colorDepth = ColorDepth.Ansi8,
    unicode = true,
    mouse = false
  )

  /**
   * Detect capabilities from a snapshot of environment variables.
   *
   * Honours [no-color.org](https://no-color.org/): if `NO_COLOR` is present
   * with any value, color depth is forced to `Mono`.
   *
   * Color depth heuristic:
   *   - `COLORTERM` ∈ {`truecolor`, `24bit`} → `Truecolor`
   *   - `TERM` contains `256color`           → `Indexed256`
   *   - `TERM` is empty or `dumb`            → `Mono`
   *   - `TERM` looks like an xterm-family    → `Ansi16`
   *   - otherwise                            → `Ansi8`
   *
   * Unicode is inferred from `LANG`/`LC_ALL`/`LC_CTYPE` containing `UTF-8`.
   *
   * Mouse is inferred from a known-good `TERM` family list. This is a heuristic;
   * apps can override the result by constructing `Capabilities` explicitly.
   */
  def detect(env: Map[String, String]): Capabilities =
    val noColor   = env.contains("NO_COLOR")
    val term      = env.getOrElse("TERM", "").toLowerCase
    val colorTerm = env.getOrElse("COLORTERM", "").toLowerCase

    val colorDepth =
      if noColor then ColorDepth.Mono
      else if colorTerm == "truecolor" || colorTerm == "24bit" then ColorDepth.Truecolor
      else if term.contains("256color") then ColorDepth.Indexed256
      else if term.isEmpty || term == "dumb" then ColorDepth.Mono
      else if isXtermFamily(term) then ColorDepth.Ansi16
      else ColorDepth.Ansi8

    val unicode =
      Seq("LC_ALL", "LC_CTYPE", "LANG").exists(k => env.getOrElse(k, "").toUpperCase.contains("UTF-8"))

    val mouse = isXtermFamily(term)

    Capabilities(colorDepth, unicode, mouse)

  private def isXtermFamily(term: String): Boolean =
    term.startsWith("xterm")
      || term.startsWith("screen")
      || term.startsWith("tmux")
      || term.startsWith("rxvt")
      || term.startsWith("alacritty")
      || term.startsWith("kitty")
      || term.startsWith("wezterm")
      || term.startsWith("foot")
      || term == "linux"
