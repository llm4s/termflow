package termflow.tui

/**
 * Named colour slots for building terminal UIs that don't bake in a
 * specific palette.
 *
 * A `Theme` maps short semantic names (`primary`, `success`, `error`, …) to
 * concrete [[Color]] values. Apps reference slots by meaning, and can swap
 * palettes (e.g. dark/light) without touching any view code.
 *
 * ## Direct usage
 *
 * The simplest approach is to hold a theme value somewhere in the model (or
 * as a `val`) and read slots from it:
 *
 * {{{
 * val theme = Theme.dark
 *
 * override def view(m: Model): RootNode =
 *   RootNode(... children = List(
 *     TextNode(2.x, 2.y, List("Status: ".text, "OK".text(fg = theme.success)))
 *   ), ...)
 * }}}
 *
 * ## Using `given` for call-site ergonomics
 *
 * For apps that want to stay theme-agnostic at call sites, bring a `Theme`
 * into implicit scope and use the `themed` extensions defined on [[Theme]]:
 *
 * {{{
 * given Theme = Theme.dark
 *
 * override def view(m: Model): RootNode =
 *   RootNode(... children = List(
 *     TextNode(2.x, 2.y, List(
 *       "Status: ".text,
 *       "OK".themed(_.success)                              // string -> Text
 *     )),
 *     BoxNode(1.x, 1.y, 40, 6, Nil,
 *             style = Style(border = true).themed(_.border)) // style copy
 *   ), ...)
 * }}}
 *
 * ## Customising
 *
 * Themes are plain case classes, so ad-hoc overrides are just `copy`:
 *
 * {{{
 * val highContrast = Theme.dark.copy(success = Color.White)
 * }}}
 *
 * @param primary    Dominant accent colour (headers, active tabs, primary actions).
 * @param secondary  Secondary accent for less prominent controls.
 * @param error      Errors and destructive actions. Usually red.
 * @param warning    Non-fatal warnings. Usually yellow.
 * @param success    Confirmations and success states. Usually green.
 * @param info       Informational highlights. Usually cyan.
 * @param border     Default colour for [[BoxNode]] borders.
 * @param background Background for large surfaces. `Color.Default` means
 *                   "use the terminal's configured background".
 * @param foreground Default body-text colour. `Color.Default` inherits the
 *                   terminal's foreground, which is usually what you want
 *                   unless the theme is explicitly opinionated.
 *
 * @note `muted` / `dim` is intentionally omitted from v1. TermFlow's [[Style]]
 *       has no dim attribute, so a muted slot would have to alias an existing
 *       colour, which is misleading. Add when `Style` grows a dim/faint flag.
 *
 * @note **Background slots only paint cells that explicitly set them.**
 *       The renderer does not fill the whole frame with `theme.background` —
 *       it only colours cells that are actually drawn, so `theme.background`
 *       and `theme.foreground` only become visible on cells whose `Style`
 *       references them. If you toggle from [[Theme.dark]] to [[Theme.light]]
 *       on a black-background terminal, plain text using `fg = theme.foreground`
 *       (i.e. black) becomes invisible because the surrounding cells are not
 *       repainted to white. Workarounds:
 *
 *         - Use [[termflow.tui.widgets.StatusBar]] for any row that needs to
 *           stay visible across themes — it uses inverse video
 *           (`fg = theme.background, bg = theme.primary`) which is always
 *           painted.
 *         - Pin foreground colours to a specific accent slot
 *           (`theme.primary`, `theme.success`, …) rather than `foreground`.
 *         - Or paint backgrounds explicitly via `Style(bg = theme.background)`
 *           on each text run.
 */
final case class Theme(
  primary: Color,
  secondary: Color,
  error: Color,
  warning: Color,
  success: Color,
  info: Color,
  border: Color,
  background: Color,
  foreground: Color
):

  /** All slots as an ordered list. Convenient for tests and debugging. */
  def slots: List[(String, Color)] =
    List(
      "primary"    -> primary,
      "secondary"  -> secondary,
      "error"      -> error,
      "warning"    -> warning,
      "success"    -> success,
      "info"       -> info,
      "border"     -> border,
      "background" -> background,
      "foreground" -> foreground
    )

object Theme:

  /**
   * Opinionated dark-mode palette.
   *
   * Background pinned to [[Color.Black]], foreground to [[Color.White]], with
   * the standard ANSI accent colours for each semantic slot. Suitable as a
   * sensible default for terminals that are already rendering on a dark
   * background.
   */
  val dark: Theme = Theme(
    primary = Color.Blue,
    secondary = Color.Cyan,
    error = Color.Red,
    warning = Color.Yellow,
    success = Color.Green,
    info = Color.Cyan,
    border = Color.Blue,
    background = Color.Black,
    foreground = Color.White
  )

  /**
   * Opinionated light-mode palette.
   *
   * Same accent colours as [[dark]], but background is [[Color.White]] and
   * foreground is [[Color.Black]]. Accent colours are unchanged because the
   * ANSI basic-16 palette renders acceptably on either background.
   */
  val light: Theme = Theme(
    primary = Color.Blue,
    secondary = Color.Cyan,
    error = Color.Red,
    warning = Color.Yellow,
    success = Color.Green,
    info = Color.Cyan,
    border = Color.Blue,
    background = Color.White,
    foreground = Color.Black
  )

  /**
   * Theme with every slot set to [[Color.Default]].
   *
   * Useful as a starting point for apps that want to stay entirely at the
   * terminal's configured palette, or as a null object in tests. Rendering
   * a `mono` theme emits no SGR escapes at all.
   */
  val mono: Theme = Theme(
    primary = Color.Default,
    secondary = Color.Default,
    error = Color.Default,
    warning = Color.Default,
    success = Color.Default,
    info = Color.Default,
    border = Color.Default,
    background = Color.Default,
    foreground = Color.Default
  )

  /**
   * Return the `Theme` from implicit scope.
   *
   * Shorthand for `summon[Theme]`, useful when you want to pass the ambient
   * theme explicitly to something that takes a regular `Theme` parameter.
   */
  def current(using t: Theme): Theme = t

  /**
   * Select a single slot from the ambient theme.
   *
   * {{{
   * given Theme = Theme.dark
   * val accent: Color = Theme.slot(_.primary)
   * }}}
   */
  def slot(select: Theme => Color)(using t: Theme): Color = select(t)

  /**
   * Extension adding `style.themed(_.primary)` syntax for setting a theme
   * slot as the style's foreground colour.
   *
   * Requires a `given Theme` in scope. Preserves every other field on the
   * receiver, so `Style(bold = true).themed(_.success)` yields a bold green
   * style.
   */
  extension (s: Style)
    def themed(select: Theme => Color)(using t: Theme): Style =
      s.copy(fg = select(t))

  /**
   * Extension adding `"label".themed(_.success)` syntax for producing a
   * themed [[Text]] segment directly from a string.
   *
   * Requires a `given Theme` in scope. Equivalent to
   * `"label".text(fg = Theme.slot(_.success))` but reads more naturally
   * inside a `TextNode` child list.
   */
  extension (txt: String)
    def themed(select: Theme => Color)(using t: Theme): Text =
      Text(txt, Style(fg = select(t)))
