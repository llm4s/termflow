package termflow.tui

import termflow.tui.KeyDecoder.InputKey

/**
 * Declarative mapping from terminal keys to application messages.
 *
 * Apps that handle keyboard input via `Sub.InputKey` typically have a
 * pattern-match in their `update` (or a helper) that turns each `InputKey`
 * into the appropriate `Msg`. `Keymap` formalises that lookup table:
 *
 * {{{
 * import termflow.tui.*
 * import termflow.tui.KeyDecoder.InputKey.*
 *
 * val keys: Keymap[Msg] =
 *   Keymap.quit(Msg.Quit) ++
 *   Keymap.focus(next = Msg.NextFocus, previous = Msg.PrevFocus) ++
 *   Keymap(
 *     Enter        -> Msg.Activate,
 *     CharKey('t') -> Msg.ToggleTheme
 *   )
 *
 * // In update:
 * case Msg.ConsoleInputKey(k) =>
 *   keys.lookup(k) match
 *     case Some(msg) => update(model, msg, ctx)
 *     case None      => model.tui
 * }}}
 *
 * Keymaps compose with `++` (later bindings win) so apps can layer global
 * shortcuts (`quit`, `focus`) under app-specific bindings without
 * boilerplate. The contract is intentionally simple — single-key only, no
 * chord support — to match the minimal viable feature set described in #82.
 *
 * @tparam Msg The application's message type.
 */
final case class Keymap[Msg](bindings: Map[InputKey, Msg]):

  /** Look up a message bound to `key`, if any. */
  def lookup(key: InputKey): Option[Msg] = bindings.get(key)

  /**
   * Add or override a single binding. Equivalent to `++(Keymap(binding))`.
   *
   * If `binding._1` is already mapped, the new value replaces it.
   */
  def +(binding: (InputKey, Msg)): Keymap[Msg] =
    Keymap(bindings + binding)

  /**
   * Merge two keymaps. Bindings from `other` win on conflicts so callers
   * can layer overrides on top of shared baselines:
   *
   * {{{
   * val base    = Keymap.quit(Msg.Quit)
   * val mine    = Keymap(KeyDecoder.InputKey.CharKey('q') -> Msg.PromptQuit)
   * val merged  = base ++ mine    // 'q' now goes to Msg.PromptQuit
   * }}}
   */
  def ++(other: Keymap[Msg]): Keymap[Msg] =
    Keymap(bindings ++ other.bindings)

  /** Number of registered bindings. */
  def size: Int = bindings.size

  /** `true` when no bindings are registered. */
  def isEmpty: Boolean = bindings.isEmpty

object Keymap:

  /** A keymap with no bindings. `lookup` always returns `None`. */
  def empty[Msg]: Keymap[Msg] = Keymap(Map.empty[InputKey, Msg])

  /**
   * Build a keymap from a varargs list of bindings.
   *
   * If the same key appears more than once, the last binding wins
   * (consistent with `Map(...)` semantics).
   */
  def apply[Msg](bindings: (InputKey, Msg)*): Keymap[Msg] =
    Keymap(bindings.toMap)

  /**
   * Conventional quit bindings: `Ctrl+C`, `Escape`, and `q` / `Q` all map
   * to the same message.
   *
   * Use `++` to add more bindings without overwriting these.
   */
  def quit[Msg](msg: Msg): Keymap[Msg] = Keymap(
    InputKey.Ctrl('C')    -> msg,
    InputKey.Escape       -> msg,
    InputKey.CharKey('q') -> msg,
    InputKey.CharKey('Q') -> msg
  )

  /**
   * Conventional focus bindings: `Tab` to advance, no shift-tab binding
   * because the current `KeyDecoder` doesn't distinguish `Shift+Tab` from
   * a bare ESC sequence.
   *
   * Apps that want explicit forward / backward bindings can pass both
   * messages and add their own back-cycle key (e.g. `BackTick`):
   *
   * {{{
   * Keymap.focus(next = Msg.NextFocus, previous = Msg.PrevFocus) ++
   *   Keymap(InputKey.CharKey('`') -> Msg.PrevFocus)
   * }}}
   *
   * `Tab` on most terminals is decoded as `Ctrl+I` by the ASCII control
   * range, so that's the binding installed.
   *
   * @param next     Message to dispatch on Tab (`Ctrl+I`).
   * @param previous Currently unused — wire your own binding via `++`. The
   *                 parameter exists so the API doesn't have to change once
   *                 `Shift+Tab` decoding is supported.
   */
  def focus[Msg](next: Msg, previous: Msg): Keymap[Msg] =
    val _ = previous // reserved; see ScalaDoc
    Keymap(InputKey.Ctrl('I') -> next)

  /**
   * Conventional editing bindings: arrow keys, Home, End, Enter, Backspace.
   *
   * Useful when wiring an app that handles its own text input (without
   * using the `Prompt` helper). Apps using `Prompt` already get these
   * via `Prompt.handleKey`.
   *
   * @param onEnter     Message dispatched on Enter.
   * @param onBackspace Message dispatched on Backspace.
   * @param onLeft      Message dispatched on Left arrow.
   * @param onRight     Message dispatched on Right arrow.
   */
  def editing[Msg](
    onEnter: Msg,
    onBackspace: Msg,
    onLeft: Msg,
    onRight: Msg
  ): Keymap[Msg] = Keymap(
    InputKey.Enter      -> onEnter,
    InputKey.Backspace  -> onBackspace,
    InputKey.ArrowLeft  -> onLeft,
    InputKey.ArrowRight -> onRight
  )
