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
 * boilerplate.
 *
 * For multi-key sequences (e.g. `Ctrl+X Ctrl+C`), see [[ChordKeymap]].
 * For modal editor-style mode switching, see [[ModalKeymap]].
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
   * Conventional focus bindings: `Tab` advances, `Shift+Tab` retreats.
   *
   * `Tab` arrives as `Ctrl+I` from the ASCII decoder; `Shift+Tab` arrives
   * as `InputKey.BackTab` (decoded from the `[Z` escape sequence).
   *
   * @param next     Message dispatched on `Tab`.
   * @param previous Message dispatched on `Shift+Tab`.
   */
  def focus[Msg](next: Msg, previous: Msg): Keymap[Msg] =
    Keymap(
      InputKey.Ctrl('I') -> next,
      InputKey.BackTab   -> previous
    )

  /**
   * Bind `ArrowUp` / `ArrowDown` to `previous` / `next` focus messages.
   *
   * Safe to compose into a truly-global keymap because single-line widgets
   * like [[termflow.tui.widgets.TextField]] don't consume vertical arrows.
   *
   * {{{
   * val Globals =
   *   Keymap.focus(next = NextFocus, previous = PrevFocus) ++
   *     Keymap.focusVertical(previous = PrevFocus, next = NextFocus)
   * }}}
   *
   * @param previous Message dispatched on `ArrowUp`.
   * @param next     Message dispatched on `ArrowDown`.
   */
  def focusVertical[Msg](previous: Msg, next: Msg): Keymap[Msg] =
    Keymap(
      InputKey.ArrowUp   -> previous,
      InputKey.ArrowDown -> next
    )

  /**
   * Bind `ArrowLeft` / `ArrowRight` to `previous` / `next` focus messages.
   *
   * **Compose into a non-text focus layer only.** Inside a
   * [[termflow.tui.widgets.TextField]], the horizontal arrows are used for
   * in-field cursor movement — binding them globally would break editing.
   * The idiom is to layer the apps' per-element routing so that fields
   * consume these keys themselves and button-focused (or no-focus) rows
   * fall through to this keymap.
   *
   * {{{
   * val NonTextShortcuts =
   *   Keymap.focusHorizontal(previous = PrevFocus, next = NextFocus)
   * }}}
   *
   * @param previous Message dispatched on `ArrowLeft`.
   * @param next     Message dispatched on `ArrowRight`.
   */
  def focusHorizontal[Msg](previous: Msg, next: Msg): Keymap[Msg] =
    Keymap(
      InputKey.ArrowLeft  -> previous,
      InputKey.ArrowRight -> next
    )

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

  /**
   * Render a chord sequence as a human-readable string for help popups
   *  and logging. e.g. `Vector(Ctrl('X'), Ctrl('C'))` → `"C-x C-c"`.
   */
  def renderChord(seq: Vector[InputKey]): String =
    seq.map(renderKey).mkString(" ")

  /** Render a single key for help display. */
  def renderKey(k: InputKey): String = k match
    case InputKey.CharKey(c) if c == ' ' => "Space"
    case InputKey.CharKey(c)             => c.toString
    case InputKey.Ctrl(c)                => s"C-${c.toLower}"
    case InputKey.Enter                  => "Enter"
    case InputKey.Escape                 => "Esc"
    case InputKey.Backspace              => "Backspace"
    case InputKey.Delete                 => "Delete"
    case InputKey.Insert                 => "Insert"
    case InputKey.Home                   => "Home"
    case InputKey.End                    => "End"
    case InputKey.PageUp                 => "PageUp"
    case InputKey.PageDown               => "PageDown"
    case InputKey.ArrowUp                => "Up"
    case InputKey.ArrowDown              => "Down"
    case InputKey.ArrowLeft              => "Left"
    case InputKey.ArrowRight             => "Right"
    case InputKey.BackTab                => "S-Tab"
    case InputKey.F1                     => "F1"
    case InputKey.F2                     => "F2"
    case InputKey.F3                     => "F3"
    case InputKey.F4                     => "F4"
    case InputKey.F5                     => "F5"
    case InputKey.F6                     => "F6"
    case InputKey.F7                     => "F7"
    case InputKey.F8                     => "F8"
    case InputKey.F9                     => "F9"
    case InputKey.F10                    => "F10"
    case InputKey.F11                    => "F11"
    case InputKey.F12                    => "F12"
    case InputKey.Modified(inner, mods) =>
      val parts = List(
        if mods.shift then "S" else "",
        if mods.alt then "A" else "",
        if mods.ctrl then "C" else ""
      ).filter(_.nonEmpty)
      if parts.isEmpty then renderKey(inner)
      else s"${parts.mkString("-")}-${renderKey(inner)}"
    case InputKey.Paste(_)     => "Paste"
    case InputKey.Mouse(_)     => "Mouse"
    case InputKey.Unknown(seq) => s"?($seq)"
    case InputKey.EndOfInput   => "EOF"

/**
 * One step in a [[ChordKeymap]] dispatch.
 *
 *   - [[ChordResult.Pending]]  — a prefix of a chord matched; the
 *                                caller should keep the returned state
 *                                and feed the next key into it.
 *   - [[ChordResult.Resolved]] — a chord completed; the caller should
 *                                dispatch `msg`. The state resets.
 *   - [[ChordResult.NoMatch]]  — neither a prefix nor a complete chord.
 *                                The state resets so the next key starts
 *                                a fresh sequence. The unmatched key is
 *                                returned in `key` for fall-through.
 */
enum ChordResult[+Msg]:
  case Pending(state: ChordState)
  case Resolved(state: ChordState, msg: Msg)
  case NoMatch(state: ChordState, key: InputKey)

/**
 * In-flight chord progress. Carries the keys typed so far but not yet
 * resolved; defaults to empty (no chord underway).
 *
 * Apps hold a single [[ChordState]] in their model and feed it through
 * [[ChordKeymap.step]] each keystroke.
 */
final case class ChordState(prefix: Vector[InputKey] = Vector.empty):
  def isEmpty: Boolean              = prefix.isEmpty
  def reset: ChordState             = ChordState(Vector.empty)
  def push(k: InputKey): ChordState = ChordState(prefix :+ k)

/**
 * Keymap supporting multi-key chord sequences.
 *
 * A chord is a [[Vector]] of [[InputKey]]s that, when typed in sequence,
 * dispatches a single message. Chords can share a prefix
 * (`Ctrl+X Ctrl+C` and `Ctrl+X Ctrl+S` both start with `Ctrl+X`) — the
 * dispatcher tracks how far through a chord the user is and only fires
 * the message when the sequence completes.
 *
 * Single-key bindings are just chords of length 1. The companion
 * exposes `fromKeymap` so apps can promote an existing [[Keymap]] into
 * a `ChordKeymap` for free.
 *
 * {{{
 * import termflow.tui.KeyDecoder.InputKey.*
 *
 * val keys: ChordKeymap[Msg] =
 *   ChordKeymap.empty
 *     .bind(Vector(Ctrl('X'), Ctrl('C')), Msg.Quit)
 *     .bind(Vector(Ctrl('X'), Ctrl('S')), Msg.Save)
 *     .bind(Vector(CharKey('q')), Msg.QuickQuit)
 *
 * // In update:
 * case Msg.ConsoleInputKey(k) =>
 *   keys.step(model.chord, k) match
 *     case ChordResult.Resolved(s, msg) => model.copy(chord = s).withMsg(msg)
 *     case ChordResult.Pending(s)       => model.copy(chord = s).tui
 *     case ChordResult.NoMatch(s, key)  => fallthrough(model.copy(chord = s), key)
 * }}}
 *
 * @tparam Msg The application's message type.
 */
final case class ChordKeymap[Msg](chords: Map[Vector[InputKey], Msg]):

  /**
   * Add or override a chord binding. Returns a new ChordKeymap with the
   * binding installed.
   */
  def bind(sequence: Vector[InputKey], msg: Msg): ChordKeymap[Msg] =
    if sequence.isEmpty then this
    else copy(chords = chords + (sequence -> msg))

  /** Convenience: bind a single-key shortcut. */
  def bind(key: InputKey, msg: Msg): ChordKeymap[Msg] =
    bind(Vector(key), msg)

  /** Merge two chord keymaps. Right-side bindings win on conflicts. */
  def ++(other: ChordKeymap[Msg]): ChordKeymap[Msg] =
    ChordKeymap(chords ++ other.chords)

  /** Look up a complete chord, or `None` if the sequence isn't bound. */
  def lookup(sequence: Vector[InputKey]): Option[Msg] =
    chords.get(sequence)

  /**
   * Whether any bound chord starts with `prefix` *and* is longer than
   *  it. Used to detect a pending state during dispatch.
   */
  def isPrefix(prefix: Vector[InputKey]): Boolean =
    chords.keys.exists(seq => seq.size > prefix.size && seq.startsWith(prefix))

  /**
   * Advance the chord state by one keystroke. See [[ChordResult]] for
   * the three possible outcomes. Apps fold the returned state back into
   * their model.
   */
  def step(state: ChordState, key: InputKey): ChordResult[Msg] =
    val next = state.push(key)
    lookup(next.prefix) match
      case Some(msg) => ChordResult.Resolved(state.reset, msg)
      case None =>
        if isPrefix(next.prefix) then ChordResult.Pending(next)
        else ChordResult.NoMatch(state.reset, key)

  /** Number of bound chords. */
  def size: Int = chords.size

  /** `true` when no chords are bound. */
  def isEmpty: Boolean = chords.isEmpty

  /**
   * Bindings as `(rendered chord, message)` pairs, suitable for a help
   *  popup. Chord rendering uses [[Keymap.renderChord]].
   */
  def helpEntries: List[(String, Msg)] =
    chords.toList
      .map { case (seq, msg) => Keymap.renderChord(seq) -> msg }
      .sortBy(_._1)

object ChordKeymap:

  /** Empty keymap. */
  def empty[Msg]: ChordKeymap[Msg] = ChordKeymap(Map.empty)

  /**
   * Promote a single-key [[Keymap]] into a [[ChordKeymap]] (every
   *  binding becomes a chord of length 1).
   */
  def fromKeymap[Msg](k: Keymap[Msg]): ChordKeymap[Msg] =
    ChordKeymap(k.bindings.map { case (key, msg) => Vector(key) -> msg })

/**
 * Modal keymap — different bindings active depending on which mode the
 * app is in. Mirrors the modal-editor metaphor (Vim's normal/insert/
 * visual): the same keystroke can do different things based on mode.
 *
 * Apps own the current mode in their model. Mode changes are themselves
 * messages: a binding's right-hand side typically encodes "mode
 * transition + side effect" by being interpreted in `update`.
 *
 * {{{
 * enum Mode:
 *   case Normal, Insert, Visual
 *
 * val keys = ModalKeymap[Mode, Msg](
 *   Mode.Normal -> ChordKeymap.empty
 *     .bind(InputKey.CharKey('i'), Msg.EnterInsert)
 *     .bind(InputKey.CharKey('q'), Msg.Quit),
 *   Mode.Insert -> ChordKeymap.empty
 *     .bind(InputKey.Escape, Msg.LeaveInsert)
 * )
 *
 * // In update:
 * case Msg.ConsoleInputKey(k) =>
 *   keys.step(model.mode, model.chord, k) match …
 * }}}
 *
 * @tparam Mode The mode enum.
 * @tparam Msg  The application's message type.
 */
final case class ModalKeymap[Mode, Msg](modes: Map[Mode, ChordKeymap[Msg]]):

  /** The keymap active in `mode`, or [[ChordKeymap.empty]] if not bound. */
  def forMode(mode: Mode): ChordKeymap[Msg] =
    modes.getOrElse(mode, ChordKeymap.empty[Msg])

  /** Replace or extend the bindings for `mode`. */
  def withMode(mode: Mode, k: ChordKeymap[Msg]): ModalKeymap[Mode, Msg] =
    copy(modes = modes + (mode -> k))

  /**
   * Step the chord state forward in the current mode. Equivalent to
   *  `forMode(mode).step(state, key)`.
   */
  def step(mode: Mode, state: ChordState, key: InputKey): ChordResult[Msg] =
    forMode(mode).step(state, key)

  /** Help entries for `mode`, suitable for the help overlay. */
  def helpEntries(mode: Mode): List[(String, Msg)] =
    forMode(mode).helpEntries

object ModalKeymap:

  /** Empty modal keymap (no modes registered). */
  def empty[Mode, Msg]: ModalKeymap[Mode, Msg] = ModalKeymap(Map.empty)

  /** Build from explicit `(mode, chordKeymap)` pairs. */
  def apply[Mode, Msg](pairs: (Mode, ChordKeymap[Msg])*): ModalKeymap[Mode, Msg] =
    ModalKeymap(pairs.toMap)

object KeymapHelp:

  /**
   * Build a help overlay listing the bindings in `chords`. Each row is
   * `<chord>  <description>`, where the description is whatever the
   * caller provides for each message (typically a short verb).
   *
   * @param title    Title rendered on the top border.
   * @param chords   The chord keymap to summarise.
   * @param describe Map message → human-readable description.
   * @param position Anchor (default centred).
   */
  def overlay[Msg](
    title: String,
    chords: ChordKeymap[Msg],
    describe: Msg => String,
    position: OverlayPosition = OverlayPosition.Centered
  )(using theme: Theme): Overlay =
    val rows = chords.helpEntries.map { case (chord, msg) => chord -> describe(msg) }
    val maxChord =
      if rows.isEmpty then 0 else rows.map(_._1.length).max
    val maxDesc =
      if rows.isEmpty then 0 else rows.map(_._2.length).max
    val contentWidth = maxChord + 2 + maxDesc
    val width        = math.max(title.length + 4, contentWidth + 4)
    val height       = math.max(4, 3 + rows.size)

    val box = Theme.box(XCoord(1), YCoord(1), width, height)
    val titleNode = TextNode(
      XCoord(3),
      YCoord(1),
      List(Text(s" $title ", Style(fg = theme.primary, bold = true)))
    )
    val rowNodes = rows.zipWithIndex.map { case ((chord, desc), i) =>
      val padded = chord.padTo(maxChord, ' ')
      TextNode(
        XCoord(3),
        YCoord(3 + i),
        List(
          Text(padded, Style(fg = theme.primary, bold = true)),
          Text("  ", Style(fg = theme.foreground)),
          Text(desc, Style(fg = theme.foreground))
        )
      )
    }
    Overlay(
      position = position,
      width = width,
      height = height,
      children = box :: titleNode :: rowNodes,
      inputCapture = InputCapture.Modal
    )
