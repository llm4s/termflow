package termflow.tui

/**
 * Identifier for a focusable element in the UI.
 *
 * `FocusId` is opaque over `String` so it can't be confused with arbitrary
 * domain strings. Construct with `FocusId("save")` and unwrap with
 * `id.value`. Identifiers are compared by string value, so two `FocusId`s
 * built from the same string are equal.
 */
opaque type FocusId = String

object FocusId:

  /** Wrap a raw string as a `FocusId`. */
  def apply(s: String): FocusId = s

  extension (id: FocusId)
    /** Unwrap to the underlying string. */
    def value: String = id

/**
 * Tracks an ordered list of focusable elements and which one currently owns
 * focus.
 *
 * `FocusManager` is a plain immutable value — apps store it in their model
 * and call `next` / `previous` / `focus` from `update` to produce a new
 * manager. `view` then asks `isFocused(id)` per widget to decide on
 * presentation.
 *
 * {{{
 * import termflow.tui.*
 * import termflow.tui.widgets.*
 *
 * given Theme = Theme.dark
 * val Save   = FocusId("save")
 * val Cancel = FocusId("cancel")
 *
 * case class Model(fm: FocusManager)
 * val initial = Model(FocusManager(Vector(Save, Cancel)))
 *
 * // In update:
 * case Msg.Tab => model.copy(fm = model.fm.next).tui
 *
 * // In view:
 * Layout.row(gap = 2)(
 *   Button("Save",   focused = model.fm.isFocused(Save)),
 *   Button("Cancel", focused = model.fm.isFocused(Cancel))
 * )
 * }}}
 *
 * The manager is a pure value: every transition method returns a new
 * `FocusManager` and never mutates the receiver. Cycling wraps around at
 * either end of the order. An empty manager has no current focus and all
 * transitions are no-ops.
 *
 * @param ids     Focusable element ids in cycle order.
 * @param current The id that currently owns focus, if any.
 */
final case class FocusManager(
  ids: Vector[FocusId],
  current: Option[FocusId]
):

  /** `true` when `id` is the currently focused element. */
  def isFocused(id: FocusId): Boolean = current.contains(id)

  /**
   * Move focus to the next element in the order, wrapping back to the start.
   *
   * No-op when the manager is empty. If the current focus isn't in `ids`
   * (e.g. after a `withIds` shrink), focus jumps to the first element.
   */
  def next: FocusManager =
    if ids.isEmpty then this
    else
      val nextId = current match
        case Some(c) =>
          val idx = ids.indexOf(c)
          if idx < 0 then ids.head else ids((idx + 1) % ids.size)
        case None => ids.head
      copy(current = Some(nextId))

  /**
   * Move focus to the previous element in the order, wrapping to the last.
   *
   * No-op when the manager is empty. If the current focus isn't in `ids`,
   * focus jumps to the last element.
   */
  def previous: FocusManager =
    if ids.isEmpty then this
    else
      val prevId = current match
        case Some(c) =>
          val idx = ids.indexOf(c)
          if idx < 0 then ids.last
          else ids((idx - 1 + ids.size) % ids.size)
        case None => ids.last
      copy(current = Some(prevId))

  /**
   * Set focus to `id` explicitly.
   *
   * No-op when `id` isn't in the manager's `ids` — the previous focus is
   * preserved. Use [[withIds]] to register a new focusable first.
   */
  def focus(id: FocusId): FocusManager =
    if ids.contains(id) then copy(current = Some(id)) else this

  /**
   * Clear focus without removing any ids.
   *
   * Useful when the app temporarily wants no element focused (e.g. during
   * a modal overlay).
   */
  def clear: FocusManager = copy(current = None)

  /**
   * Replace the registered focus order.
   *
   * If the previous current focus is still in `newIds`, it's preserved.
   * Otherwise the first id (if any) becomes focused, or focus is cleared
   * for an empty list.
   */
  def withIds(newIds: Vector[FocusId]): FocusManager =
    val nextCurrent = current match
      case Some(c) if newIds.contains(c) => Some(c)
      case _                             => newIds.headOption
    FocusManager(newIds, nextCurrent)

object FocusManager:

  /** Empty manager — no focusable elements, no current focus. */
  val empty: FocusManager = FocusManager(Vector.empty, None)

  /**
   * Build a manager whose initial focus is the first id, if any.
   *
   * {{{
   * val fm = FocusManager(Vector(FocusId("name"), FocusId("email")))
   * fm.focused == Some(FocusId("name"))
   * }}}
   */
  def apply(ids: Vector[FocusId]): FocusManager =
    new FocusManager(ids, ids.headOption)
