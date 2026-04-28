package termflow.tui

/**
 * Position of an [[Overlay]] relative to the parent root frame.
 *
 * Resolved to absolute `(x, y)` coordinates by the renderer based on the
 * overlay's own width/height and the root's frame dimensions, so apps don't
 * have to recompute positions on resize.
 */
enum OverlayPosition:
  /** Centred horizontally and vertically inside the root frame. */
  case Centered

  /** Pinned to the top-left corner with a small inset. */
  case TopLeft

  /** Pinned to the top-right corner with a small inset. */
  case TopRight

  /** Pinned to the bottom-left corner with a small inset. */
  case BottomLeft

  /** Pinned to the bottom-right corner with a small inset. */
  case BottomRight

  /** Anchored at an explicit `(x, y)`. The simple escape hatch. */
  case At(x: XCoord, y: YCoord)

object OverlayPosition:
  /** Standard inset (1 cell from each edge) used by the corner positions. */
  val cornerInset: Int = 1

  /**
   * Resolve a position to an absolute top-left `(x, y)` for an overlay of
   * the given `(width, height)` rendered inside `(rootWidth, rootHeight)`.
   *
   * Off-screen results are clamped so an oversized overlay stays anchored
   * at `(1, 1)` rather than producing negative coordinates.
   */
  def resolve(
    pos: OverlayPosition,
    overlayWidth: Int,
    overlayHeight: Int,
    rootWidth: Int,
    rootHeight: Int
  ): (XCoord, YCoord) =
    val (rawX, rawY) = pos match
      case Centered =>
        val cx = ((rootWidth - overlayWidth) / 2) + 1
        val cy = ((rootHeight - overlayHeight) / 2) + 1
        (cx, cy)
      case TopLeft =>
        (1 + cornerInset, 1 + cornerInset)
      case TopRight =>
        ((rootWidth - overlayWidth - cornerInset) + 1, 1 + cornerInset)
      case BottomLeft =>
        (1 + cornerInset, (rootHeight - overlayHeight - cornerInset) + 1)
      case BottomRight =>
        ((rootWidth - overlayWidth - cornerInset) + 1, (rootHeight - overlayHeight - cornerInset) + 1)
      case At(x, y) =>
        (x.value, y.value)
    val clampedX = math.max(1, rawX)
    val clampedY = math.max(1, rawY)
    (XCoord(clampedX), YCoord(clampedY))

/**
 * How an [[Overlay]] interacts with input directed at the underlying frame.
 *
 * The renderer enforces visual stacking unconditionally; input semantics are
 * the caller's responsibility — apps decide based on the overlay's
 * `inputCapture` whether to ignore base-view key events while the overlay is
 * up. The renderer suppresses the base view's [[InputNode]] when any overlay
 * sets `inputCapture = Modal`, so the hardware cursor follows the dialog.
 */
enum InputCapture:
  /** Visual only; base view continues to own the cursor and receive input. */
  case None

  /** Base view's input is suppressed while this overlay is shown. */
  case Modal

/**
 * A positioned, sized rectangle of children rendered on top of the base view.
 *
 * Use overlays for transient UI that doesn't reflow the base view: confirm
 * dialogs, command palettes, autocompletion popups, devtools panes. The base
 * view's `view` function does not need to know an overlay is up — apps
 * compose `RootNode(... overlays = List(...))` from their model state.
 *
 * Overlays inside a `RootNode.overlays` list are drawn bottom-to-top in
 * document order, so later entries paint over earlier ones.
 *
 * @param position     Where to anchor the overlay's top-left.
 * @param width        Cell width.
 * @param height       Cell height.
 * @param children     Child VNodes. Coordinates are local to the overlay's
 *                     own top-left when relative-coord layout lands; until
 *                     then they are absolute (1-based) just like
 *                     [[RootNode.children]].
 * @param input        Optional input the overlay owns. Suppresses base
 *                     view's input under [[InputCapture.Modal]].
 * @param inputCapture Whether base-view input is suppressed.
 */
final case class Overlay(
  position: OverlayPosition,
  width: Int,
  height: Int,
  children: List[VNode],
  input: Option[InputNode] = None,
  inputCapture: InputCapture = InputCapture.None
)
