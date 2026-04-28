package termflow.tui

/**
 * A rectangular region on the terminal grid (1-based, inclusive).
 *
 * Used by the [[HitTest]] cache to map a `(col, row)` mouse coordinate
 * back to the layout zone it landed in.
 *
 * @param x      Top-left column (1-based).
 * @param y      Top-left row (1-based).
 * @param width  Width in cells. `width <= 0` makes the rect empty —
 *               `contains` will always return false.
 * @param height Height in cells. Same `<= 0` rule.
 */
final case class Rect(x: Int, y: Int, width: Int, height: Int):

  /** Right edge column, inclusive. Equal to `x + width - 1`. */
  def right: Int = x + math.max(0, width) - 1

  /** Bottom edge row, inclusive. Equal to `y + height - 1`. */
  def bottom: Int = y + math.max(0, height) - 1

  /** True when `(col, row)` lies inside this rectangle, edges inclusive. */
  def contains(col: Int, row: Int): Boolean =
    width > 0 && height > 0 &&
      col >= x && col <= right &&
      row >= y && row <= bottom

  /** Top-left corner as a [[Coord]]. */
  def at: Coord = Coord(XCoord(x), YCoord(y))

object Rect:

  /** An empty rectangle (`contains` always false). */
  val empty: Rect = Rect(0, 0, 0, 0)

  /** Build a rectangle from a [[Coord]] origin and `(width, height)`. */
  def at(origin: Coord, width: Int, height: Int): Rect =
    Rect(origin.x.value, origin.y.value, width, height)

/**
 * A registry of named rectangles produced by the layout pass, used to
 * map mouse coordinates to the widget / zone the click landed in.
 *
 * Zones are registered in document order. [[hit]] searches in **reverse
 * insertion order** — later (z-top) zones win over earlier ones. Apps
 * pair this with `InputKey.Mouse(...)` events:
 *
 * {{{
 * msg match
 *   case Msg.Mouse(MouseEvent.Press(_, col, row, _)) =>
 *     model.hitTest.hit(col, row) match
 *       case Some("submit-button") => submit(model)
 *       case Some(s"list-row-$idx") => selectRow(model, idx.toInt)
 *       case _ => model.tui
 * }}}
 *
 * Apps drive how zones are populated: either explicitly via
 * [[HitTest.add]] when constructing the view, or implicitly via
 * [[Layout.Zone]] which registers the resolved rect during layout.
 *
 * Type parameter `Id` is the zone identifier — strings are the most
 * common, but apps preferring an enum or a sealed ADT for compile-time
 * exhaustiveness can use that instead.
 */
final case class HitTest[Id](zones: Vector[(Id, Rect)]):

  /** Append a zone to the registry. Later zones win in [[hit]]. */
  def add(id: Id, rect: Rect): HitTest[Id] =
    HitTest(zones :+ (id, rect))

  /** Append a zone using a [[Coord]] origin and `(width, height)`. */
  def add(id: Id, origin: Coord, width: Int, height: Int): HitTest[Id] =
    add(id, Rect.at(origin, width, height))

  /** Append all zones from `other`, after this registry's existing entries. */
  def ++(other: HitTest[Id]): HitTest[Id] =
    HitTest(zones ++ other.zones)

  /**
   * Return the id of the topmost zone containing `(col, row)`, or
   * `None` if no registered zone covers the cell.
   */
  def hit(col: Int, row: Int): Option[Id] =
    zones.reverseIterator.collectFirst {
      case (id, rect) if rect.contains(col, row) => id
    }

  /** Number of registered zones. */
  def size: Int = zones.size

  /** True when no zones are registered. */
  def isEmpty: Boolean = zones.isEmpty

object HitTest:

  /** An empty hit-test registry. */
  def empty[Id]: HitTest[Id] = HitTest[Id](Vector.empty)

  /** Build a registry from a sequence of `(id, rect)` pairs. */
  def of[Id](zones: (Id, Rect)*): HitTest[Id] =
    HitTest(zones.toVector)
