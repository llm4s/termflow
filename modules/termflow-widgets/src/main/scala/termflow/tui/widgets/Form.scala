package termflow.tui.widgets

import termflow.tui.*
import termflow.tui.TuiPrelude.*

/**
 * Declarative form layout — a vertical column of labelled rows where
 * each row hosts an arbitrary widget that renders itself based on
 * whether it currently owns focus.
 *
 * Form is presentation-only. Apps own the field state (the current
 * value of each text field, checkbox, button, …) and a
 * [[FocusManager]] that tracks which row owns focus. Form lays out
 * the labels + widgets and asks each row's render callback to produce
 * its `VNode`, passing the focused boolean so the widget renders the
 * appropriate visual.
 *
 * Validation is also app-side: apps decide on submit (typically when a
 * specific button row is activated) whether the field state is valid
 * and what to do next. The `errors` parameter just controls per-row
 * error messages drawn beneath each row.
 *
 * {{{
 * given Theme = Theme.dark
 * val NameId  = FocusId("name")
 * val AgreeId = FocusId("agree")
 * val SaveId  = FocusId("save")
 *
 * Form.column(
 *   rows = Vector(
 *     Form.Row(NameId, "Name:",  focused => widgets.Button(label = nameValue, focused = focused)),
 *     Form.Row(AgreeId, "Agree:", focused => widgets.CheckBox("I accept", agree, focused)),
 *     Form.Row(SaveId, "",       focused => widgets.Button("Save", focused))
 *   ),
 *   focusManager = model.fm,
 *   at           = Coord(2.x, 4.y)
 * )
 * }}}
 *
 * @param id      Stable [[FocusId]] for the row — passed to the
 *                FocusManager for navigation.
 * @param label   Label rendered to the left of the widget. Empty
 *                string yields no label cell, just the widget.
 * @param widget  Render callback receiving `focused: Boolean` and
 *                returning the row's `VNode`.
 * @param height  Cell height the row will occupy. Defaults to `1`.
 */
object Form:

  final case class Row(
    id: FocusId,
    label: String,
    widget: Boolean => VNode,
    height: Int = 1
  )

  /**
   * Lay out a column of [[Row]]s. Returns one or more `VNode`s per row
   * (label TextNode + widget VNode), positioned vertically starting at
   * `at`.
   *
   * @param rows         The rows to render.
   * @param focusManager Source of "is row N focused?".
   * @param at           Top-left of the first row.
   * @param labelWidth   Cells reserved for the left-hand label column.
   *                     Each label is right-padded to this width so the
   *                     widget column lines up. Set to `0` to omit the
   *                     label column entirely.
   * @param gap          Blank rows between fields. Defaults to `0`.
   * @param errors       Optional per-row error messages keyed by
   *                     [[FocusId.value]]. Drawn one row below the
   *                     field in the theme's `error` slot, padding the
   *                     row's effective height by 1.
   */
  def column(
    rows: Vector[Row],
    focusManager: FocusManager,
    at: Coord = Coord(XCoord(1), YCoord(1)),
    labelWidth: Int = 12,
    gap: Int = 0,
    errors: Map[String, String] = Map.empty
  )(using theme: Theme): List[VNode] =
    val builder = List.newBuilder[VNode]
    var row     = at.y.value
    rows.foreach { r =>
      val focused    = focusManager.isFocused(r.id)
      val labelStyle = Style(fg = if focused then theme.primary else theme.foreground, bold = focused)
      // Label column. Skipped when labelWidth = 0 or label is empty.
      if labelWidth > 0 && r.label.nonEmpty then
        val padded = r.label.padTo(labelWidth, ' ').take(labelWidth)
        builder += TextNode(at.x, row.y, List(Text(padded, labelStyle)))
      // Widget rendered at (at.x + labelWidth, row).
      val widgetCol = if labelWidth > 0 then at.x + labelWidth else at.x
      val widget    = r.widget(focused)
      builder += Layout.translate(widget, dx = widgetCol.value - 1, dy = row - 1)

      // Inline error row (one cell below the field) when present.
      errors.get(r.id.value).foreach { msg =>
        val errRow = row + r.height
        builder += TextNode(at.x, errRow.y, List(Text(s"  $msg", Style(fg = theme.error))))
      }

      val errPad = if errors.contains(r.id.value) then 1 else 0
      row = row + r.height + errPad + gap
    }
    builder.result()

  /**
   * Total height the form will occupy with the given rows + gap +
   * error annotations. Useful for sizing a containing panel.
   */
  def totalHeight(
    rows: Vector[Row],
    gap: Int = 0,
    errors: Map[String, String] = Map.empty
  ): Int =
    if rows.isEmpty then 0
    else
      val sum    = rows.map(_.height).sum
      val errPad = rows.count(r => errors.contains(r.id.value))
      val gapPad = (rows.size - 1) * gap
      sum + errPad + gapPad
