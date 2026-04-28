package termflow.tui.widgets

import org.scalatest.funsuite.AnyFunSuite
import termflow.tui.*
import termflow.tui.TuiPrelude.*

class FormSpec extends AnyFunSuite:

  given Theme = Theme.dark

  private val NameId = FocusId("name")
  private val OkId   = FocusId("ok")
  private val NoteId = FocusId("note")

  private val sampleRows: Vector[Form.Row] = Vector(
    Form.Row(NameId, "Name:", focused => Button("Alice", focused)),
    Form.Row(OkId, "Agree:", focused => Button("OK", focused)),
    Form.Row(NoteId, "", focused => Button("Save", focused))
  )

  // ---- column rendering ---------------------------------------------------

  test("column renders a label TextNode + widget VNode for each row") {
    val fm    = FocusManager(Vector(NameId, OkId, NoteId))
    val nodes = Form.column(sampleRows, fm)
    // 3 rows with labels for the first two: 2 label + 3 widget = 5 nodes.
    assert(nodes.size == 5, s"expected 5 nodes, got ${nodes.size}")
  }

  test("rows with empty label produce only a widget node") {
    val fm    = FocusManager(Vector(NoteId))
    val rows  = Vector(Form.Row(NoteId, "", focused => Button("Save", focused)))
    val nodes = Form.column(rows, fm)
    assert(nodes.size == 1)
  }

  test("rows are positioned vertically with the configured gap") {
    val fm    = FocusManager(Vector(NameId, OkId, NoteId))
    val nodes = Form.column(sampleRows, fm, at = Coord(2.x, 5.y), gap = 1)
    val ys    = nodes.collect { case TextNode(_, y, _) => y.value }
    // The label TextNodes should be at y=5 (Name) and y=7 (Agree, after gap of 1).
    assert(ys.contains(5))
    assert(ys.contains(7))
  }

  test("focused row's widget receives focused = true") {
    val fm                            = FocusManager(Vector(NameId, OkId, NoteId), current = Some(OkId))
    var seen: Set[(FocusId, Boolean)] = Set.empty
    val rows = Vector(
      Form.Row(NameId, "A", focused => { seen += ((NameId, focused)); Button("a", focused) }),
      Form.Row(OkId, "B", focused => { seen += ((OkId, focused)); Button("b", focused) }),
      Form.Row(NoteId, "C", focused => { seen += ((NoteId, focused)); Button("c", focused) })
    )
    Form.column(rows, fm)
    assert(seen == Set((NameId, false), (OkId, true), (NoteId, false)))
  }

  test("error rows are drawn one row beneath the field, in the theme error slot") {
    val fm     = FocusManager(Vector(NameId))
    val rows   = Vector(Form.Row(NameId, "Name:", focused => Button("a", focused)))
    val errors = Map("name" -> "is required")
    val nodes  = Form.column(rows, fm, errors = errors)
    val errNode = nodes.collectFirst {
      case TextNode(_, _, runs) if runs.exists(_.txt.contains("is required")) => runs.head
    }
    assert(errNode.isDefined, "expected an error TextNode")
  }

  test("totalHeight sums row heights, gaps, and error annotations") {
    assert(Form.totalHeight(sampleRows) == 3) // 3 rows of height 1, no gap, no errors
    assert(Form.totalHeight(sampleRows, gap = 1) == 5)
    assert(Form.totalHeight(sampleRows, errors = Map("name" -> "x")) == 4)
    assert(Form.totalHeight(Vector.empty[Form.Row]) == 0)
  }
