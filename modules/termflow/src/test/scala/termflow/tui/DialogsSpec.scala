package termflow.tui

import org.scalatest.funsuite.AnyFunSuite

class DialogsSpec extends AnyFunSuite:

  given Theme = Theme.dark

  test("Dialogs.confirm produces a centred modal overlay") {
    val o = Dialogs.confirm("Delete file?")
    assert(o.position == OverlayPosition.Centered)
    assert(o.inputCapture == InputCapture.Modal)
    assert(o.input.isEmpty)
    assert(o.width >= 40)
    assert(o.height >= 7)
  }

  test("Dialogs.confirm respects yesFocused for button highlight") {
    val noFocused  = Dialogs.confirm("Sure?", yesFocused = false)
    val yesFocused = Dialogs.confirm("Sure?", yesFocused = true)
    assert(stringForm(noFocused).contains("[ No ]"))
    assert(stringForm(yesFocused).contains("[ Yes ]"))
  }

  test("Dialogs.confirm honours custom labels and title") {
    val o = Dialogs.confirm(
      "Continue?",
      title = "Are you sure",
      yesLabel = "Proceed",
      noLabel = "Abort"
    )
    val s = stringForm(o)
    assert(s.contains("Are you sure"))
    assert(s.contains("Proceed"))
    assert(s.contains("Abort"))
  }

  test("Dialogs.message wraps title, body lines, and choices") {
    val o = Dialogs.message(
      title = "Update available",
      body = List("A new version is ready.", "Install now?"),
      choices = List(Dialogs.Choice("Later", focused = false), Dialogs.Choice("Install", focused = true))
    )
    val s = stringForm(o)
    assert(s.contains("Update available"))
    assert(s.contains("A new version is ready."))
    assert(s.contains("Install now?"))
    assert(s.contains("Later"))
    assert(s.contains("[ Install ]"))
  }

  test("a confirm dialog actually composites as a modal over a base RootNode") {
    val baseRoot = RootNode(
      width = 80,
      height = 24,
      children = List(TextNode(XCoord(2), YCoord(2), List(Text("background", Style())))),
      input = Some(InputNode(XCoord(2), YCoord(20), prompt = "x", style = Style(), cursor = 1))
    )
    val withDialog = baseRoot.copy(overlays = List(Dialogs.confirm("OK to proceed?")))
    val frame      = AnsiRenderer.buildFrame(withDialog)
    val rendered   = (0 until frame.height).map(r => frame.cells(r).map(_.ch).mkString).mkString("\n")
    assert(rendered.contains("OK to proceed?"))
    assert(frame.cursor.isEmpty, "modal dialog must take the cursor away from the base view")
  }

  // ---- helper: walk an overlay's tree into a flat string for assertions ---

  private def stringForm(o: Overlay): String =
    val sb = new StringBuilder
    o.children.foreach(walk(_, sb))
    o.input.foreach(in => sb.append(in.prompt))
    sb.toString

  private def walk(v: VNode, sb: StringBuilder): Unit = v match
    case TextNode(_, _, runs)                => runs.foreach(t => sb.append(t.txt))
    case BoxNode(_, _, _, _, children, _, _) => children.foreach(walk(_, sb))
    case _                                   => ()
