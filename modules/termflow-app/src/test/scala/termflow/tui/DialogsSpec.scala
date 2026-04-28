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

  // ---- Dialogs.textInput ---------------------------------------------------

  test("Dialogs.textInput places the value in an InputNode the dialog owns") {
    val o = Dialogs.textInput(title = "Rename", prompt = "New name:", value = "draft")
    assert(o.inputCapture == InputCapture.Modal)
    assert(o.input.isDefined, "textInput must own its own input")
    val in = o.input.get
    assert(in.prompt == "draft")
    assert(in.cursor == 5, s"default cursor should sit at end, got ${in.cursor}")
  }

  test("Dialogs.textInput pins a fixed prefix at the left of the viewport") {
    val o  = Dialogs.textInput(title = "Path", prompt = "Path:", value = "src", prefix = "> ")
    val in = o.input.get
    assert(in.prompt.startsWith("> "), "prefix should appear in the rendered prompt")
    assert(in.prefixLength == 2, s"prefixLength should match prefix size, got ${in.prefixLength}")
    // Cursor accounts for prefix when no explicit cursor is supplied.
    assert(in.cursor == "> ".length + "src".length)
  }

  test("Dialogs.textInput renders OK/Cancel and respects okFocused") {
    val okFocused     = Dialogs.textInput("Edit", "Text:", value = "")
    val cancelFocused = Dialogs.textInput("Edit", "Text:", value = "", okFocused = false)
    assert(stringForm(okFocused).contains("[ OK ]"))
    assert(stringForm(cancelFocused).contains("[ Cancel ]"))
  }

  test("Dialogs.textInput honours custom labels") {
    val o = Dialogs.textInput("Save?", "File:", value = "x", okLabel = "Save", cancelLabel = "Discard")
    val s = stringForm(o)
    assert(s.contains("Save"))
    assert(s.contains("Discard"))
  }

  // ---- Dialogs.listSelect --------------------------------------------------

  test("Dialogs.listSelect highlights the selected row and renders all items when small") {
    val o = Dialogs.listSelect(
      title = "Pick a fruit",
      items = Seq("apple", "banana", "cherry"),
      selectedIndex = 1
    )
    assert(o.inputCapture == InputCapture.Modal)
    assert(o.input.isEmpty, "listSelect has no live input")
    val s = stringForm(o)
    assert(s.contains("apple"))
    assert(s.contains("banana"))
    assert(s.contains("cherry"))
    assert(s.contains("▸ banana"), s"selected row must carry the marker, got: $s")
  }

  test("Dialogs.listSelect scrolls so the selected row stays visible") {
    val items = (1 to 30).map(i => s"item-$i")
    val o     = Dialogs.listSelect(title = "Long list", items = items, selectedIndex = 25, maxVisible = 6)
    val s     = stringForm(o)
    assert(s.contains("item-25"), s"selected item must be in the rendered viewport, got: $s")
    assert(!s.contains("item-1 ") && !s.contains("item-2 "), "items above the scroll position should not appear")
  }

  test("Dialogs.listSelect with custom render callback formats each row") {
    final case class User(id: Int, name: String)
    val users = Seq(User(1, "alice"), User(2, "bob"))
    val o     = Dialogs.listSelect(title = "User", items = users, selectedIndex = 0, render = u => s"${u.id}:${u.name}")
    val s     = stringForm(o)
    assert(s.contains("1:alice"))
    assert(s.contains("2:bob"))
  }

  test("Dialogs.listSelectLayout exposes the same anchor math listSelect uses") {
    // Empty input: anchor=0, visibleCount=1 (always render at least one row).
    val empty = Dialogs.listSelectLayout(itemsSize = 0, selectedIndex = 0, maxVisible = 5)
    assert(empty.anchorIndex == 0)
    assert(empty.visibleCount == 1)

    // Small list fits entirely in the viewport — anchor stays at 0.
    val small = Dialogs.listSelectLayout(itemsSize = 3, selectedIndex = 1, maxVisible = 5)
    assert(small.anchorIndex == 0)
    assert(small.visibleCount == 3)

    // Long list, selection near the bottom — anchor scrolls to keep it visible.
    val long = Dialogs.listSelectLayout(itemsSize = 30, selectedIndex = 25, maxVisible = 6)
    assert(long.visibleCount == 6)
    assert(long.anchorIndex >= 25 - long.visibleCount + 1, "selection must be inside the visible window")
    assert(long.anchorIndex <= 25, "anchor must not skip past the selection")

    // firstRowOffset is documented as the panel-local Y of the first row;
    // listSelect renders rows at that Y, so the value should match.
    assert(long.firstRowOffset == 3)
  }

  test("Dialogs.listSelect handles an empty item list without exploding") {
    val o = Dialogs.listSelect(title = "Pick", items = Seq.empty[String], selectedIndex = 0)
    assert(o.height >= 5)
    val s = stringForm(o)
    assert(s.contains("Pick"))
    assert(s.contains("OK"))
  }

  // ---- Dialogs.waiting -----------------------------------------------------

  test("Dialogs.waiting picks the spinner frame from the tick (modulo)") {
    val frames = Vector("A", "B", "C")
    val o0     = Dialogs.waiting("Loading", "fetching…", tick = 0L, frames = frames)
    val o1     = Dialogs.waiting("Loading", "fetching…", tick = 1L, frames = frames)
    val o4     = Dialogs.waiting("Loading", "fetching…", tick = 4L, frames = frames) // wraps to "B"
    assert(stringForm(o0).contains("A fetching"))
    assert(stringForm(o1).contains("B fetching"))
    assert(stringForm(o4).contains("B fetching"))
  }

  test("Dialogs.waiting falls back to the default spinner when frames is empty") {
    val o = Dialogs.waiting("Loading", "still working", tick = 0L, frames = Vector.empty)
    val s = stringForm(o)
    assert(Dialogs.defaultSpinnerFrames.exists(f => s.contains(s"$f still working")))
  }

  test("Dialogs.waiting omits the action row by default and renders one when cancelLabel is set") {
    val plain      = Dialogs.waiting("Working", "saving…", tick = 0L)
    val cancelable = Dialogs.waiting("Working", "saving…", tick = 0L, cancelLabel = Some("Stop"))
    assert(plain.height == 5)
    assert(cancelable.height == 7)
    assert(stringForm(cancelable).contains("[ Stop ]"))
  }

  test("Dialogs.waiting with negative tick still picks a valid frame (no exception)") {
    val o = Dialogs.waiting("…", "going backwards", tick = -1L, frames = Vector("X", "Y", "Z"))
    val s = stringForm(o)
    assert(
      s.contains("Z going backwards") || s.contains("Y going backwards") || s.contains("X going backwards"),
      s"expected one of X/Y/Z prefixed frames, got: $s"
    )
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
