package termflow.tui

/** Basic colour palette for text and borders. */
enum Color {
  case Default, Black, Red, Green, Yellow, Blue, Magenta, Cyan, White
}

final case class Style(
  fg: Color = Color.Default,
  bg: Color = Color.Default,
  bold: Boolean = false,
  underline: Boolean = false,
  border: Boolean = false
)

sealed trait VNode {
  def x: XCoord
  def y: YCoord
  def width: Int
  def height: Int
  def style: Style
}

final case class Text(txt: String, style: Style)

final case class TextNode(
  x: XCoord,
  y: YCoord,
  txt: List[Text]
) extends VNode {
  override def width: Int   = 1
  override def height: Int  = 1
  override def style: Style = Style()
}

final case class BoxNode(
  x: XCoord,
  y: YCoord,
  width: Int,
  height: Int,
  children: List[VNode],
  style: Style = Style()
) extends VNode

final case class InputNode(
  x: XCoord,
  y: YCoord,
  prompt: String,
  style: Style,
  cursor: Int = -1,  // -1 means at end
  lineWidth: Int = 0 // 0 means use prompt length
) extends VNode {
  val width: Int  = if (lineWidth > 0) lineWidth else prompt.length + 1
  val height: Int = 1
}

final case class RootNode(
  width: Int,
  height: Int,
  children: List[VNode],
  input: Option[InputNode]
)
