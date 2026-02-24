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

final case class Text(txt: String, style: Style)

enum VNode {
  case TextNode(
    override val x: XCoord,
    override val y: YCoord,
    txt: List[Text]
  )
  case BoxNode(
    override val x: XCoord,
    override val y: YCoord,
    override val width: Int,
    override val height: Int,
    children: List[VNode],
    override val style: Style = Style()
  )
  case InputNode(
    override val x: XCoord,
    override val y: YCoord,
    prompt: String,
    override val style: Style,
    cursor: Int = -1,  // -1 means at end
    lineWidth: Int = 0 // 0 means use prompt length
  )

  def x: XCoord = this match {
    case TextNode(x, _, _)           => x
    case BoxNode(x, _, _, _, _, _)   => x
    case InputNode(x, _, _, _, _, _) => x
  }

  def y: YCoord = this match {
    case TextNode(_, y, _)           => y
    case BoxNode(_, y, _, _, _, _)   => y
    case InputNode(_, y, _, _, _, _) => y
  }

  def width: Int = this match {
    case TextNode(_, _, _)             => 1
    case BoxNode(_, _, width, _, _, _) => width
    case InputNode(_, _, prompt, _, _, lineWidth) =>
      if (lineWidth > 0) lineWidth else prompt.length + 1
  }

  def height: Int = this match {
    case TextNode(_, _, _)              => 1
    case BoxNode(_, _, _, height, _, _) => height
    case InputNode(_, _, _, _, _, _)    => 1
  }

  def style: Style = this match {
    case TextNode(_, _, _)               => Style()
    case BoxNode(_, _, _, _, _, style)   => style
    case InputNode(_, _, _, style, _, _) => style
  }
}

// Compatibility aliases so call sites can keep using TextNode/BoxNode/InputNode directly.
type TextNode = VNode.TextNode
val TextNode = VNode.TextNode
type BoxNode = VNode.BoxNode
val BoxNode = VNode.BoxNode
type InputNode = VNode.InputNode
val InputNode = VNode.InputNode

final case class RootNode(
  width: Int,
  height: Int,
  children: List[VNode],
  input: Option[InputNode]
)
