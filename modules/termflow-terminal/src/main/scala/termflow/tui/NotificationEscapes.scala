package termflow.tui

/**
 * Escape sequences for terminal-attention requests and desktop notifications.
 *
 * Returns `None` for [[NotificationKind.Disabled]] so callers can write
 * conditionally without branching themselves.
 *
 * Two output forms:
 *
 *   - [[attentionFor]] — minimum signal for "this session needs attention".
 *     Emits BEL on every kind except `Disabled`; iTerm2 additionally emits
 *     `OSC 1337 RequestAttention=yes` so the dock icon bounces.
 *   - [[notifyFor]] — desktop notification with title / body for the kinds
 *     that support it; falls back to BEL on [[NotificationKind.BellOnly]].
 */
private[tui] object NotificationEscapes:

  private val BEL: String = ""
  private val ESC: String = ""
  private val ST: String  = ESC + "\\"

  def attentionFor(kind: NotificationKind): Option[String] = kind match
    case NotificationKind.Disabled => None
    case NotificationKind.BellOnly => Some(BEL)
    case NotificationKind.ITerm2   => Some(s"${ESC}]1337;RequestAttention=yes${BEL}${BEL}")
    case NotificationKind.Kitty    => Some(BEL)
    case NotificationKind.Vte      => Some(BEL)

  def notifyFor(kind: NotificationKind, title: String, body: String): Option[String] =
    val t = sanitize(title)
    val b = sanitize(body)
    kind match
      case NotificationKind.Disabled => None
      case NotificationKind.BellOnly => Some(BEL)
      case NotificationKind.ITerm2   =>
        // OSC 9 takes a single message; combine title+body when both are set.
        val msg = if t.isEmpty then b else if b.isEmpty then t else s"$t: $b"
        Some(s"${ESC}]9;$msg${BEL}")
      case NotificationKind.Kitty =>
        // OSC 99 ; metadata ; payload ST. Empty metadata uses defaults.
        // Format the title in the metadata so the notification has a heading.
        val payload = if b.isEmpty then t else b
        val meta    = if t.isEmpty then "" else s"i=1:d=0:p=title;$t"
        Some(s"${ESC}]99;$meta;$payload$ST")
      case NotificationKind.Vte =>
        // OSC 777 ; notify ; <title> ; <body> BEL
        Some(s"${ESC}]777;notify;$t;$b${BEL}")

  /**
   * Strip bytes that would terminate or confuse the OSC envelope: BEL, ESC,
   * and any other C0 control. Semicolons and pipes inside content are kept
   * — terminals tolerate them inside the trailing payload field.
   */
  private def sanitize(s: String): String =
    val out = new StringBuilder(s.length)
    s.foreach(c => if c >= 0x20 && c != 0x7f then out.append(c))
    out.toString
