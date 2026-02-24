package termflow.run.jline

object AsciiTables {

  def printAsciiTable(args: Array[String]): Unit = {
    val _ = args
    println("ASCII Table (0–127):")
    for (i <- 0 to 127) {
      val ch = i.toChar
      val display =
        if (ch.isControl) f"\\u${i}%04x" // escape control chars
        else s"'$ch'"
      println(f"$i%3d -> $display")
    }
  }

  def fullAsciiTable(args: Array[String]): Unit = {
    val _ = args
    // Control character names (0–31 + 127)
    val controlChars: Map[Int, String] = Map(
      0   -> "NUL (Null)",
      1   -> "SOH (Start of Heading)",
      2   -> "STX (Start of Text)",
      3   -> "ETX (End of Text)",
      4   -> "EOT (End of Transmission)",
      5   -> "ENQ (Enquiry)",
      6   -> "ACK (Acknowledge)",
      7   -> "BEL (Bell, \\a)",
      8   -> "BS  (Backspace, \\b)",
      9   -> "TAB (Horizontal Tab, \\t)",
      10  -> "LF  (Line Feed / Newline, \\n)",
      11  -> "VT  (Vertical Tab, \\v)",
      12  -> "FF  (Form Feed, \\f)",
      13  -> "CR  (Carriage Return, \\r)",
      14  -> "SO  (Shift Out)",
      15  -> "SI  (Shift In)",
      16  -> "DLE (Data Link Escape)",
      17  -> "DC1 (Device Control 1 / XON)",
      18  -> "DC2 (Device Control 2)",
      19  -> "DC3 (Device Control 3 / XOFF)",
      20  -> "DC4 (Device Control 4)",
      21  -> "NAK (Negative Acknowledge)",
      22  -> "SYN (Synchronous Idle)",
      23  -> "ETB (End of Transmission Block)",
      24  -> "CAN (Cancel)",
      25  -> "EM  (End of Medium)",
      26  -> "SUB (Substitute)",
      27  -> "ESC (Escape, \\u001b)",
      28  -> "FS  (File Separator)",
      29  -> "GS  (Group Separator)",
      30  -> "RS  (Record Separator)",
      31  -> "US  (Unit Separator)",
      127 -> "DEL (Delete)"
    )

    println("Full ASCII Table (0–127):\n")
    println("Code | Char | Description")
    println("-----+------+-----------------------------")

    for (i <- 0 to 127) {
      val desc =
        if (controlChars.contains(i)) controlChars(i)
        else s"'${i.toChar}'"
      val printable = if (i >= 32 && i <= 126) i.toChar.toString else ""
      println(f"$i%4d | $printable%-4s | $desc")
    }
  }
}
