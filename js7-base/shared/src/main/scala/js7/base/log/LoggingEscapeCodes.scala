package js7.base.log

object LoggingEscapeCodes {
  @inline val isColorAllowed: Boolean =
    sys.props.get("js7.log.colored").forall(Set("true", "yes", "on", ""))

  /** Reset color and mode. */
  val resetColor = if (!isColorAllowed) "" else AnsiEscapeCodes.resetColor
  val bold = if (!isColorAllowed) "" else AnsiEscapeCodes.bold
  val black = if (!isColorAllowed) "" else AnsiEscapeCodes.black
  val red = if (!isColorAllowed) "" else AnsiEscapeCodes.red
  val green = if (!isColorAllowed) "" else AnsiEscapeCodes.green
  val blue = if (!isColorAllowed) "" else AnsiEscapeCodes.blue
  val magenta = if (!isColorAllowed) "" else AnsiEscapeCodes.magenta
  val orange = if (!isColorAllowed) "" else AnsiEscapeCodes.orange
}
