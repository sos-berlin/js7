package js7.base.log

object LoggingEscapeCodes:
  @inline val isColorAllowed: Boolean =
    sys.props.get("js7.log.colored").forall(Set("true", "yes", "on", ""))

  /** Reset color and mode. */
  val resetColor = if !isColorAllowed then "" else AnsiEscapeCodes.resetColor
  val bold = if !isColorAllowed then "" else AnsiEscapeCodes.bold
  val black = if !isColorAllowed then "" else AnsiEscapeCodes.black
  val red = if !isColorAllowed then "" else AnsiEscapeCodes.red
  val green = if !isColorAllowed then "" else AnsiEscapeCodes.green
  val blue = if !isColorAllowed then "" else AnsiEscapeCodes.blue
  val magenta = if !isColorAllowed then "" else AnsiEscapeCodes.magenta
  val orange = if !isColorAllowed then "" else AnsiEscapeCodes.orange
