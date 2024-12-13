package js7.base.log

import js7.base.utils.SystemPropertiesExtensions.asSwitch

object LoggingEscapeCodes:

  val isColorAllowed: Boolean =
    sys.props.asSwitch("js7.log.colored", ifMissing = true)

  /** Reset color and mode. */
  val resetColor: String = if !isColorAllowed then "" else AnsiEscapeCodes.resetColor
  val bold: String = if !isColorAllowed then "" else AnsiEscapeCodes.bold
  val black: String = if !isColorAllowed then "" else AnsiEscapeCodes.black
  val red: String = if !isColorAllowed then "" else AnsiEscapeCodes.red
  val green: String = if !isColorAllowed then "" else AnsiEscapeCodes.green
  val blue: String = if !isColorAllowed then "" else AnsiEscapeCodes.blue
  val magenta: String = if !isColorAllowed then "" else AnsiEscapeCodes.magenta
  val orange: String = if !isColorAllowed then "" else AnsiEscapeCodes.orange
