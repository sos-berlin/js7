package js7.base.log

import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.SystemPropertiesExtensions.asSwitch

object LoggingEscapeCodes:

  val isColorAllowed: Boolean =
    sys.props.asSwitch("js7.log.colored", ifMissing = true)

  /** Reset color and mode. */
  val resetColor: String = isColorAllowed ?? AnsiEscapeCodes.resetColor
  val bold: String = isColorAllowed ?? AnsiEscapeCodes.bold
  val black: String = isColorAllowed ?? AnsiEscapeCodes.black
  val red: String = isColorAllowed ?? AnsiEscapeCodes.red
  val green: String = isColorAllowed ?? AnsiEscapeCodes.green
  val blue: String = isColorAllowed ?? AnsiEscapeCodes.blue
  val magenta: String = isColorAllowed ?? AnsiEscapeCodes.magenta
  val orange: String = isColorAllowed ?? AnsiEscapeCodes.orange
