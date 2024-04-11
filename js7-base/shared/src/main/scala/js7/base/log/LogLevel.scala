package js7.base.log

import java.util.Locale
import js7.base.convert.As

/**
  * @author Joacim Zschimmer
  */
enum LogLevel(private val name: String):
  case Trace extends LogLevel("trace")
  case Debug extends LogLevel("debug")
  case Info extends LogLevel("info")
  case Warn extends LogLevel("warn")
  case Error extends LogLevel("error")
  case None extends LogLevel("none")


object LogLevel:
  given Ordering[LogLevel] = Ordering.by(_.ordinal)

  def apply(name: String): LogLevel =
    try LogLevel.valueOf(name.toLowerCase(Locale.ROOT).capitalize)
    catch case _: IllegalArgumentException =>
      throw new IllegalArgumentException(s"Invalid LogLevel: $name")

  implicit val StringAsLogLevel: As[String, LogLevel] =
    As[String, LogLevel](LogLevel.apply)
