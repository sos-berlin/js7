package js7.base.log

import js7.base.convert.As

/**
  * @author Joacim Zschimmer
  */
sealed trait LogLevel


object LogLevel:
  object LogNone extends LogLevel
  object Trace extends LogLevel
  object Debug extends LogLevel
  object Info extends LogLevel
  object Warn extends LogLevel
  object Error extends LogLevel

  implicit val ordering: Ordering[LogLevel] =
    Ordering.by:
      case LogNone => 0
      case Trace => 1
      case Debug => 2
      case Info => 3
      case Warn => 4
      case Error => 5

  def apply(string: String): LogLevel =
    string.toLowerCase match
      case "none"  => LogNone
      case "trace" => Trace
      case "debug" => Debug
      case "info"  => Info
      case "warn"  => Warn
      case "error" => Error
      case _ => throw new IllegalArgumentException(s"Invalid LogLevel '$string'")

  implicit val StringAsLogLevel: As[String, LogLevel] =
    As[String, LogLevel](LogLevel.apply)
