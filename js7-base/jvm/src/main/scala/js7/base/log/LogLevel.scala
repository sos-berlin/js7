package js7.base.log

import com.typesafe.scalalogging.Logger as ScalaLogger
import js7.base.convert.As
import org.slf4j
import org.slf4j.Marker

/**
  * @author Joacim Zschimmer
  */
sealed trait LogLevel

object LogLevel
{
  object LogNone extends LogLevel
  object Trace extends LogLevel
  object Debug extends LogLevel
  object Info extends LogLevel
  object Warn extends LogLevel
  object Error extends LogLevel

  def apply(string: String): LogLevel =
    string.toLowerCase match {
      case "none"  => LogNone
      case "trace" => Trace
      case "debug" => Debug
      case "info"  => Info
      case "warn"  => Warn
      case "error" => Error
      case _ => throw new IllegalArgumentException(s"Invalid LogLevel '$string'")
    }

  implicit val StringAsLogLevel = As[String, LogLevel](LogLevel.apply)

  object syntax
  {
    implicit final class LevelLogger(private val delegate: slf4j.Logger) extends AnyVal {
      def isEnabled(level: LogLevel): Boolean =
        level match {
          case LogNone  => false
          case Trace => delegate.isTraceEnabled
          case Debug => delegate.isDebugEnabled
          case Info  => delegate.isInfoEnabled
          case Warn  => delegate.isWarnEnabled
          case Error => delegate.isErrorEnabled
        }

      def isEnabled(level: LogLevel, marker: Marker): Boolean =
        level match {
          case LogNone  => false
          case Trace => delegate.isTraceEnabled(marker)
          case Debug => delegate.isDebugEnabled(marker)
          case Info  => delegate.isInfoEnabled(marker)
          case Warn  => delegate.isWarnEnabled(marker)
          case Error => delegate.isErrorEnabled(marker)
        }

      def log(level: LogLevel, message: => String): Unit =
        level match {
          case LogNone  =>
          case Trace => delegate.trace(message)
          case Debug => delegate.debug(message)
          case Info  => delegate.info(message)
          case Warn  => delegate.warn(message)
          case Error => delegate.error(message)
        }

      def log(level: LogLevel, message: => String, throwable: Throwable): Unit =
        level match {
          case LogNone  =>
          case Trace => delegate.trace(message, throwable)
          case Debug => delegate.debug(message, throwable)
          case Info  => delegate.info(message, throwable)
          case Warn  => delegate.warn(message, throwable)
          case Error => delegate.error(message, throwable)
        }

      def log(level: LogLevel, marker: Marker, message: => String): Unit =
        level match {
          case LogNone  =>
          case Trace => delegate.trace(marker, message)
          case Debug => delegate.debug(marker, message)
          case Info  => delegate.info(marker, message)
          case Warn  => delegate.warn(marker, message)
          case Error => delegate.error(marker, message)
        }
    }

    implicit final class LevelScalaLogger(private val delegate: ScalaLogger) extends AnyVal {
      def isEnabled(level: LogLevel): Boolean = delegate.underlying.isEnabled(level)

      def log(level: LogLevel, message: => String): Unit =
        if (isEnabled(level)) {
          delegate.underlying.log(level, message)
        }

      def log(level: LogLevel, message: => String, throwable: Throwable): Unit =
        if (isEnabled(level)) {
          delegate.underlying.log(level, message, throwable)
        }

      def log(level: LogLevel, marker: Marker, message: => String): Unit =
        if (isEnabled(level)) {
          delegate.underlying.log(level, marker, message)
        }
    }
  }
}
