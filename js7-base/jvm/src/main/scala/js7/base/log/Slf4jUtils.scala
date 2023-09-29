package js7.base.log

import js7.base.log.LogLevel.{Debug, Error, Info, LogNone, Trace, Warn}
import org.slf4j
import org.slf4j.{LoggerFactory, Marker}

object Slf4jUtils
{
  // Tests only: Try to await slf4j initialization to avoid error messages like this:
  // SLF4J: (logger name here) ...
  // SLF4J: The following set of substitute loggers may have been accessed
  // SLF4J: during the initialization phase. Logging calls during this
  // SLF4J: phase were not honored. However, subsequent logging calls to these
  // SLF4J: loggers will work as normally expected.
  // SLF4J: See also https://www.slf4j.org/codes.html#substituteLogger
  LoggerFactory.getILoggerFactory

  def initialize() = {}

  object syntax {
    implicit final class LevelLogger(private val delegate: slf4j.Logger) extends AnyVal {
      def isEnabled(level: LogLevel): Boolean =
        level match {
          case LogNone => false
          case Trace => delegate.isTraceEnabled
          case Debug => delegate.isDebugEnabled
          case Info => delegate.isInfoEnabled
          case Warn => delegate.isWarnEnabled
          case Error => delegate.isErrorEnabled
        }

      def isEnabled(level: LogLevel, marker: Marker): Boolean =
        level match {
          case LogNone => false
          case Trace => delegate.isTraceEnabled(marker)
          case Debug => delegate.isDebugEnabled(marker)
          case Info => delegate.isInfoEnabled(marker)
          case Warn => delegate.isWarnEnabled(marker)
          case Error => delegate.isErrorEnabled(marker)
        }

      def log(level: LogLevel, message: => String): Unit =
        if isEnabled(level) then level match {
          case LogNone =>
          case Trace => delegate.trace(message)
          case Debug => delegate.debug(message)
          case Info => delegate.info(message)
          case Warn => delegate.warn(message)
          case Error => delegate.error(message)
        }

      def log(level: LogLevel, message: => String, throwable: Throwable): Unit =
        if isEnabled(level) then level match {
          case LogNone =>
          case Trace => delegate.trace(message, throwable)
          case Debug => delegate.debug(message, throwable)
          case Info => delegate.info(message, throwable)
          case Warn => delegate.warn(message, throwable)
          case Error => delegate.error(message, throwable)
        }

      def log(level: LogLevel, marker: Marker, message: => String): Unit =
        if isEnabled(level, marker) then level match {
          case LogNone =>
          case Trace => delegate.trace(marker, message)
          case Debug => delegate.debug(marker, message)
          case Info => delegate.info(marker, message)
          case Warn => delegate.warn(marker, message)
          case Error => delegate.error(marker, message)
        }
    }
  }
}
