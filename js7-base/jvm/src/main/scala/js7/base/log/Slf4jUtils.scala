package js7.base.log

import org.slf4j
import org.slf4j.{LoggerFactory, Marker}

object Slf4jUtils:
  // Tests only: Try to await slf4j initialization to avoid error messages like this:
  // SLF4J: (logger name here) ...
  // SLF4J: The following set of substitute loggers may have been accessed
  // SLF4J: during the initialization phase. Logging calls during this
  // SLF4J: phase were not honored. However, subsequent logging calls to these
  // SLF4J: loggers will work as normally expected.
  // SLF4J: See also https://www.slf4j.org/codes.html#substituteLogger
  LoggerFactory.getILoggerFactory

  def initialize(): Unit = {}

  object syntax:
    implicit final class LevelLogger(private val delegate: slf4j.Logger) extends AnyVal:
      def isEnabled(level: LogLevel): Boolean =
        level match
          case LogLevel.None => false
          case LogLevel.Trace => delegate.isTraceEnabled
          case LogLevel.Debug => delegate.isDebugEnabled
          case LogLevel.Info => delegate.isInfoEnabled
          case LogLevel.Warn => delegate.isWarnEnabled
          case LogLevel.Error => delegate.isErrorEnabled

      def isEnabled(level: LogLevel, marker: Marker): Boolean =
        level match
          case LogLevel.None => false
          case LogLevel.Trace => delegate.isTraceEnabled(marker)
          case LogLevel.Debug => delegate.isDebugEnabled(marker)
          case LogLevel.Info => delegate.isInfoEnabled(marker)
          case LogLevel.Warn => delegate.isWarnEnabled(marker)
          case LogLevel.Error => delegate.isErrorEnabled(marker)

      def log(level: LogLevel, message: => String): Unit =
        if isEnabled(level) then level match
          case LogLevel.None =>
          case LogLevel.Trace => delegate.trace(message)
          case LogLevel.Debug => delegate.debug(message)
          case LogLevel.Info => delegate.info(message)
          case LogLevel.Warn => delegate.warn(message)
          case LogLevel.Error => delegate.error(message)

      def log(level: LogLevel, message: => String, throwable: Throwable): Unit =
        if isEnabled(level) then level match
          case LogLevel.None =>
          case LogLevel.Trace => delegate.trace(message, throwable)
          case LogLevel.Debug => delegate.debug(message, throwable)
          case LogLevel.Info => delegate.info(message, throwable)
          case LogLevel.Warn => delegate.warn(message, throwable)
          case LogLevel.Error => delegate.error(message, throwable)

      def log(level: LogLevel, marker: Marker, message: => String): Unit =
        if isEnabled(level, marker) then level match
          case LogLevel.None =>
          case LogLevel.Trace => delegate.trace(marker, message)
          case LogLevel.Debug => delegate.debug(marker, message)
          case LogLevel.Info => delegate.info(marker, message)
          case LogLevel.Warn => delegate.warn(marker, message)
          case LogLevel.Error => delegate.error(marker, message)
