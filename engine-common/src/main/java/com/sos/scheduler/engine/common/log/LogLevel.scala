package com.sos.scheduler.engine.common.log

import com.typesafe.scalalogging.{Logger ⇒ ScalaLogger}
import org.slf4j

/**
  * @author Joacim Zschimmer
  */
sealed trait LogLevel

object LogLevel {
  object None extends LogLevel
  object Trace extends LogLevel
  object Debug extends LogLevel
  object Info extends LogLevel
  object Warn extends LogLevel
  object Error extends LogLevel

  implicit class LevelLogger(val delegate: slf4j.Logger) extends AnyVal {
    def isEnabled(level: LogLevel): Boolean =
      level match {
        case None  ⇒ false
        case Trace ⇒ delegate.isTraceEnabled
        case Debug ⇒ delegate.isDebugEnabled
        case Info  ⇒ delegate.isInfoEnabled
        case Warn  ⇒ delegate.isWarnEnabled
        case Error ⇒ delegate.isErrorEnabled
      }

    def log(level: LogLevel, message: ⇒ String): Unit = {
      level match {
        case None  ⇒
        case Trace ⇒ delegate.trace(message)
        case Debug ⇒ delegate.debug(message)
        case Info  ⇒ delegate.info(message)
        case Warn  ⇒ delegate.warn(message)
        case Error ⇒ delegate.error(message)
      }
    }
  }

  implicit class LevelScalaLogger(val delegate: ScalaLogger) extends AnyVal {
    def isEnabled(level: LogLevel): Boolean = delegate.underlying.isEnabled(level)

    def log(level: LogLevel, message: ⇒ String): Unit = {
      if (isEnabled(level)) {
        delegate.underlying.log(level, message)
      }
    }
  }
}
