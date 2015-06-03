package com.sos.scheduler.engine.common.scalautil

import org.slf4j.{Logger ⇒ Slf4jLogger, LoggerFactory, Marker}

final class Logger(val delegate: Slf4jLogger) extends AnyVal {

  @inline
  def error(line: ⇒ String): Unit = {
    if (delegate.isErrorEnabled) {
      delegate.error(line)
    }
  }

  @inline
  def error(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isErrorEnabled) {
      delegate.error(line, t)
    }
  }

  @inline
  def warn(line: ⇒ String): Unit = {
    if (delegate.isWarnEnabled) {
      delegate.warn(line)
    }
  }

  @inline
  def warn(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isWarnEnabled) {
      delegate.warn(line, t)
    }
  }

  @inline
  def info(line: ⇒ String): Unit = {
    if (delegate.isInfoEnabled) {
      delegate.info(line)
    }
  }

  @inline
  def info(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isInfoEnabled) {
      delegate.info(line, t)
    }
  }

  @inline
  def info(m: Marker, line: ⇒ String): Unit = {
    if (delegate.isInfoEnabled(m)) {
      delegate.info(m, line)
    }
  }

  @inline
  def info(m: Marker, line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isInfoEnabled(m)) {
      delegate.info(m, line, t)
    }
  }

  @inline
  def debug(line: ⇒ String): Unit = {
    if (delegate.isDebugEnabled) {
      delegate.debug(line)
    }
  }

  @inline
  def debug(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isDebugEnabled) {
      delegate.debug(line, t)
    }
  }

  @inline
  def trace(line: ⇒ String): Unit = {
    if (delegate.isTraceEnabled) {
      delegate.trace(line)
    }
  }

  @inline
  def trace(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isTraceEnabled)
      delegate.trace(line, t)
  }

  def isErrorEnabled = delegate.isErrorEnabled

  def isWarnEnabled = delegate.isWarnEnabled

  def isInfoEnabled = delegate.isInfoEnabled

  def isDebugEnabled = delegate.isDebugEnabled

  def isTraceEnabled = delegate.isTraceEnabled
}


object Logger {

  def apply(c: Class[_]) =
    new Logger(LoggerFactory.getLogger(normalizeClassName(c)))

  def apply(name: String) =
    new Logger(LoggerFactory.getLogger(name))

  /** Removes '$' from Scalas companion object class. */
  private def normalizeClassName(c: Class[_]) =
    c.getName stripSuffix "$"
}
