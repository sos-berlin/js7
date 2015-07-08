package com.sos.scheduler.engine.common.scalautil

import org.slf4j.{Logger ⇒ Slf4jLogger, LoggerFactory, Marker}

final class Logger private(val delegate: Slf4jLogger, lineToString: String ⇒ String) {

  @inline
  def error(line: ⇒ String): Unit = {
    if (delegate.isErrorEnabled) {
      delegate.error(lineToString(line))
    }
  }

  @inline
  def error(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isErrorEnabled) {
      delegate.error(lineToString(line), t)
    }
  }

  @inline
  def warn(line: ⇒ String): Unit = {
    if (delegate.isWarnEnabled) {
      delegate.warn(lineToString(line))
    }
  }

  @inline
  def warn(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isWarnEnabled) {
      delegate.warn(lineToString(line), t)
    }
  }

  @inline
  def info(line: ⇒ String): Unit = {
    if (delegate.isInfoEnabled) {
      delegate.info(lineToString(line))
    }
  }

  @inline
  def info(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isInfoEnabled) {
      delegate.info(lineToString(line), t)
    }
  }

  @inline
  def info(m: Marker, line: ⇒ String): Unit = {
    if (delegate.isInfoEnabled(m)) {
      delegate.info(m, lineToString(line))
    }
  }

  @inline
  def info(m: Marker, line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isInfoEnabled(m)) {
      delegate.info(m, lineToString(line), t)
    }
  }

  @inline
  def debug(line: ⇒ String): Unit = {
    if (delegate.isDebugEnabled) {
      delegate.debug(lineToString(line))
    }
  }

  @inline
  def debug(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isDebugEnabled) {
      delegate.debug(lineToString(line), t)
    }
  }

  @inline
  def trace(line: ⇒ String): Unit = {
    if (delegate.isTraceEnabled) {
      delegate.trace(lineToString(line))
    }
  }

  @inline
  def trace(line: ⇒ String, t: Throwable): Unit = {
    if (delegate.isTraceEnabled)
      delegate.trace(lineToString(line), t)
  }

  def isErrorEnabled = delegate.isErrorEnabled

  def isWarnEnabled = delegate.isWarnEnabled

  def isInfoEnabled = delegate.isInfoEnabled

  def isDebugEnabled = delegate.isDebugEnabled

  def isTraceEnabled = delegate.isTraceEnabled
}


object Logger {

  def apply(c: Class[_]) = new Logger(LoggerFactory.getLogger(normalizeClassName(c)), identity)

  def withPrefix(c: Class[_], prefix: String) = new Logger(LoggerFactory.getLogger(normalizeClassName(c)), line ⇒ s"($prefix) $line")

  def apply(name: String) = new Logger(LoggerFactory.getLogger(name), identity)

  /** Removes '$' from Scalas companion object class. */
  private def normalizeClassName(c: Class[_]) = c.getName stripSuffix "$"
}
