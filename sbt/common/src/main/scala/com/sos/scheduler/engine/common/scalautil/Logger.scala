package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.log.ConvertingLogger
import com.typesafe.scalalogging.{Logger ⇒ ScalaLogger}
import org.slf4j.LoggerFactory

object Logger {

  def apply(c: Class[_]): ScalaLogger =
    ScalaLogger(normalizeClassName(c))

  def apply(name: String): ScalaLogger =
    ScalaLogger(name)

  def withPrefix(c: Class[_], prefix: String): ScalaLogger =
    if (prefix.isEmpty)
      apply(c)
    else
      ScalaLogger(new ConvertingLogger.Prefixed(
        prefix,
        LoggerFactory.getLogger(normalizeClassName(c))))

  /** Removes '$' from Scalas companion object class. */
  def normalizeClassName(c: Class[_]): String =
    c.getName stripSuffix "$"
}
