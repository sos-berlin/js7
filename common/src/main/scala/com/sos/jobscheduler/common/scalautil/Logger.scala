package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.common.log.ConvertingLogger
import com.typesafe.scalalogging.{Logger ⇒ ScalaLogger}
import org.slf4j.LoggerFactory
import scala.reflect.ClassTag

object Logger {

  def apply[A: ClassTag]: ScalaLogger =
    apply(implicitClass[A])

  def apply(c: Class[_]): ScalaLogger =
    ScalaLogger(normalizeClassName(c))

  def apply(name: String): ScalaLogger =
    ScalaLogger(name)

  def withPrefix[A: ClassTag](prefix: String): ScalaLogger =
    withClassAndPrefix(implicitClass[A], prefix)

  def withClassAndPrefix(c: Class[_], prefix: String): ScalaLogger =
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
