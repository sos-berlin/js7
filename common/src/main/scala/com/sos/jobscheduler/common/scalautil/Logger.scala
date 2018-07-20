package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.log.ConvertingLogger
import com.typesafe.scalalogging.{Logger ⇒ ScalaLogger}
import org.slf4j.{LoggerFactory, Marker, MarkerFactory}
import scala.reflect.ClassTag

object Logger {

  val Timing: Marker = MarkerFactory.getMarker("timing")
  val Event: Marker = MarkerFactory.getMarker("event")
  val Java: Marker = MarkerFactory.getMarker("java")

  def apply[A: ClassTag]: ScalaLogger =
    apply(implicitClass[A])

  def apply(c: Class[_]): ScalaLogger =
    ScalaLogger(normalizeClassName(c))

  def apply(name: String): ScalaLogger =
    ScalaLogger(name)

  def withPrefix[A: ClassTag](prefix: String): ScalaLogger =
    withPrefix(implicitClass[A], prefix)

  def withPrefix(c: Class[_], prefix: String): ScalaLogger =
    if (prefix.isEmpty)
      apply(c)
    else
      ScalaLogger(new ConvertingLogger.Prefixed(
        prefix,
        LoggerFactory.getLogger(normalizeClassName(c))))

  /** Removes '$' from Scalas companion object class. */
  def normalizeClassName(c: Class[_]): String =
    c.getName stripSuffix "$" replaceFirst("^com[.]sos[.]jobscheduler", "jobscheduler")

  object ops {
    implicit final class RichScalaLogger(private val underlying: ScalaLogger) extends AnyVal
    {
      def error(problem: Problem): Unit =
        problem.throwableOption match {
          case Some(t) ⇒
            underlying.error(problem.toString, t.appendCurrentStackTrace)
          case None ⇒
            underlying.error(problem.toString)
        }
    }
  }
}
