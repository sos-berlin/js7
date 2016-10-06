package com.sos.scheduler.engine.data.queries

import com.google.common.base.Splitter
import com.sos.scheduler.engine.base.convert.As
import com.sos.scheduler.engine.base.utils.ScalaUtils._
import scala.collection.JavaConversions._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object CommaSeparated {

  private val CommaSplitter = Splitter.on(',')

  private[queries] def commaSplittedAsSet[A](to: String ⇒ A) = As[String, Set[A]] {
    case "" ⇒ Set()
    case o ⇒ (CommaSplitter split o map to).toSet
  }

  private[queries] def toNamedCommaSeparated[A: ClassTag](name: String, elementsOption: Option[TraversableOnce[A]])(toString: A ⇒ String): Option[(String, String)] =
    for (elements ← elementsOption) yield name → toCommaSeparated(elements)(toString)

  private[queries] def toCommaSeparated[A: ClassTag](elements: TraversableOnce[A])(toString: A ⇒ String): String =
    (for (o ← elements) yield {
      val string = toString(o)
      require(!string.contains(','), s"For this serialization, a ${implicitClass[A]} must not contain a comma ',': '$o'")
      string
    }) mkString ","
}
