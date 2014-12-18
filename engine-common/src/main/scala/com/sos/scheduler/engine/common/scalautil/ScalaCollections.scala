package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.ScalaUtils._
import scala.collection.{TraversableLike, immutable}
import scala.sys.error

object ScalaCollections {
  implicit class RichTraversable[A](val delegate: Traversable[A]) extends AnyVal {

    def requireDistinct[K](key: A ⇒ K) = {
      duplicates(key) match {
        case o if o.nonEmpty ⇒ error("Unexpected duplicates: "+ o.keys.mkString(", "))
        case _ ⇒
      }
      delegate
    }

    /** Liefert die Duplikate, also Listenelemente, deren Schlüssel mehr als einmal vorkommt. */
    def duplicates[K](key: A ⇒ K) =
      delegate groupBy key filter { _._2.size > 1 }
  }

  implicit class RichPairTraversable[A, B](val delegate: Traversable[(A, B)]) extends AnyVal {
    def toSeqMultiMap: Map[A, immutable.Seq[B]] =
      delegate groupBy { _._1 } map { case (k, v) ⇒ k -> (v map { _._2 }).toImmutableSeq }
  }

  def emptyToNone(o: String): Option[String] =
    if (o.isEmpty) None else Some(o)

  def emptyToNone[A <: TraversableLike[_, _]](o: A): Option[A] =
    if (o.isEmpty) None else Some(o)

  def emptyToNone[A](o: Array[A]): Option[Array[A]] =
    if (o.isEmpty) None else Some(o)

}
