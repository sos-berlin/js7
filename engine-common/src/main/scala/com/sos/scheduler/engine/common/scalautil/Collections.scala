package com.sos.scheduler.engine.common.scalautil

import javax.annotation.Nullable
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.{TraversableLike, immutable, mutable}
import scala.util.control.NonFatal
import scala.language.implicitConversions

object Collections {
  object implicits {
    implicit class RichTraversableOnce[A](val delegate: TraversableOnce[A]) extends AnyVal {
      def toImmutableSeq: immutable.Seq[A] =
        delegate match {
          case o: immutable.Seq[A] ⇒ o
          case _ ⇒ Vector() ++ delegate
        }

      def toImmutableIterable: immutable.Iterable[A] =
        delegate match {
          case o: immutable.Iterable[A] ⇒ o
          case _ ⇒ immutable.Iterable() ++ delegate
        }

      def countEquals: Map[A, Int] =
        delegate.toTraversable groupBy identity map { case (k, v) ⇒ k → v.size }

      def compareElementWise(other: TraversableOnce[A])(implicit ordering: Ordering[A]): Int = compareIteratorsElementWise(delegate.toIterator, other.toIterator)
    }

    implicit class RichArray[A](val delegate: Array[A]) extends AnyVal {
      def toImmutableSeq: immutable.Seq[A] =
        Vector() ++ delegate
    }

    implicit class RichJavaIterable[A](val delegate: java.lang.Iterable[A]) extends AnyVal {
      def toImmutableSeq: immutable.Seq[A] =
        Vector() ++ delegate
    }

    implicit class RichJavaIterator[A](val delegate: java.util.Iterator[A]) extends AnyVal {
      def toImmutableSeq: immutable.Seq[A] =
        Vector() ++ delegate
    }

    implicit class RichTraversable[A](val delegate: Traversable[A]) extends AnyVal {
      def toKeyedMap[K](toKey: A ⇒ K): Map[K, A] = (delegate map { o ⇒ toKey(o) → o }).uniqueToMap

      def requireUniqueness[K](key: A ⇒ K) = {
        duplicates(key) match {
          case o if o.nonEmpty ⇒ throw new DuplicateKeyException(s"Unexpected duplicates: ${o.keys mkString ", "}")
          case _ ⇒
        }
        delegate
      }

      /** Liefert die Duplikate, also Listenelemente, deren Schlüssel mehr als einmal vorkommt. */
      def duplicates[K](key: A ⇒ K) =
        delegate groupBy key filter { _._2.size > 1 }
    }

    implicit class RichPairTraversable[A, B](val delegate: Traversable[(A, B)]) extends AnyVal {
      def uniqueToMap = {
        delegate.requireUniqueness { _._1 }
        delegate.toMap
      }

      def toSeqMultiMap: Map[A, immutable.Seq[B]] =
        delegate groupBy { _._1 } map { case (key, seq) ⇒ key → (seq map { _._2 }).toImmutableSeq }
    }

    implicit class InsertableMutableMap[K, V](val delegate: mutable.Map[K, V]) extends AnyVal {
      def insert(kv: (K, V)): Unit = {
        if (delegate contains kv._1) throw new DuplicateKeyException(s"Key ${kv._1} is already known in ${delegate.stringPrefix}")
        delegate += kv
      }
    }

    implicit def javaStreamToIterator[A](stream: java.util.stream.Stream[A]): Iterator[A] = stream.iterator

    implicit class RichJavaStream[A](val delegate: java.util.stream.Stream[A]) extends AnyVal {
      def toImmutableSeq: immutable.Seq[A] = Vector() ++ delegate.iterator
    }
  }

  def emptyToNone(@Nullable o: String): Option[String] =
    if (o == null || o.isEmpty) None else Some(o)

  def emptyToNone[A <: TraversableLike[_, _]](@Nullable o: A): Option[A] =
    if (o == null || o.isEmpty) None else Some(o)

  def emptyToNone[A](@Nullable o: Array[A]): Option[Array[A]] =
    if (o == null || o.isEmpty) None else Some(o)

  @tailrec
  private def compareIteratorsElementWise[A](a: Iterator[A], b: Iterator[A])(implicit ordering: Ordering[A]): Int =
    (a.nonEmpty, b.nonEmpty) match {
      case (false, false) ⇒ 0
      case (true, false) ⇒ +1
      case (false, true) ⇒ -1
      case (true, true) ⇒
        ordering.compare(a.next(), b.next()) match {
          case 0 ⇒ compareIteratorsElementWise(a, b)
          case o ⇒ o
        }
    }
}
