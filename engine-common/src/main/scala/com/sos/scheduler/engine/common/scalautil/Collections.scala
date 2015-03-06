package com.sos.scheduler.engine.common.scalautil

import javax.annotation.Nullable
import scala.collection.JavaConversions._
import scala.collection.{TraversableLike, immutable, mutable}
import scala.util.control.NonFatal

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
        delegate groupBy { _._1 } map { case (k, v) ⇒ k → (v map { _._2 }).toImmutableSeq }
    }

    implicit class ConvertingPF[K, V](val delegate: PartialFunction[K, V]) extends AnyVal {
      /**
       * @throws IllegalArgumentException with 'key', wrapping the exception of `delegate`
       */
      def convert[A](key: K)(f: V ⇒ A): A = handleException[A](key, f, delegate(key))

      /**
       * @throws IllegalArgumentException with 'key', wrapping the exception of `delegate`
       */
      def getConverted[A](key: K)(f: V ⇒ A): Option[A] = delegate.lift(key) map { handleException[A](key, f, _) }

      private def handleException[A](key: K, f: V ⇒ A, value: V): A =
        try f(value)
        catch { case NonFatal(t) ⇒ throw new IllegalArgumentException(s"Key '$key': $t", t) }
    }

    implicit class InsertableMutableMap[K, V](val delegate: mutable.Map[K, V]) extends AnyVal {
      def insert(kv: (K, V)): Unit = {
        if (delegate contains kv._1) throw new DuplicateKeyException(s"Key ${kv._1} is already in ${delegate.stringPrefix}")
        delegate += kv
      }
    }

    implicit class RichJavaStream[A](val delegate: java.util.stream.Stream[A]) extends AnyVal {
      def toImmutableSeq: immutable.Seq[A] = toVector
      def toVector: Vector[A] = Vector() ++ delegate.iterator
      def toSet: Set[A] = Set() ++ delegate.iterator
      def toIterator: Iterator[A] = delegate.iterator
    }
  }

  def emptyToNone(@Nullable o: String): Option[String] =
    if (o == null || o.isEmpty) None else Some(o)

  def emptyToNone[A <: TraversableLike[_, _]](@Nullable o: A): Option[A] =
    if (o == null || o.isEmpty) None else Some(o)

  def emptyToNone[A](@Nullable o: Array[A]): Option[Array[A]] =
    if (o == null || o.isEmpty) None else Some(o)
}


