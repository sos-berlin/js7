package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import javax.annotation.Nullable
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.{Seq, Vector, VectorBuilder}
import scala.collection.{BufferedIterator, immutable, mutable}
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

object Collections {
  object implicits {
    implicit final class RichIndexedSeq[A](private val underlying: IndexedSeq[A]) extends AnyVal {
      def get(i: Int): Option[A] =
        if (underlying.indices contains i) Some(underlying(i)) else None
    }

    implicit final class RichTraversableOnce[A](private val delegate: IterableOnce[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        delegate match {
          case o: Seq[A] => o
          case _ => Vector.empty ++ delegate
        }

      def toImmutableIterable: immutable.Iterable[A] =
        delegate match {
          case o: immutable.Iterable[A] => o
          case _ => immutable.Iterable() ++ delegate
        }

      def countEquals: Map[A, Int] =
        delegate.toTraversable groupBy identity map { case (k, v) => k -> v.size }

      def compareElementWise(other: IterableOnce[A])(implicit ordering: Ordering[A]): Int = compareIteratorsElementWise(delegate.toIterator, other.toIterator)
    }

    implicit final class RichSeq[A](private val delegate: collection.Iterable[A]) extends AnyVal {
      /**
        * Like `fold` but uses `neutral` only when `operation` cannot be applied, that is size &lt; 2.
        *
        * @param neutral is assumed to be a real neutral element
        */
      def foldFast(neutral: A)(operation: (A, A) => A): A = delegate match {
        case Nil => neutral
        case collection.Seq(a) => a
        case seq => seq reduce operation
      }
    }

    implicit final class RichArray[A](private val delegate: Array[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        Vector.empty ++ delegate
    }

    implicit final class RichJavaIterable[A](private val delegate: java.lang.Iterable[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        Vector.empty ++ delegate.asScala
    }

    implicit final class RichJavaIterator[A](private val delegate: java.util.Iterator[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        Vector.empty ++ delegate.asScala
    }

    implicit final class RichTraversable[F[x] <: Iterable[x], A](private val delegate: F[A]) extends AnyVal {
      def toKeyedMap[K](toKey: A => K): Map[K, A] = (delegate map { o => toKey(o) -> o }).uniqueToMap

      def toKeyedMap[K](toKey: A => K, ifNot: Iterable[K] => Nothing): Map[K, A] = (delegate map { o => toKey(o) -> o }).uniqueToMap(ifNot)

      def uniqueToSet: Set[A] =
        delegate.requireUniqueness.toSet

      def checkUniqueness: Checked[F[A]] =
        checkUniqueness(identity[A])

      def checkUniqueness[K](key: A => K): Checked[F[A]] =
        duplicateKeys(key) match {
          case Some(duplicates) => Left(Problem(s"Unexpected duplicates: ${duplicates.keys mkString ", "}"))
          case None => Right(delegate)
        }

      def requireUniqueness: Iterable[A] =
        requireUniqueness(identity[A])

      def requireUniqueness[K](key: A => K): Iterable[A] =
        ifNotUnique[K, A](key, keys => throw new DuplicateKeyException(s"Unexpected duplicates: ${keys mkString ", "}"))

      def ifNotUnique[K, B >: A](key: A => K, then_ : Iterable[K] => Iterable[B]): Iterable[B] =
        duplicateKeys(key) match {
          case Some(o) => then_(o.keys)
          case None => delegate
        }

      def duplicates: Iterable[A] =
        delegate groupBy identity collect { case (k, v) if v.size > 1 => k }

      /** Liefert die Duplikate, also Listenelemente, deren Schlüssel mehr als einmal vorkommt. */
      def duplicateKeys[K](key: A => K): Option[Map[K, Iterable[A]]] =
        delegate groupBy key filter { _._2.size > 1 } match {
          case o if o.isEmpty => None
          case o => Some(o)
        }

      /**
        * Like `groupBy`, but returns a `Vector[(K, Vector[A])] ` retaining the original key order (of every first occurrence),
        */
      def retainOrderGroupBy[K](toKey: A => K): Vector[(K, Vector[A])] = {
        val m = mutable.LinkedHashMap[K, immutable.VectorBuilder[A]]()
        for (elem <- delegate) {
          m.getOrElseUpdate(toKey(elem), new immutable.VectorBuilder[A]) += elem
        }
        val b = Vector.newBuilder[(K, Vector[A])]
        b.sizeHint(m.size)
        for ((k, vectorBuilder) <- m) {
          b += ((k, vectorBuilder.result))
        }
        b.result
      }
    }

    implicit final class RichIterator[A](private val underlying: Iterator[A]) extends AnyVal {
      def takeWhileAndOne(predicate: A => Boolean): Iterator[A] = {
        var firstNonMatching: Iterator[A] = Iterator.empty
        underlying.takeWhile { a =>
          val p = predicate(a)
          if (!p) firstNonMatching = Iterator.single(a)
          p
        } ++ firstNonMatching
      }
    }

    implicit final class RichSet[A](private val delegate: Set[A]) extends AnyVal {
      def isDisjointWith(o: Set[A]): Boolean =
        delegate forall (k => !o.contains(k))
    }

    implicit final class RichPairTraversable[A, B](private val delegate: Iterable[(A, B)]) extends AnyVal {
      def uniqueToMap: Map[A, B] = {
        delegate.requireUniqueness { _._1 }
        delegate.toMap
      }

      def uniqueToMap(ifNot: Iterable[A] => Nothing): Map[A, B] = {
        delegate.ifNotUnique(_._1, ifNot)
        delegate.toMap
      }

      def toSeqMultiMap: Map[A, Seq[B]] =
        delegate groupBy { _._1 } map { case (key, seq) => key -> (seq map { _._2 }).toImmutableSeq }
    }

    implicit final class InsertableMutableMap[K, V](private val delegate: mutable.Map[K, V]) extends AnyVal {
      def insert(kv: (K, V)): Unit = {
        if (delegate contains kv._1) throw new DuplicateKeyException(s"Key ${kv._1} is already known")
        delegate += kv
      }
    }
  }

  implicit final class RichVectorCompanion(private val underlying: Vector.type) extends AnyVal {
    @deprecated
    def build[A](body: VectorBuilder[A] => Unit): Vector[A] = {
      val b = new VectorBuilder[A]
      body(b)
      b.result
    }
  }

  implicit final class RichArrayCompanion(private val underlying: Array.type) extends AnyVal {
    @deprecated
    def build[A: ClassTag](body: mutable.ArrayBuilder[A] => Unit): Array[A] = {
      val b = mutable.ArrayBuilder.make[A]
      body(b)
      b.result()
    }
  }

  implicit final class RichMap[K, V](private val underlying: Map[K, V]) extends AnyVal {
    def toChecked(unknownKey: K => Problem): Map[K, Checked[V]] =
      underlying.mapValues(Right.apply).toMap withDefault unknownKey.andThen(Left.apply)

    def withNoSuchKey(noSuchKey: K => Nothing): Map[K, V] =
      underlying withDefault (k => noSuchKey(k))

    def mapValuesStrict[W](f: V => W): Map[K, W] =
      underlying map { case (k, v) => k -> f(v) }
  }

  implicit final class RichMutableMap[K, V](private val underlying: mutable.Map[K, V]) extends AnyVal {
    def mapValuesStrict[W](f: V => W): mutable.Map[K, W] =
      underlying map { case (k, v) => k -> f(v) }
  }

  implicit final class RichBufferedIterator[A](private val delegate: BufferedIterator[A]) extends AnyVal {
    def headOption: Option[A] = if (delegate.hasNext) Some(delegate.head) else None
  }

  def emptyToNone(@Nullable o: String): Option[String] =
    if (o == null || o.isEmpty) None else Some(o)

  def emptyToNone[A <: Iterable[_]](@Nullable o: A): Option[A] =
    if (o == null || o.isEmpty) None else Some(o)

  def emptyToNone[A](@Nullable o: Array[A]): Option[Array[A]] =
    if (o == null || o.isEmpty) None else Some(o)

  @tailrec
  private def compareIteratorsElementWise[A](a: Iterator[A], b: Iterator[A])(implicit ordering: Ordering[A]): Int =
    (a.nonEmpty, b.nonEmpty) match {
      case (false, false) => 0
      case (true, false) => +1
      case (false, true) => -1
      case (true, true) =>
        ordering.compare(a.next(), b.next()) match {
          case 0 => compareIteratorsElementWise(a, b)
          case o => o
        }
    }
}
