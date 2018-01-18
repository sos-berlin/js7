package com.sos.jobscheduler.base.utils

import javax.annotation.Nullable
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.generic.{GenMapFactory, GenericCompanion}
import scala.collection.immutable.{Seq, Vector}
import scala.collection.{GenMap, GenMapLike, GenTraversable, TraversableLike, immutable, mutable}
import scala.language.{higherKinds, implicitConversions}

object Collections {
  object implicits {
    implicit class RichIndexedSeq[A](val underlying: IndexedSeq[A]) extends AnyVal {
      def get(i: Int): Option[A] =
        if (underlying.indices contains i) Some(underlying(i)) else None
    }

    implicit class RichTraversableOnce[A](val delegate: TraversableOnce[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        delegate match {
          case o: Seq[A] ⇒ o
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

    implicit class RichSeq[A](val delegate: collection.Seq[A]) extends AnyVal {
      /**
        * Like `fold` but uses `neutral` only when `operation` cannot be applied, that is size &lt; 2.
        *
        * @param neutral is assumed to be a real neutral element
        */
      def foldFast(neutral: A)(operation: (A, A) ⇒ A): A = delegate match {
        case Nil ⇒ neutral
        case collection.Seq(a) ⇒ a
        case seq ⇒ seq reduce operation
      }
    }

    implicit class RichArray[A](val delegate: Array[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        Vector() ++ delegate
    }

    implicit class RichJavaIterable[A](val delegate: java.lang.Iterable[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        Vector() ++ delegate.asScala
    }

    implicit class RichJavaIterator[A](val delegate: java.util.Iterator[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        Vector() ++ delegate.asScala
    }

    implicit class RichTraversable[A](val delegate: Traversable[A]) extends AnyVal {
      def toKeyedMap[K](toKey: A ⇒ K): Map[K, A] = (delegate map { o ⇒ toKey(o) → o }).uniqueToMap

      def toKeyedMap[K](toKey: A ⇒ K, ifNot: Iterable[K] ⇒ Nothing): Map[K, A] = (delegate map { o ⇒ toKey(o) → o }).uniqueToMap(ifNot)

      def uniqueToSet: Set[A] =
        delegate.requireUniqueness.toSet

      def requireUniqueness: Traversable[A] =
        requireUniqueness(identity[A])

      def requireUniqueness[K](key: A ⇒ K): Traversable[A] =
        ifNotUnique[K, A](key, keys ⇒ throw new DuplicateKeyException(s"Unexpected duplicates: ${keys mkString ", "}"))

      def ifNotUnique[K, B >: A](key: A ⇒ K, then_ : Iterable[K] ⇒ Traversable[B]): Traversable[B] =
        duplicateKeys(key) match {
          case o if o.nonEmpty ⇒ then_(o.keys)
          case _ ⇒ delegate
        }

      def duplicates: Traversable[A] =
        delegate groupBy identity collect { case (k, v) if v.size > 1 ⇒ k }

      /** Liefert die Duplikate, also Listenelemente, deren Schlüssel mehr als einmal vorkommt. */
      def duplicateKeys[K](key: A ⇒ K): Map[K, Traversable[A]] =
        delegate groupBy key filter { _._2.size > 1 }

      /**
        * Like `groupBy`, but returns a `Vector[(K, Vector[A])] ` retaining the original key order (of every first occurrence),
        */
      def retainOrderGroupBy[K](toKey: A ⇒ K): Vector[(K, Vector[A])] = {
        val m = mutable.LinkedHashMap[K, immutable.VectorBuilder[A]]()
        for (elem ← delegate) {
          m.getOrElseUpdate(toKey(elem), new immutable.VectorBuilder[A]) += elem
        }
        val b = Vector.newBuilder[(K, Vector[A])]
        b.sizeHint(m.size)
        for ((k, vectorBuilder) ← m) {
          b += ((k, vectorBuilder.result))
        }
        b.result
      }
    }

    implicit class RichSet[A](val delegate: Set[A]) extends AnyVal {
      def isDisjointWith(o: Set[A]): Boolean =
        delegate forall (k ⇒ !o.contains(k))
    }

    implicit class RichPairTraversable[A, B](val delegate: Traversable[(A, B)]) extends AnyVal {
      def uniqueToMap: Map[A, B] = {
        delegate.requireUniqueness { _._1 }
        delegate.toMap
      }

      def uniqueToMap(ifNot: Iterable[A] ⇒ Nothing): Map[A, B] = {
        delegate.ifNotUnique(_._1, ifNot)
        delegate.toMap
      }

      def toSeqMultiMap: Map[A, Seq[B]] =
        delegate groupBy { _._1 } map { case (key, seq) ⇒ key → (seq map { _._2 }).toImmutableSeq }
    }

    implicit class InsertableMutableMap[K, V](val delegate: mutable.Map[K, V]) extends AnyVal {
      def insert(kv: (K, V)): Unit = {
        if (delegate contains kv._1) throw new DuplicateKeyException(s"Key ${kv._1} is already known in ${delegate.stringPrefix}")
        delegate += kv
      }
    }

    implicit def javaStreamToIterator[A](stream: java.util.stream.Stream[A]): Iterator[A] = stream.iterator.asScala

    implicit class RichJavaStream[A](val delegate: java.util.stream.Stream[A]) extends AnyVal {
      def toImmutableSeq: Seq[A] =
        Vector() ++ delegate.iterator.asScala
    }
  }

  implicit class RichGenericCompanion[+CC[X] <: GenTraversable[X]](val delegate: GenericCompanion[CC]) extends AnyVal {
    def build[A](body: mutable.Builder[A, CC[A]] ⇒ Unit): CC[A] = {
      val b = delegate.newBuilder[A]
      body(b)
      b.result
    }
  }

  implicit class RichMap[K, V](val underlying: Map[K, V]) extends AnyVal {
    def withNoSuchKey(noSuchKey: K ⇒ Nothing): Map[K, V] =
      underlying withDefault (k ⇒ noSuchKey(k))
  }

  // To satisfy IntelliJ IDEA 2016.2.5
  //implicit class RichVector(delegate: Vector.type) extends RichGenericCompanion[Vector](delegate)

  implicit class RichMapCompanion[CC[A, B] <: GenMap[A, B] with GenMapLike[A, B, CC[A, B]]](val delegate: GenMapFactory[CC]) extends AnyVal {
    def build[A, B](body: mutable.Builder[(A, B), CC[A, B]] ⇒ Unit): CC[A, B] = {
      val b = delegate.newBuilder[A, B]
      body(b)
      b.result
    }
  }

  //implicit class RichListMap[K, V](val underlying: ListMap[K, V]) extends AnyVal {
  //  // ListMap is ordered
  //  def keySeq: Seq[K] = underlying.keys.toVector
  //  def valueSeq: Seq[V] = underlying.values.toVector
  //}

  implicit class RichBufferedIterator[A](val delegate: BufferedIterator[A]) extends AnyVal {
    def headOption: Option[A] = if (delegate.hasNext) Some(delegate.head) else None
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
