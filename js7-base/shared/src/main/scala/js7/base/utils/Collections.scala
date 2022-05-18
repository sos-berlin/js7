package js7.base.utils

import cats.Monoid
import javax.annotation.Nullable
import js7.base.problem.Problems.DuplicateKey
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.collection.{BufferedIterator, Factory, mutable}

object Collections
{
  private def defaultDuplicatesToProblem[K](duplicates: Map[K, Iterable[_]]) =
    duplicatesToProblem("Unexpected duplicates", duplicates)

  def duplicatesToProblem[K](prefix: String, duplicates: Map[K, Iterable[_]]) =
    Problem(s"$prefix: ${duplicates.map { case (k, v) => s"${v.size}×$k" }.mkString("; ")}")

  object implicits {
    implicit final class RichIndexedSeq[A](private val underlying: IndexedSeq[A]) extends AnyVal {
      def get(i: Int): Option[A] =
        if (underlying.indices contains i) Some(underlying(i)) else None

      def dropLastWhile(predicate: A => Boolean): IndexedSeq[A] = {
        var i = underlying.length
        while (i > 0 && predicate(underlying(i - 1))) i = i -1
        underlying.slice(0, i)
      }
    }

    implicit final class RichIterableOnce[CC[x] <: IterableOnce[x], A](private val iterableOnce: CC[A])
    extends AnyVal
    {
      def fold_(implicit A: Monoid[A]): A =
        A.combineAll(iterableOnce)

      def countEquals: Map[A, Int] =
        iterableOnce.iterator.to(Iterable) groupBy identity map { case (k, v) => k -> v.size }

      def compareElementWise(other: IterableOnce[A])(implicit ordering: Ordering[A]): Int =
        compareIteratorsElementWise(iterableOnce.iterator, other.iterator)

      def unless(condition: Boolean)(implicit factory: Factory[A, CC[A]]): CC[A] =
        when(!condition)

      def when(condition: Boolean)(implicit factory: Factory[A, CC[A]]): CC[A] =
        if (condition)
          iterableOnce
        else
          factory.fromSpecific(Nil)
    }

    implicit final class RichIterable[CC[x] <: Iterable[x], A](private val underlying: CC[A])
    extends AnyVal
    {
      /**
        * Like `fold` but uses `neutral` only when `operation` cannot be applied, that is size &lt; 2.
        *
        * @param neutral is assumed to be a real neutral element
        */
      def foldFast(neutral: A)(operation: (A, A) => A): A =
        if (underlying.isEmpty) neutral
        else if (underlying.sizeIs == 1) underlying.head
        else underlying reduce operation

      def toCheckedKeyedMap[K](toKey: A => K): Checked[Map[K, A]] =
        toCheckedKeyedMap(toKey, defaultDuplicatesToProblem)

      def toCheckedKeyedMap[K](toKey: A => K, duplicatesToProblem: Map[K, Iterable[A]] => Problem): Checked[Map[K, A]] =
        underlying.view.map(o => toKey(o) -> o).checkedUniqueToMap(duplicatesToProblem)

      def toKeyedMap[K](toKey: A => K): Map[K, A] =
        underlying.view.map(o => toKey(o) -> o).uniqueToMap

      def toKeyedMap[K](toKey: A => K, ifNot: Iterable[K] => Nothing): Map[K, A] =
        underlying.view.map(o => toKey(o) -> o).uniqueToMap(ifNot)

      def uniqueToSet: Set[A] =
        underlying.requireUniqueness.toSet

      def checkUniqueness: Checked[Iterable[A]] =
        checkUniqueness(identity[A])

      def checkUniqueness[K](key: A => K): Checked[CC[A]] =
        duplicateKeys(key) match {
          case Some(duplicates) => Left(defaultDuplicatesToProblem(duplicates))
          case None => Right(underlying)
        }

      def checkUniqueness_[K](
        key: A => K)(
        duplicatesToProblem: Map[K, Iterable[A]] => Problem)
      : Checked[CC[A]] =
        duplicateKeys(key) match {
          case Some(duplicates) => Left(duplicatesToProblem(duplicates))
          case None => Right(underlying)
        }

      def requireUniqueness: Iterable[A] =
        requireUniqueness(identity[A])

      def requireUniqueness[K](key: A => K): Iterable[A] =
        ifNotUnique[K, A](key, keys => throw new DuplicateKeyException(s"Unexpected duplicates: ${keys mkString ", "}"))

      def areUnique: Boolean =
        areUniqueBy[A](identity)

      def areUniqueBy[K](key: A => K): Boolean =
        duplicateKeys(key).isEmpty

      def ifNotUnique[K, B >: A](key: A => K, then_ : Iterable[K] => Iterable[B]): Iterable[B] =
        duplicateKeys(key) match {
          case Some(o) => then_(o.keys)
          case None => underlying
        }

      def duplicates: Iterable[A] =
        underlying groupBy identity collect { case (k, v) if v.sizeIs > 1 => k }

      /** Liefert die Duplikate, also Listenelemente, deren Schlüssel mehr als einmal vorkommt. */
      def duplicateKeys[K](key: A => K): Option[Map[K, Iterable[A]]] =
        if (underlying.sizeIs == underlying.view.map(key).toSet.size)
          None
        else
          underlying.groupBy(key).filter(_._2.sizeIs > 1) match {
            case o if o.isEmpty => None
            case o => Some(o)
          }

      /**
        * Like `groupBy`, but returns a `Vector[(K, Vector[A])] ` retaining the original key order (of every first occurrence),
        */
      def retainOrderGroupBy[K](toKey: A => K): Vector[(K, Vector[A])] = {
        val m = mutable.LinkedHashMap.empty[K, VectorBuilder[A]]
        for (elem <- underlying) {
          m.getOrElseUpdate(toKey(elem), new VectorBuilder[A]) += elem
        }
        val b = Vector.newBuilder[(K, Vector[A])]
        b.sizeHint(m.size)
        for ((k, vectorBuilder) <- m) {
          b += ((k, vectorBuilder.result()))
        }
        b.result()
      }
    }

    implicit final class RichIterator[A](private val underlying: Iterator[A]) extends AnyVal {
      def takeWhileInclusive(predicate: A => Boolean): Iterator[A] = {
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

    implicit final class RichPairTraversable[K, V](private val delegate: Iterable[(K, V)]) extends AnyVal
    {
      def checkedUniqueToMap: Checked[Map[K, V]] =
        checkedUniqueToMap(defaultDuplicatesToProblem)

      def checkedUniqueToMap(duplicatesToProblem: Map[K, Iterable[V]] => Problem): Checked[Map[K, V]] = {
        val result = delegate.toMap
        if (delegate.sizeIs != result.size)
          delegate
            .checkUniqueness_/*returns Left*/(_._1)(
              duplicates => duplicatesToProblem(duplicates.view.mapValues(_.map(_._2)).toMap))
            .flatMap(_ => Left(Problem.pure("checkedUniqueToMap"))/*never happens*/)
        else
          Right(result)
      }

      def uniqueToMap: Map[K, V] = {
        val result = delegate.toMap
        if (delegate.sizeIs != result.size) delegate.requireUniqueness(_._1)
        result
      }

      def uniqueToMap(ifNot: Iterable[K] => Nothing): Map[K, V] = {
        val result = delegate.toMap
        if (delegate.sizeIs != result.size) delegate.ifNotUnique(_._1, ifNot)
        result
      }

      def toSeqMultiMap: Map[K, Seq[V]] =
        delegate.groupBy(_._1) map { case (key, seq) => key -> seq.map(_._2).toSeq }
    }

    implicit final class InsertableMutableMap[K, V](private val delegate: mutable.Map[K, V]) extends AnyVal {
      def insert(key: K, value: V): Unit = {
        if (delegate contains key) throw new DuplicateKeyException(s"Key $key is already known")
        delegate.update(key, value)
      }
    }
  }

  implicit class RichList[A](private val underlying: List[A]) extends AnyVal
  {
    def mergeConsecutiveElements(merge: PartialFunction[(A, A), A]): List[A] =
      underlying
        .scanLeft(List.empty[A])((reverseList, expr) =>
          (reverseList, expr) match {
            case (a :: tail, b) =>
              merge.lift((a, b)).fold(b :: a :: tail)(_ :: tail)
            case _ =>
              expr :: reverseList
          })
        .lastOption.toList
        .flatten
        .reverse
  }

  implicit final class RichMap[K, V](private val underlying: Map[K, V]) extends AnyVal {
    def toChecked(unknownKey: K => Problem): Map[K, Checked[V]] =
      underlying.view.mapValues(Right.apply).toMap withDefault unknownKey.andThen(Left.apply)

    def withNoSuchKey(noSuchKey: K => Nothing): Map[K, V] =
      underlying withDefault (k => noSuchKey(k))

    def mapValuesStrict[W](f: V => W): Map[K, W] =
      underlying map { case (k, v) => k -> f(v) }

    def insert(kv: (K, V)): Checked[Map[K, V]] =
      if (underlying contains kv._1)
        Left(DuplicateKey(kv._1.getClass.simpleScalaName, kv._1))
      else
        Right(underlying + kv)
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
