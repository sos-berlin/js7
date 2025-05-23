package js7.base.utils

import cats.Monoid
import js7.base.problem.Problems.DuplicateKey
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichJavaClass}
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.collection.{BufferedIterator, Factory, mutable}

object Collections:

  private def defaultDuplicatesToProblem[K](duplicates: Map[K, Iterable[?]]) =
    duplicatesToProblem("Unexpected duplicates", duplicates)

  def duplicatesToProblem[K](prefix: String, duplicates: Map[K, Iterable[?]]): Problem =
    Problem(s"$prefix: ${duplicates.map { case (k, v) => s"${v.size}×$k" }.mkString("; ")}")

  object implicits:
    implicit final class RichIndexedSeq[A](private val underlying: IndexedSeq[A]) extends AnyVal:
      def get(i: Int): Option[A] =
        underlying.indices contains i thenSome underlying(i)

      def dropLastWhile(predicate: A => Boolean): IndexedSeq[A] =
        var i = underlying.length
        while i > 0 && predicate(underlying(i - 1)) do i = i -1
        underlying.slice(0, i)

    implicit final class RichIterableOnce[CC[x] <: IterableOnce[x], A](private val iterableOnce: CC[A])
    extends AnyVal:
      def foldMonoids(using A: Monoid[A]): A =
        A.combineAll(iterableOnce)

      def countEquals: Map[A, Int] =
        iterableOnce.iterator.to(Iterable) groupBy identity map { case (k, v) => k -> v.size }

      def compareElementWise(other: IterableOnce[A])(implicit ordering: Ordering[A]): Int =
        compareIteratorsElementWise(iterableOnce.iterator, other.iterator)

      def unless(condition: Boolean)(implicit factory: Factory[A, CC[A]]): CC[A] =
        when(!condition)

      def when(condition: Boolean)(implicit factory: Factory[A, CC[A]]): CC[A] =
        if condition then
          iterableOnce
        else
          factory.fromSpecific(Nil)

      /** Returns each element as List head with n following elements as List tail. */
      def lookAhead(n: Int)(implicit factory: Factory[List[A], CC[List[A]]]): CC[List[A]] =
        if n < 0 then throw new IllegalArgumentException(s"lookAhead($n)?")

        val buffer = mutable.ListBuffer.empty[A]
        buffer.sizeHint(n + 1)

        val it = iterableOnce.iterator

        while buffer.length < n - 1 && it.hasNext do buffer += it.next()

        factory.fromSpecific:
          new Iterator[List[A]]:
            def hasNext = it.hasNext || buffer.nonEmpty

            def next() =
              if it.hasNext then
                buffer += it.next()
              if buffer.isEmpty then throw new NoSuchElementException("lookAhead: end of sequence")
              val result = buffer.toList
              buffer.remove(0)
              result

    implicit final class RichIterable[CC[+x] <: Iterable[x], A](private val underlying: CC[A])
    extends AnyVal:
      /**
        * Like `fold` but uses `neutral` only when `operation` cannot be applied, that is size &lt; 2.
        *
        * @param neutral is assumed to be a real neutral element
        */
      def foldFast(neutral: A)(operation: (A, A) => A): A =
        if underlying.isEmpty then neutral
        else if underlying.sizeIs == 1 then underlying.head
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

      def checkUniqueness: Checked[CC[A]] =
        checkUniquenessBy(identity[A])

      def checkUniquenessBy[K](key: A => K): Checked[CC[A]] =
        duplicateKeys(key) match
          case Some(duplicates) => Left(defaultDuplicatesToProblem(duplicates))
          case None => Right(underlying)

      def checkUniqueness_[K](
        key: A => K)(
        duplicatesToProblem: Map[K, Iterable[A]] => Problem)
      : Checked[CC[A]] =
        duplicateKeys(key) match
          case Some(duplicates) => Left(duplicatesToProblem(duplicates))
          case None => Right(underlying)

      def requireUniqueness: CC[A] =
        underlying match
          case _: collection.Set[?] => underlying
          case _ => requireUniqueness(identity[A])

      def requireUniqueness[K](key: A => K): CC[A] =
        ifNotUnique[K, A](key, keys =>
          throw new DuplicateKeyException(s"Unexpected duplicates: ${keys mkString ", "}"))

      def areUnique: Boolean =
        underlying match
          case _: collection.Set[?] => true
          case _ => areUniqueBy[A](identity)

      def areUniqueBy[K](key: A => K): Boolean =
        duplicateKeys(key).isEmpty

      def ifNotUnique[K, B >: A](key: A => K, then_ : Iterable[K] => CC[B]): CC[B] =
        duplicateKeys(key) match
          case Some(o) => then_(o.keys)
          case None => underlying

      def duplicates: Iterable[A] =
        underlying match
          case _: collection.Set[?] => Nil
          case _ =>
            underlying.groupBy(identity).collect:
              case (k, v) if v.sizeIs > 1 => k

      /** Liefert die Duplikate, also Listenelemente, deren Schlüssel mehr als einmal vorkommt. */
      def duplicateKeys[K](key: A => K): Option[Map[K, Iterable[A]]] =
        if underlying.sizeIs <= 1
          || underlying.sizeIs == underlying.view.map(key).toSet.size
        then
          None
        else
          underlying.groupBy(key).filter(_._2.sizeIs > 1) match
            case o if o.isEmpty => None
            case o => Some(o)

      /**
        * Like `groupBy`, but returns a `Vector[(K, Vector[A])] ` retaining the original key order (of every first occurrence),
        */
      def retainOrderGroupBy[K](toKey: A => K): Vector[(K, Vector[A])] =
        val m = mutable.LinkedHashMap.empty[K, VectorBuilder[A]]
        for elem <- underlying do
          m.getOrElseUpdate(toKey(elem), new VectorBuilder[A]) += elem
        val b = Vector.newBuilder[(K, Vector[A])]
        b.sizeHint(m.size)
        for (k, vectorBuilder) <- m do
          b += ((k, vectorBuilder.result()))
        b.result()

    implicit final class RichSet[A](private val delegate: Set[A]) extends AnyVal:
      def isDisjointWith(o: Set[A]): Boolean =
        delegate forall (k => !o.contains(k))

    implicit final class RichPairTraversable[K, V](private val delegate: Iterable[(K, V)]) extends AnyVal:
      def checkedUniqueToMap: Checked[Map[K, V]] =
        checkedUniqueToMap(defaultDuplicatesToProblem)

      def checkedUniqueToMap(duplicatesToProblem: Map[K, Iterable[V]] => Problem): Checked[Map[K, V]] =
        val result = delegate.toMap
        if delegate.sizeIs != result.size then
          delegate
            .checkUniqueness_/*returns Left*/(_._1)(
              duplicates => duplicatesToProblem(duplicates.view.mapValues(_.map(_._2)).toMap))
            .flatMap(_ => Left(Problem.pure("checkedUniqueToMap"))/*never happens*/)
        else
          Right(result)

      def uniqueToMap: Map[K, V] =
        val result = delegate.toMap
        if delegate.sizeIs != result.size then delegate.requireUniqueness(_._1)
        result

      def uniqueToMap(ifNot: Iterable[K] => Nothing): Map[K, V] =
        val result = delegate.toMap
        if delegate.sizeIs != result.size then delegate.ifNotUnique(_._1, ifNot)
        result

      def toSeqMultiMap: Map[K, Seq[V]] =
        delegate.groupBy(_._1) map { case (key, seq) => key -> seq.map(_._2).toSeq }

    implicit final class InsertableMutableMap[K, V](private val delegate: mutable.Map[K, V]) extends AnyVal:
      def insert(key: K, value: V): Unit =
        if delegate contains key then throw new DuplicateKeyException(s"Key $key is already known")
        delegate.update(key, value)

  implicit class RichList[A](private val underlying: List[A]) extends AnyVal:
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

  implicit final class RichMap[K, V](private val underlying: Map[K, V]) extends AnyVal:
    def toChecked(unknownKey: K => Problem): Map[K, Checked[V]] =
      underlying.view.mapValues(Right.apply).toMap withDefault unknownKey.andThen(Left.apply)

    def withNoSuchKey(noSuchKey: K => Nothing): Map[K, V] =
      underlying withDefault (k => noSuchKey(k))

    def mapValuesStrict[W](f: V => W): Map[K, W] =
      underlying map { case (k, v) => k -> f(v) }

    def insert(kv: (K, V)): Checked[Map[K, V]] =
      insert(kv._1, kv._2)

    def insert(key: K, value: V): Checked[Map[K, V]] =
      if underlying contains key then
        Left(DuplicateKey(key.getClass.shortClassName, key))
      else
        Right(underlying.updated(key, value))

  implicit final class RichMutableMap[K, V](private val underlying: mutable.Map[K, V]) extends AnyVal:
    def mapValuesStrict[W](f: V => W): mutable.Map[K, W] =
      underlying map { case (k, v) => k -> f(v) }

  implicit final class RichBufferedIterator[A](private val delegate: BufferedIterator[A]) extends AnyVal:
    def headOption: Option[A] =
      delegate.hasNext thenSome delegate.head

  def emptyToNone(o: String | Null): Option[String] =
    o match
      case null => None
      case o: String => if o.isEmpty then None else Some(o)

  def emptyToNone[A <: Iterable[?]](a: A | Null): Option[A] =
    if a == null || a.isEmpty then None else Some(a)

  def emptyToNone[A](o: Array[A] | Null): Option[Array[A]] =
    if o == null || o.isEmpty then None else Some(o)

  @tailrec
  private def compareIteratorsElementWise[A](a: Iterator[A], b: Iterator[A])(implicit ordering: Ordering[A]): Int =
    (a.nonEmpty, b.nonEmpty) match
      case (false, false) => 0
      case (true, false) => +1
      case (false, true) => -1
      case (true, true) =>
        ordering.compare(a.next(), b.next()) match
          case 0 => compareIteratorsElementWise(a, b)
          case o => o
