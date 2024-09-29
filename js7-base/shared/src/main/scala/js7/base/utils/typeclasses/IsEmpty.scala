package js7.base.utils.typeclasses

import cats.{Eq, Monoid}
import scala.language.implicitConversions

trait IsEmpty[A]:

  def isEmpty(a: A): Boolean

  /** None if a isEmpty, otherwise Some(a). */
  def ifNonEmpty(a: A): Option[A] =
    if isEmpty(a) then None else Some(a)


object IsEmpty:

  def apply[A](implicit o: IsEmpty[A]): IsEmpty[A] = o

  def fromIsEmpty[A](empty: A => Boolean): IsEmpty[A] =
    empty(_)


  object syntax:
    extension [A](a: A)(using IsEmpty: IsEmpty[A])
      def isEmpty: Boolean =
        IsEmpty.isEmpty(a)

      inline def nonEmpty: Boolean =
        !isEmpty

      /** None if `this` isEmpty. */
      inline def ?? : Option[A] =
        ifNonEmpty

      /** Wraps this in Some if nonEmpty, or returns None if empty */
      def ifNonEmpty: Option[A] =
        IsEmpty.ifNonEmpty(a)

      inline def ??(replacementForEmpty: => A): A =
        ifEmpty(replacementForEmpty)

      def ifEmpty[A1 >: A](replacementForEmpty: => A1): A1 =
        if a.isEmpty then
          replacementForEmpty
        else
          a


    extension [F[+_], A](fa: F[A])
      /** Applies f to this when this isNonEmpty, otherwise return this. */
      def whenNonEmpty[A1 >: A](f: F[A] => F[A1])(using IsEmpty[F[A]]): F[A1] =
        if fa.isEmpty then
          fa
        else
          f(fa)


  given IsEmpty[String] = _.isEmpty
  given IsEmpty[Int] = _ == 0

  // `??` operator conflicts with binary `??` operator defined in ScalaUtils
  // (but unary `?` operator is equivalent).
  //implicit val booleanIsEmpty: IsEmpty[Boolean] = _ == false

  given [A](using monoid: Monoid[A], eq: Eq[A]): IsEmpty[A] =
    IsEmpty.fromIsEmpty[A](monoid.isEmpty)

  private val erasedIterableIsEmpty: IsEmpty[Iterable[?]] = _.isEmpty

  inline implicit def iterableIsEmpty[A <: Iterable[?]]: IsEmpty[A] =
    erasedIterableIsEmpty.asInstanceOf[IsEmpty[A]]
