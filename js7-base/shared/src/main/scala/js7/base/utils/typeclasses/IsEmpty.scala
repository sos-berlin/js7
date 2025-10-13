package js7.base.utils.typeclasses

import cats.{Eq, Monoid}
import org.jetbrains.annotations.TestOnly
import scala.language.implicitConversions

// TODO Use Alleycats Empty instead?
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

      // !? isn't a good symbol, use mapNonEmpty instead
      @TestOnly private[typeclasses] inline def !?(replacement: => A): A =
        if a.nonEmpty then
          replacement
        else
          a // empty

      def ifEmpty[B](replacementForEmpty: => B): A | B =
        if a.isEmpty then
          replacementForEmpty
        else
          a

      def mapNonEmpty[B](f: A => B): A | B =
        if a.nonEmpty then
          f(a)
        else
          a // empty


    //extension [F[+_], A](fa: F[A])
    //  /** Applies f to this when this isNonEmpty, otherwise return this. */
    //  def mapNonEmpty[B](f: F[A] => F[B])(using IsEmpty[F[A]]): F[A | B] =
    //    if fa.isEmpty then
    //      fa
    //    else
    //      f(fa)


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
