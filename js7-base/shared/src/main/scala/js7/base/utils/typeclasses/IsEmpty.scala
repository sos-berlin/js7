package js7.base.utils.typeclasses

import cats.{Eq, Monoid}
import scala.language.implicitConversions

trait IsEmpty[A]
{
  def isEmpty(a: A): Boolean

  final def isNonEmpty(a: A) = !isEmpty(a)

  /** None if a isEmpty, otherwise Some(a). */
  def emptyToNone(a: A): Option[A] =
    if (isEmpty(a)) None else Some(a)
}

object IsEmpty
{
  def apply[A](implicit o: IsEmpty[A]): IsEmpty[A] = o

  //def fromIsEmpty[A](empty: A => Boolean): IsEmpty[A] =
  //  new IsEmpty[A] {
  //    def isEmpty(a: A) = empty(a)
  //  }

  trait Ops[A] {
    def typeClassInstance: IsEmpty[A]
    def self: A
    def isEmpty: Boolean = typeClassInstance.isEmpty(self)
    def nonEmpty = !isEmpty

    /** None if `this` isEmpty. */
    def ?? : Option[A] = typeClassInstance.emptyToNone(self)
    def emptyToNone : Option[A] = typeClassInstance.emptyToNone(self)
  }

  object syntax {
    implicit def toIsEmptyAllOps[A](target: A)(implicit tc: IsEmpty[A]): Ops[A] =
      new Ops[A] {
        val self = target
        val typeClassInstance = tc
      }
  }

  implicit val stringIsEmpty: IsEmpty[String] = _.isEmpty
  implicit val intIsEmpty: IsEmpty[Int] = _ == 0

  // `??` operator conflicts with binary `??` operator defined in ScalaUtils
  // (but unary `?` operator is equivalent).
  implicit val booleanIsEmpty: IsEmpty[Boolean] = _ == false

  implicit def monoidIsEmpty[A](implicit monoid: Monoid[A], eq: Eq[A]): IsEmpty[A] =
    IsEmpty[A](monoid.isEmpty)

  private val erasedIterableIsEmpty: IsEmpty[Iterable[?]] = _.isEmpty

  @inline implicit def iterableIsEmpty[A <: Iterable[?]]: IsEmpty[A] =
    erasedIterableIsEmpty.asInstanceOf[IsEmpty[A]]
}
