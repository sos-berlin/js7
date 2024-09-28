package js7.base.utils.typeclasses

import cats.{Eq, Monoid}
import scala.language.implicitConversions

trait IsEmpty[A]:

  def isEmpty(a: A): Boolean

  /** None if a isEmpty, otherwise Some(a). */
  def ifNonEmpty(a: A): Option[A] =
    if isEmpty(a) then None else Some(a)

  def ifEmpty(a: A, replacementForEmpty: => A): A =
    if isEmpty(a) then
      replacementForEmpty
    else
      a


object IsEmpty:
  def apply[A](implicit o: IsEmpty[A]): IsEmpty[A] = o

  def fromIsEmpty[A](empty: A => Boolean): IsEmpty[A] =
    empty(_)


  trait Ops[A]:
    def typeClassInstance: IsEmpty[A]
    def self: A

    def isEmpty: Boolean =
      typeClassInstance.isEmpty(self)

    inline def nonEmpty: Boolean =
      !isEmpty

    /** None if `this` isEmpty. */
    inline def ?? : Option[A] =
      ifNonEmpty

    /** Wraps this in Some if nonEmpty, or returns None if empty */
    def ifNonEmpty: Option[A] =
      typeClassInstance.ifNonEmpty(self)

    inline def ??(replacementForEmpty: => A): A =
      ifEmpty(replacementForEmpty)

    def ifEmpty(replacementForEmpty: => A): A =
      typeClassInstance.ifEmpty(self, replacementForEmpty)


  object syntax:
    implicit def toIsEmptyAllOps[A](target: A)(implicit tc: IsEmpty[A]): Ops[A] =
      new Ops[A]:
        val self = target
        val typeClassInstance = tc


  implicit val stringIsEmpty: IsEmpty[String] = _.isEmpty
  implicit val intIsEmpty: IsEmpty[Int] = _ == 0

  // `??` operator conflicts with binary `??` operator defined in ScalaUtils
  // (but unary `?` operator is equivalent).
  implicit val booleanIsEmpty: IsEmpty[Boolean] = _ == false

  implicit def monoidIsEmpty[A](implicit monoid: Monoid[A], eq: Eq[A]): IsEmpty[A] =
    IsEmpty.fromIsEmpty[A](monoid.isEmpty)

  private val erasedIterableIsEmpty: IsEmpty[Iterable[?]] = _.isEmpty

  @inline implicit def iterableIsEmpty[A <: Iterable[?]]: IsEmpty[A] =
    erasedIterableIsEmpty.asInstanceOf[IsEmpty[A]]
