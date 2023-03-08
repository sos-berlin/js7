package js7.base.utils

import cats.Applicative
import cats.effect.Resource
import js7.base.catsutils.UnsafeMemoizable
import js7.base.catsutils.UnsafeMemoizable.syntax.*

final class Allocated[F[_]: UnsafeMemoizable, +A](val allocatedThing: A, stop_ : F[Unit])
{
  val stop: F[Unit] =
    stop_.unsafeMemoize

  def toSingleUseResource(implicit F: Applicative[F]): Resource[F, Allocated[F, A]] =
    Resource.make(
      acquire = F.pure(this))(
      release = _.stop)

  override def toString = allocatedThing.toString
}

object Allocated {
  def fromPair[F[_]: UnsafeMemoizable, A](pair: (A, F[Unit])): Allocated[F, A] =
    new Allocated[F, A](pair._1, pair._2)

  def unapply[F[_], A](allocated: Allocated[F, A]) =
    Some(allocated.allocatedThing -> allocated.stop)
}
