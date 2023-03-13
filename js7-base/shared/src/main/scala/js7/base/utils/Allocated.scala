package js7.base.utils

import cats.effect.{Resource, SyncIO}
import cats.{:<:, Applicative}
import izumi.reflect.Tag
import js7.base.catsutils.UnsafeMemoizable
import js7.base.catsutils.UnsafeMemoizable.syntax.*
import js7.base.utils.AutoClosing.autoClosing
import scala.annotation.unused

final class Allocated[F[_]: UnsafeMemoizable, +A](val allocatedThing: A, stop_ : F[Unit])
  (implicit aTag: Tag[A])
{
  val stop: F[Unit] =
    stop_.unsafeMemoize

  def toSingleUseResource(implicit F: Applicative[F]): Resource[F, Allocated[F, A]] =
    Resource.make(
      acquire = F.pure(this))(
      release = _.stop)

  def useSync[R](body: A => R)(implicit @unused evidence: F :<: SyncIO)
  : R = {
    val ac: AutoCloseable = () => stop.asInstanceOf[SyncIO[Unit]].unsafeRunSync()
    autoClosing(ac)(_ => body(allocatedThing))
  }

  def toAllocatedString =
    s"Allocated[,${aTag.tag.shortName}]($allocatedThing)"

  override def toString = allocatedThing.toString
}

object Allocated {
  def fromPair[F[_]: UnsafeMemoizable, A: Tag](pair: (A, F[Unit])): Allocated[F, A] =
    new Allocated[F, A](pair._1, pair._2)

  def unapply[F[_], A](allocated: Allocated[F, A]) =
    Some(allocated.allocatedThing -> allocated.stop)
}
