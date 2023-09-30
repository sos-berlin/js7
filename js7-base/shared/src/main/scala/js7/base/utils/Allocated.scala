package js7.base.utils

import cats.effect.{Resource, SyncIO}
import cats.{:<:, Applicative}
import izumi.reflect.Tag
import js7.base.catsutils.UnsafeMemoizable
import js7.base.catsutils.UnsafeMemoizable.syntax.*
import js7.base.utils.AutoClosing.autoClosing
import scala.annotation.unused

final class Allocated[F[_]: UnsafeMemoizable, +A](val allocatedThing: A, release_ : F[Unit], label: String):

  def this(allocatedThing: A, release: F[Unit])(implicit A: Tag[A]) =
    this(allocatedThing, release, label = A.tag.shortName)

  val release: F[Unit] =
    release_.unsafeMemoize

  def map[B: Tag](f: A => B): Allocated[F, B] =
    new Allocated(f(allocatedThing), release_)

  def toSingleUseResource(implicit F: Applicative[F]): Resource[F, Allocated[F, A]] =
    Resource.make(
      acquire = F.pure(this))(
      release = _.release)

  def blockingUse[R](body: A => R)(implicit @unused evidence: F :<: SyncIO)
  : R =
    val ac: AutoCloseable = () => release.asInstanceOf[SyncIO[Unit]].unsafeRunSync()
    autoClosing(ac)(_ => body(allocatedThing))

  def toAllocatedString =
    s"Allocated[,$label]($allocatedThing)"

  override def toString = allocatedThing.toString

object Allocated:
  def apply[F[_]: UnsafeMemoizable, A: Tag](allocatedThing: A, release: F[Unit]) =
    new Allocated(allocatedThing, release)

  def fromPair[F[_]: UnsafeMemoizable, A: Tag](pair: (A, F[Unit])): Allocated[F, A] =
    new Allocated[F, A](pair._1, pair._2)

  def unapply[F[_], A](allocated: Allocated[F, A]) =
    Some(allocated.allocatedThing -> allocated.release)
