package js7.base.fs2utils

import cats.effect
import cats.effect.{Concurrent, Resource, Sync}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Stream
import fs2.concurrent.Topic
import js7.base.catsutils.UnsafeMemoizable

final class Fs2PubSub[F[_]: UnsafeMemoizable, A <: AnyRef] private(
  using F: Concurrent[F] & Sync[F]):

  private val topic: F[Topic[F, A]] =
    Topic[F, A].unsafeMemoize

  def publish(a: A): F[Unit] =
    topic.flatMap(_.publish1(a).void)

  @deprecated("Use close")
  def complete: F[Unit] =
    close

  def close: F[Unit] =
    topic.flatMap(_.close.void)

  @deprecated("Use streamResource")
  def newStream: F[Resource[F, Stream[F, A]]] =
    topic.map(_
      .subscribeAwait(maxQueued = 1))

  def streamResource: Resource[F, Stream[F, A]] =
    Resource.eval(topic).flatMap(_.subscribeAwait(maxQueued = 1))

object Fs2PubSub:
  @deprecated("Besser Fs2PubSub als Resource nutzen!")
  def apply[F[_]: UnsafeMemoizable, A <: AnyRef](using Concurrent[F] & Sync[F])
  : Fs2PubSub[F, A] =
    new Fs2PubSub[F, A]

  def resource[F[_]: UnsafeMemoizable, A <: AnyRef](using F: Concurrent[F] & Sync[F])
  : Resource[F, Fs2PubSub[F, A]] =
    Resource.make(F.delay(new Fs2PubSub[F, A]))(_.close)
