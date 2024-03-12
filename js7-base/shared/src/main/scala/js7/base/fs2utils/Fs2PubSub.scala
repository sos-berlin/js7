package js7.base.fs2utils

import cats.effect
import cats.effect.{Concurrent, Resource, Sync}
import cats.syntax.functor.*
import fs2.Stream
import fs2.concurrent.Topic
import js7.base.catsutils.UnsafeMemoizable

final class Fs2PubSub[F[_], A <: AnyRef] private(topic: Topic[F, A])
  (using F: Concurrent[F] & Sync[F]):

  def publish(a: A): F[Unit] =
    topic.publish1(a).void

  @deprecated("Use close")
  def complete: F[Unit] =
    close

  def close: F[Unit] =
    topic.close.void

  def newStream: Stream[F, A] =
    topic.subscribe(maxQueued = 1)

  def streamResource: Resource[F, Stream[F, A]] =
    topic.subscribeAwait(maxQueued = 0)

object Fs2PubSub:
  def resource[F[_]: UnsafeMemoizable, A <: AnyRef](using F: Concurrent[F] & Sync[F])
  : Resource[F, Fs2PubSub[F, A]] =
    for
      topic <- Resource.eval(Topic[F, A])
      pubSub <- Resource.make(
        acquire = F.delay(new Fs2PubSub[F, A](topic)))(
        release = _.close)
    yield
      pubSub
