package js7.base.fs2utils

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{FlatMap, Functor}
import fs2.Stream
import fs2.concurrent.Topic
import js7.base.catsutils.Memoizable
import js7.base.catsutils.Memoizable.syntax.MemoizableIO

final class Fs2PubSub[F[_]: Functor: FlatMap: Concurrent: Memoizable, A <: AnyRef]
{
  private val Initial = Fs2PubSub.Initial.asInstanceOf[A]
  private val EOF = Fs2PubSub.EOF.asInstanceOf[A]

  private val topic: F[Topic[F, A]] =
    Topic[F, A](Initial).memoize

  def publish(a: A): F[Unit] =
    topic.flatMap(_.publish1(a))

  def complete: F[Unit] =
    publish(EOF)

  def newStream: F[Stream[F, A]] =
    topic.map(_
      .subscribe(maxQueued = 1)
      .filter(_ != Initial)
      .takeWhile(_ != EOF))
}

object Fs2PubSub
{
  private val Initial = new AnyRef{}
  private val EOF = new AnyRef{}
}