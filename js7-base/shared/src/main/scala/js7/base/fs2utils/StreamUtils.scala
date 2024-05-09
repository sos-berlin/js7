package js7.base.fs2utils

import cats.effect
import cats.effect.{IO, Resource}
import cats.syntax.flatMap.*
import fs2.Stream
import js7.base.log.Logger
import js7.base.utils.CloseableIterator

object StreamUtils:

  private val logger = Logger[this.type]

  def closeableIteratorToStream[A](iterator: CloseableIterator[A], chunkSize: Int): Stream[IO, A] =
    Stream
      .resource(Resource.makeCase(
        acquire = IO.pure(iterator))(
        release = (iterator, exitCase) => IO:
          logger.trace(s"Close $iterator $exitCase")
          iterator.close()))
      .flatMap: iterator =>
        Stream.fromIterator(iterator, chunkSize = chunkSize)

  /** Like Stream tailRecM, but limits the memory leak.
    * After a number of `Left` retured by `f`, the returned `Stream` is truncated.
    *
    * @see see Monix 3.2.1, https://github.com/monix/monix/issues/791
    */
  @deprecated
  def memoryLeakLimitedStreamTailRecM[A, B](a: A, limit: Int)(f: A => Stream[IO, Either[A, B]])
  : Stream[IO, B] =
    Stream.suspend:
      var leftCounter = 0
      a.tailRecM(a =>
        f(a).flatMap {
          case o @ Left(_) =>
            if leftCounter >= limit then
              logger.debug(s"Limit Stream.tailRecM after $leftCounter× Left to reduce memory leakage")
              Stream.empty
            else
              leftCounter += 1
              Stream.emit(o)

          case o => Stream.emit(o)
        })
