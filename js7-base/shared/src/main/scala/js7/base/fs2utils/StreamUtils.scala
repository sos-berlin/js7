package js7.base.fs2utils

import cats.effect
import cats.effect.{IO, Resource}
import fs2.Stream
import js7.base.utils.CloseableIterator

object StreamUtils:

  def closeableIteratorToStream[A](iterator: CloseableIterator[A], chunkSize: Int): Stream[IO, A] =
    Stream
      .resource(Resource.makeCase(
        acquire = IO.pure(iterator))(
        release = (iterator, exitCase) => IO:
          //logger.trace(s"Close $iterator $exitCase")
          iterator.close()))
      .flatMap: iterator =>
        Stream.fromIterator(iterator, chunkSize = chunkSize)
