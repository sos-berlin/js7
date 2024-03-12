package js7.base.catsutils

import cats.effect.IO
import scala.concurrent.Promise
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.{Deadline, FiniteDuration}

object CatsEffectUtils:

  def promiseIO[A](body: Promise[A] => Unit): IO[A] =
    IO.defer:
      val promise = Promise[A]()
      body(promise)
      IO.fromFuture(IO.pure(promise.future))

  def durationOfIO[A](io: IO[A]): IO[(A, FiniteDuration)] =
    IO.defer:
      val t = Deadline.now
      io.map(_ -> t.elapsed)
