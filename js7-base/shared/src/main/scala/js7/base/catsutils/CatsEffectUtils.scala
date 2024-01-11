package js7.base.catsutils

import cats.effect.IO
import scala.concurrent.Promise

object CatsEffectUtils:

  def promiseIO[A](body: Promise[A] => Unit): IO[A] =
    IO.defer:
      val promise = Promise[A]()
      body(promise)
      IO.fromFuture(IO.pure(promise.future))
