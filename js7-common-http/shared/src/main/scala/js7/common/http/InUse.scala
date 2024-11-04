package js7.common.http

import cats.effect.std.Mutex
import cats.effect.{IO, Resource, ResourceIO}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.log.Logger
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.common.http.InUse.*

private final class InUse:

  private val inUse = Atomic(false)
  private val mutex = memoize(Mutex[IO].map(_.lock))

  def is: Boolean =
    inUse.get()

  /** Not really synchronized with mutex, but we don't use it asynchronously. */
  def get: IO[Boolean] =
    IO(inUse.get())

  def resource[A](api: A): ResourceIO[Unit] =
    Resource:
      mutex.flatMap(_.allocated).logWhenItTakesLonger.flatMap: (_, release) =>
        IO:
          logger.trace(s"↘ $api: inUse := true")
          inUse := true
          () -> IO.defer:
            logger.trace(s"↙ $api: inUse := false")
            inUse := false
            release

  override def toString = s"InUse(${inUse.get()})"


private object InUse:
  private val logger = Logger[this.type]
