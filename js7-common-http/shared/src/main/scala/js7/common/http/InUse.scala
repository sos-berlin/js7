package js7.common.http

import cats.effect.kernel.Resource
import cats.effect.{IO, ResourceIO}
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.log.Logger
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.{Atomic, MVar}
import js7.common.http.InUse.*

private final class InUse:

  private val inUse = Atomic(false)
  private val inUseVar = MVar.empty[IO, Unit].unsafeMemoize

  def is: Boolean =
    inUse.get()

  def get: IO[Boolean] =
    inUseVar.flatMap(_.tryRead).map(_.isDefined)

  def resource[A](api: A): ResourceIO[Unit] =
    Resource.make(
      acquire =
        inUseVar.flatMap(_.put(())).logWhenItTakesLonger *>
          IO:
            logger.trace(s"↘ $api: inUse := true")
            inUse := true)(
      release = _ => IO.defer:
        logger.trace(s"↙ $api: inUse := false")
        inUse := false
        inUseVar.flatMap(_.tryTake).void)

  override def toString = s"InUse(${inUse.get})"


private object InUse:
  private val logger = Logger[this.type]
