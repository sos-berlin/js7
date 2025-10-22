package js7.common.pekkohttp.web.session

import cats.effect.{FiberIO, IO}
import cats.syntax.option.*
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.utils.ScalaUtils.syntax.foldMap

private[session] trait HasTimeout:

  private var state = none[
    (timeoutAt: CatsDeadline, timeoutFiber: FiberIO[Unit])]

  final def isAlive: IO[Boolean] =
    state.fold(IO.True):
      _.timeoutAt.hasTimeLeft

  /** Only untouched ones are eternal! */
  private[session] final def timeoutAt(timeoutAt: CatsDeadline, timeoutFiber: FiberIO[Unit])
  : IO[Unit] =
    CatsDeadline.now.map: now =>
      state = Some((timeoutAt, timeoutFiber))

  def cancelTimeoutFiber: IO[Unit] =
    state.foldMap: fiber =>
      fiber.timeoutFiber.cancel

  private[session] final def isEternal: Boolean =
    state.isEmpty
