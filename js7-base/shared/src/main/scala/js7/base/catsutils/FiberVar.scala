package js7.base.catsutils

import cats.effect.std.AtomicCell
import cats.effect.{FiberIO, IO}
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.utils.CatsUtils
import js7.base.utils.CatsUtils.canceledFiberIO

final class FiberVar[A]:

  private val ref = AtomicCell[IO].of(HasFiber(canceledFiberIO[A]): MyState).unsafeMemoize

  def isCanceled: IO[Boolean] =
    ref.flatMap(_.get).map:
      case Canceled => true
      case _ => false

  def cancel: IO[Unit] =
   logger.traceIO:
    ref.flatMap(_.evalUpdate:
      case Canceled => IO.pure(Canceled)
      case HasFiber(previous) =>
        logger.trace(s"### $previous.cancel")
        previous.cancel.as(Canceled))

  def set(fiber: FiberIO[A] = canceledFiberIO): IO[Unit] =
   logger.traceIO("### set", fiber):
    ref.flatMap(_.evalUpdate:
      case Canceled => IO.defer:
        logger.trace(s"### Canceled $fiber.cancel")
        fiber.cancel.as(Canceled)
      case myState @ HasFiber(previous) => IO.defer:
        val hasFiber = HasFiber(fiber)
        logger.trace(s"### $myState $previous.cancel $hasFiber")
        previous.cancel.as(hasFiber))

  private val logger = Logger[this.type]

  private sealed trait MyState
  private final case class HasFiber(fiber: FiberIO[A]) extends MyState
  private case object Canceled extends MyState
