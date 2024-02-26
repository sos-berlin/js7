package js7.base.catsutils

import cats.effect.std.AtomicCell
import cats.effect.{FiberIO, IO}
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.utils.CatsUtils.canceledFiberIO

final class FiberVar[A]:

  private val ref = AtomicCell[IO].of(HasFiber(canceledFiberIO[A]): MyState).unsafeMemoize

  def isCanceled: IO[Boolean] =
    ref.flatMap(_.get).map:
      case Canceled => true
      case _ => false

  def cancel: IO[Unit] =
    ref.flatMap(_.evalUpdate:
      case Canceled => IO.pure(Canceled)
      case HasFiber(previous) =>
        previous.cancel.as(Canceled))

  def set(fiber: FiberIO[A] = canceledFiberIO): IO[Unit] =
    ref.flatMap(_.evalUpdate:
      case Canceled => IO.defer:
        fiber.cancel.as(Canceled)
      case HasFiber(previous) => IO.defer:
        val hasFiber = HasFiber(fiber)
        previous.cancel.as(hasFiber))

  private sealed trait MyState
  private final case class HasFiber(fiber: FiberIO[A]) extends MyState
  private case object Canceled extends MyState
