package js7.base.catsutils

import cats.effect.std.AtomicCell
import cats.effect.{FiberIO, IO, Resource, ResourceIO}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.utils.CatsUtils.canceledFiberIO

final class FiberVar[A]:

  private val ref = memoize:
    AtomicCell[IO].of(HasFiber(canceledFiberIO[A]): MyState)

  def isCanceled: IO[Boolean] =
    ref.flatMap(_.get).map:
      case Canceled => true
      case _ => false

  def cancel: IO[Unit] =
    ref.flatMap(_.evalUpdate:
      case Canceled => IO.pure(Canceled)
      case HasFiber(previous) => previous.cancel.as(Canceled))

  def startFiber(io: IO[A]): IO[Unit] =
    cancelCurrent *> io.start.flatMap(set)

  def joinCurrent: IO[Unit] =
    ref.flatMap(_.get).flatMap:
      case Canceled => IO.unit
      case HasFiber(fiber) => fiber.join.void

  def cancelCurrent: IO[Unit] =
    set(canceledFiberIO)

  def set(fiber: FiberIO[A]): IO[Unit] =
    ref.flatMap(_.evalUpdate:
      case Canceled =>
        fiber.cancel.as(Canceled)
      case HasFiber(previous) =>
        previous.cancel.as(HasFiber(fiber)))

  private sealed trait MyState
  private final case class HasFiber(fiber: FiberIO[A]) extends MyState
  private case object Canceled extends MyState


object FiberVar:

  def resource[A]: ResourceIO[FiberVar[A]] =
    Resource(IO:
      val fiberVar = new FiberVar[A]
      fiberVar -> fiberVar.cancel)
