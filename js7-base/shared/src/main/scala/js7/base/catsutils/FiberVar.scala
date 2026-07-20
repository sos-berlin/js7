package js7.base.catsutils

import cats.effect.std.AtomicCell
import cats.effect.{FiberIO, IO, Resource, ResourceIO}
import js7.base.catsutils.CatsEffectExtensions.joinMaybeCancelled
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.utils.CatsUtils.canceledFiberIO

final class FiberVar[A]:

  private val ref: IO[AtomicCell[IO, MyState]] =
    memoize:
      AtomicCell[IO].of:
        HasFiber(canceledFiberIO[A])

  def isCanceled: IO[Boolean] =
    ref.flatMap(_.get).map:
      case Canceled => true
      case _ => false

  /** Cancel finally, this and all future Fibers. */
  def cancel: IO[Unit] =
    ref.flatMap(_.evalUpdate:
      case Canceled => IO.pure(Canceled)
      case HasFiber(previous) => previous.cancel.as(Canceled))

  /** Cancel only the current Fiber. */
  def cancelCurrent: IO[Unit] =
    set(canceledFiberIO)

  def startFiber(io: IO[A]): IO[Unit] =
    cancelCurrent *> io.start.flatMap(set)

  def joinWithUnit: IO[Unit] =
    joinWith(IO.unit.asInstanceOf[IO[A]]).void

  def joinWith(onCanceled: IO[A]): IO[A] =
    ref.flatMap(_.get).flatMap:
      case Canceled => onCanceled
      case HasFiber(fiber) => fiber.joinWith(onCanceled)

  /** Like joinWithUnit but a canceled Fiber results in None. */
  def joinMaybeCancelled: IO[Option[A]] =
    ref.flatMap(_.get).flatMap:
      case Canceled => IO.none
      case HasFiber(fiber) => fiber.joinMaybeCancelled

  def set(fiber: FiberIO[A]): IO[Unit] =
    ref.flatMap:
      _.evalUpdate:
        case Canceled =>
          fiber.cancel.as(Canceled)
        case HasFiber(previous) =>
          previous.cancel.as(HasFiber(fiber))

  private sealed trait MyState
  private final case class HasFiber(fiber: FiberIO[A]) extends MyState
  private case object Canceled extends MyState


object FiberVar:

  def resource[A]: ResourceIO[FiberVar[A]] =
    Resource(IO:
      val fiberVar = new FiberVar[A]
      fiberVar -> fiberVar.cancel)
