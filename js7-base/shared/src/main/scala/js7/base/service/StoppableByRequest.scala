package js7.base.service

import cats.effect.{Deferred, FiberIO, IO, Outcome}
import js7.base.catsutils.CatsEffectExtensions.{fromOutcome, raiseError_}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.raceMerge
import js7.base.problem.Checked
import js7.base.service.Problems.ServiceStoppedProblem
import js7.base.service.StoppableByRequest.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean

trait StoppableByRequest:

  protected[service] val stoppableByCancel = false

  private final val fiber = Deferred.unsafe[IO, FiberIO[Unit]]
  private val stopRequested = Deferred.unsafe[IO, Unit]
  @volatile private var _isStopping = false

  final def isStopping: Boolean =
    _isStopping

  private[service] def onFiberStarted(fiber: FiberIO[Unit]): IO[Unit] =
    this.fiber.complete(fiber).void

  protected final def untilStopRequested: IO[Unit] =
    stopRequested.get

  private val memoizedStop =
    memoize:
      IO.defer:
        logger.traceIO(s"$toString stop"):
          _isStopping = true
          stopRequested.complete(())
            .*>(fiber.get)
            .flatMap: fiber =>
              IO.whenA(stoppableByCancel)(fiber.cancel) *>
                fiber.join.flatMap:
                  case Outcome.Canceled() => IO.unit
                  case o => IO.fromOutcome(o)

  protected def stop: IO[Unit] =
    memoizedStop

  /** When stop is being requested, cancel the body and throw. */
  protected final def cancelOnStopRequest[A](body: IO[A]): IO[A] =
    body.raceMerge:
      untilStopRequested *>
        IO.raiseError_(new IllegalStateException(s"$toString is being stopped"))

  protected final def requireNotStopping: IO[Checked[Unit]] =
    IO(checkNotStopping)

  protected final def checkNotStopping: Checked[Unit] =
    !isStopping !! ServiceStoppedProblem(toString)


object StoppableByRequest:
  private val logger = Logger[this.type]
