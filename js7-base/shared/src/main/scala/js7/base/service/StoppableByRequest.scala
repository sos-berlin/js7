package js7.base.service

import cats.effect.{Deferred, FiberIO, IO, Outcome}
import js7.base.catsutils.CatsEffectExtensions.fromOutcome
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.raceFold
import js7.base.problem.{Checked, Problem}
import js7.base.service.StoppableByRequest.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean

trait StoppableByRequest:
  self =>

  protected[service] val stoppableByCancel = false

  private final val fiber = Deferred.unsafe[IO, FiberIO[Unit]]
  private val stopRequested = Deferred.unsafe[IO, Unit]
  @volatile private var _isStopping = false

  protected final def isStopping: Boolean =
    _isStopping

  private[service] def onFiberStarted(fiber: FiberIO[Unit]): IO[Unit] =
    this.fiber.complete(fiber).void

  protected final def untilStopRequested: IO[Unit] =
    stopRequested.get

  private val memoizedStop =
    memoize:
      IO.defer:
        logger
          .traceIO(s"$toString stop"):
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

  protected final def failWhenStopRequested[A](body: IO[A]): IO[A] =
    body.raceFold(
      untilStopRequested *>
        IO.raiseError(new IllegalStateException(s"$toString is being stopped")))

  protected final def requireNotStopping: IO[Checked[Unit]] =
    IO:
      !isStopping !! Problem(s"$toString is stopping")


object StoppableByRequest:
  private val logger = Logger[this.type]
