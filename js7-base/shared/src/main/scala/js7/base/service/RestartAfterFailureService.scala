package js7.base.service

import cats.effect.{Deferred, IO, ResourceIO}
import cats.syntax.flatMap.*
import cats.syntax.option.*
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.log.Logger
import js7.base.service.RestartAfterFailureService.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.Delayer.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Allocated, Atomic, DelayConf, Delayer}
import scala.concurrent.duration.*

final class RestartAfterFailureService[S <: Service: Tag] private[service](
  startDelays: Seq[FiniteDuration] = defaultRestartDelays,
  runDelays: Seq[FiniteDuration] = defaultRestartDelays)
  (serviceResource: ResourceIO[S])
extends Service:
  self =>

  private val serviceName = implicitly[Tag[S]].tag.toString
  private val currentAllocatedService = Atomic(none[Allocated[IO, S]])
  @volatile private var stopping = false
  private val untilStopRequested = Deferred.unsafe[IO, Unit]

  private val maybeStartDelay = DelayConf.maybe(startDelays)
  private val maybeRunDelay = DelayConf.maybe(runDelays)

  protected val stop =
    memoize:
      IO.defer:
        stopping = true
        untilStopRequested.complete(()) *>
          currentAllocatedService.get().fold(IO.unit)(_.release)

  protected def start =
    for
      service <- startUnderlyingService
      started <- startService(runUnderlyingService(service))
    yield started

  private def startUnderlyingService: IO[S] =
    serviceResource
      .toAllocated
      .pipeMaybe(maybeStartDelay)(
        _.onFailureRestartWithDelayer(_,
          onFailure = t => IO:
            logger.error(s"$serviceName start failed: ${t.toStringWithCauses}")
            for st <- t.ifStackTrace do
              logger.debug(s"$serviceName start failed: ${t.toStringWithCauses}", st),
          onSleep = logDelay))
      .flatTap(setCurrentService)
      .map(_.allocatedThing)

  private def setCurrentService(allocated: Allocated[IO, S]): IO[Unit] =
    IO.defer:
      currentAllocatedService
        .getAndSet(Some(allocated))
        .fold(IO.unit)(_.release.when(stopping))
        .*>(allocated.release.when(stopping))

  private def runUnderlyingService(service: S) =
    maybeRunDelay.fold(service.untilStopped)(delayConf =>
      Delayer.start[IO](delayConf).flatMap(delayer =>
        service.some.tailRecM { initialService =>
          val service = initialService match {
            case Some(initialService) => IO.pure(initialService) // First iteration
            case None => startUnderlyingService // Following iterations
          }
          service.flatMap(service =>
            service
              .untilStopped
              .map(Right(_))
              .handleErrorWith { throwable =>
                // Service has already logged the throwable
                logger.debug(s"💥 $serviceName start failed: ${throwable.toStringWithCauses}",
                  throwable.nullIfNoStackTrace)
                if stopping then
                  IO.right(())
                else
                  for _ <- delayer.sleep(logDelay) yield
                    if stopping then
                      Right(()) // finish tailRecM
                    else
                      Left(None) /*loop*/
              })
        }))

  private def logDelay(duration: FiniteDuration) =
    IO(logger.info(
      "Due to failure, " + serviceName + " restarts " +
        (if duration.isZero then "now" else "in " + duration.pretty)))

  // Call only after this Service has been started!
  def unsafeCurrentService(): S =
    currentAllocatedService.get()
      .getOrElse(throw new IllegalStateException(s"$myName not yet started"))
      .allocatedThing

  override def toString = s"RestartAfterFailureService(${
    currentAllocatedService.get().fold("not started")(_.allocatedThing.toString)})"

  private def myName = s"RestartAfterFailureService[$serviceName]"


object RestartAfterFailureService:
  private val logger = Logger[this.type]
  private[service] val defaultRestartDelays: Seq[FiniteDuration] =
    Vector(0.s, 1.s, 3.s, 6.s, 10.s)
