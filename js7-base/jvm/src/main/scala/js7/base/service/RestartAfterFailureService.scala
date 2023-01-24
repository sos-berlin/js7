package js7.base.service

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import cats.syntax.flatMap.*
import cats.syntax.option.*
import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.service.RestartAfterFailureService.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Delayer.syntax.RichDelayerTask
import js7.base.utils.ScalaUtils.some
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{DelayConf, Delayer}
import monix.eval.Task
import monix.execution.atomic.Atomic
import scala.concurrent.duration.*

final class RestartAfterFailureService[S <: Service: Tag] private[service](
  startDelays: Seq[FiniteDuration] = defaultRestartDelays,
  runDelays: Seq[FiniteDuration] = defaultRestartDelays)
  (serviceResource: Resource[Task, S])
extends Service
{
  self =>

  private val serviceName = implicitly[Tag[S]].tag.toString
  private val currentService = Atomic(none[S])
  @volatile private var stopping = false
  private val untilStopRequested = Deferred.unsafe[Task, Unit]

  private val maybeStartDelay = DelayConf.maybe(startDelays)
  private val maybeRunDelay = DelayConf.maybe(runDelays)

  val stop =
    Task.defer {
      stopping = true
      untilStopRequested.complete(()) *>
        currentService.get().fold(Task.unit)(_.stop)
    }.memoize

  protected def start: Task[Service.Started] =
    for {
      service <- startUnderlyingService
      started <- startService(runUnderlyingService(service))
    } yield started


  private def startUnderlyingService: Task[S] =
    serviceResource
      .acquire
      .pipeMaybe(maybeStartDelay)(
        _.onFailureRestartWithDelayer(_,
          onFailure = t => Task {
            logger.error(s"$serviceName start failed: ${t.toStringWithCauses}")
            for (st <- t.ifStackTrace)
              logger.debug(s"$serviceName start failed: ${t.toStringWithCauses}", st)
          },
          onSleep = logDelay))
      .flatTap(setCurrentService)

  private def setCurrentService(service: S): Task[Unit] =
    Task.defer {
      currentService
        .getAndSet(Some(service))
        .fold(Task.unit)(_.stop.when(stopping))
        .*>(service.stop.when(stopping))
    }

  private def runUnderlyingService(service: S) =
    maybeRunDelay.fold(service.untilStopped)(delayConf =>
      Delayer.start[Task](delayConf).flatMap(delayer =>
        Task.tailRecM(some(service)) { initialService =>
          val service = initialService match {
            case Some(initialService) => Task.pure(initialService) // First iteration
            case None => startUnderlyingService // Following iterations
          }
          service.flatMap(service =>
            service
              .untilStopped
              .map(Right(_))
              .onErrorHandleWith { throwable =>
                // Service has already logged the throwable
                logger.debug(s"💥 $serviceName start failed: ${throwable.toStringWithCauses}",
                  throwable.nullIfNoStackTrace)
                if (stopping)
                  Task.right(())
                else
                  for (_ <- delayer.sleep(logDelay)) yield
                    if (stopping)
                      Right(()) // finish tailRecM
                    else
                      Left(None) /*loop*/
              })
        }))

  private def logDelay(duration: FiniteDuration) =
    Task(logger.info(
      "Due to failure, " + serviceName + " restarts " +
        (if (duration.isZero) "now" else "in " + duration.pretty)))

  // Call only after this Service has been started!
  def unsafeCurrentService(): S =
    currentService.get()
      .getOrElse(throw new IllegalStateException(s"$myName not yet started"))

  override def toString =
    currentService.get().fold(myName)(o => s"RestartAfterFailureService($o)")

  private def myName = s"RestartAfterFailureService[$serviceName]"
}

object RestartAfterFailureService
{
  private val logger = Logger(getClass)
  private[service] val defaultRestartDelays: Seq[FiniteDuration] =
    Vector(0.s, 1.s, 3.s, 6.s, 10.s)
}
