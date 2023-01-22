package js7.base.service

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import cats.syntax.flatMap.*
import cats.syntax.option.*
import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.service.RestartAfterFailureService.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.repeatLast
import js7.base.utils.ScalaUtils.some
import js7.base.utils.ScalaUtils.syntax.*
import monix.eval.Task
import monix.execution.Scheduler
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

  val stop =
    Task.defer {
      stopping = true
      untilStopRequested.complete(()) *>
        currentService.get().fold(Task.unit)(_.stop)
    }.memoize

  protected def start: Task[Service.Started] =
    Task.deferAction(implicit scheduler =>
      for {
        service <- startUnderlyingService
        started <- startService(
          Task.tailRecM((some(service), now, repeatLast(runDelays))) {
            case (initialService, since, delays) =>
              val service = initialService match {
                case Some(initialService) => Task.pure(initialService)
                case None => startUnderlyingService
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
                      delayAfterFailure(since, delays, runDelays)
                        .map(delays =>
                          if (stopping)
                            Right(())
                          else
                            Left((None, now, delays)))
                  })
          })
      } yield started)

  private def startUnderlyingService(implicit scheduler: Scheduler): Task[S] =
    Task
      .defer(serviceResource
        .startService
        .onErrorRestartLoop((now, repeatLast(startDelays))) {
          case (throwable, (since, delays), retry) =>
            logThrowable(throwable)
            delayAfterFailure(since, delays, startDelays)
              .flatMap(delays => retry((scheduler.now, delays)))
        })
      .flatTap(service => setCurrentService(service))

  private def logThrowable(throwable: Throwable): Unit = {
    logger.error(s"$serviceName start failed: ${throwable.toStringWithCauses}")
    for (st <- throwable.ifStackTrace)
      logger.debug(s"$serviceName start failed: ${throwable.toStringWithCauses}", st)
  }

  private def delayAfterFailure(
    since: MonixDeadline,
    delays: LazyList[FiniteDuration],
    newDelays: Seq[FiniteDuration])
  : Task[LazyList[FiniteDuration]] = {
    val duration = since.elapsed
    val delay = (delays.head - duration) max ZeroDuration
    // Throwable has already been error-logged by caller (start) or Service (run)
    logger.info(s"Due to failure, $serviceName restarts ${
      if (delay.isZero) "now" else s"in ${delay.pretty}"}")
    Task.race(untilStopRequested.get, Task.sleep(delay)) *>
      Task(
        if (duration >= newDelays.last)
          repeatLast(newDelays) // reset restartDelays
        else
          delays.tail)
  }

  private  def setCurrentService(service: S): Task[Unit] =
    Task.defer {
      currentService
        .getAndSet(Some(service))
        .fold(Task.unit)(_.stop.when(stopping))
        .*>(service.stop.when(stopping))
    }

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
