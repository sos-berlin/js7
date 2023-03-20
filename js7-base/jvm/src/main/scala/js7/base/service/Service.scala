package js7.base.service

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCase, Resource}
import cats.syntax.flatMap.*
import com.typesafe.scalalogging.Logger as ScalaLogger
import izumi.reflect.Tag
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.problem.Problem
import js7.base.service.Service.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import monix.eval.Task
import monix.execution.atomic.Atomic
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

trait Service extends AnyRef {
  service =>

  private val started = Atomic(false)
  private val stopped = Deferred.unsafe[Task, Try[Unit]]

  protected def start: Task[Started]
  protected def stop: Task[Unit]

  final def untilStopped: Task[Unit] =
    Task.defer {
      if (!started.get())
        Task.raiseError(Problem.pure(
          s"$service.untilStopped but service has not been started").throwable)
      else
        stopped.get.flatMap(_
          .fold(Task.raiseError, Task(_)))
    }

  protected final def startServiceAndLog(logger: ScalaLogger, args: String = "")(run: Task[Unit])
  : Task[Started] =
    startService(
      logInfoStartAndStop(logger, service.toString, args)(
        run))

  protected final def startService(run: Task[Unit]): Task[Started] =
    CorrelId
      .bindNew(logger.debugTask(s"$service run")(
        Task.deferAction(scheduler => Task(scheduler.now)).flatMap(since =>
          run
            .guaranteeCase {
              case ExitCase.Error(t) =>
                // A service should not die
                logger.error(s"$service died after ${since.elapsed.pretty}: ${t.toStringWithCauses}")
                stopped.complete(Failure(t))

              case ExitCase.Canceled =>
                stopped.complete(Failure(Problem.pure(s"$service canceled").throwable))

              case ExitCase.Completed =>
                stopped.complete(Success(()))
            })))
      .start
      .flatMap(fiber =>
        service match {
          case service: StoppableByRequest => service.onFiberStarted(fiber)
          case _ => Task.unit
        })
      .as(Started)
}

object Service
{
  val defaultRestartDelays: Seq[FiniteDuration] =
    RestartAfterFailureService.defaultRestartDelays

  private[service] object Empty extends Service {
    protected val start = startService(Task.unit)
    protected def stop = Task.unit
  }

  private val logger = Logger[this.type]

  def resource[S <: Service](newService: Task[S]): Resource[Task, S] =
    Resource.make(
      acquire = startService(newService))(
      release = service => service.stop.logWhenItTakesLonger(s"stopping $service"))

  private def startService[S <: Service](newService: Task[S]): Task[S] =
    newService.flatTap(service =>
      if (service.started.getAndSet(true))
        Task.raiseError(Problem.pure(s"$toString started twice").throwable)
      else
        logger.traceTask(s"$service start")(
          service.start))

  private def logInfoStartAndStop[A](
    logger: ScalaLogger,
    serviceName: String,
    args: String = "")
    (task: Task[A])
  : Task[A] =
    Task.defer {
      val a = args.nonEmpty ?? s"($args)"
      logger.info(s"$serviceName$a started")
      task.guaranteeCase {
        case ExitCase.Error(_) => Task.unit // startService logs the error
        case ExitCase.Canceled => Task(logger.info(s"❌ $serviceName canceled"))
        case ExitCase.Completed => Task(logger.info(s"$serviceName stopped"))
      }
    }

  def restartAfterFailure[S <: Service: Tag](
    startDelays: Seq[FiniteDuration] = defaultRestartDelays,
    runDelays: Seq[FiniteDuration] = defaultRestartDelays)
    (serviceResource: Resource[Task, S])
  : Resource[Task, RestartAfterFailureService[S]] =
      resource(Task(
        new RestartAfterFailureService(startDelays, runDelays)(serviceResource)))

  trait StoppableByRequest extends Service with js7.base.service.StoppableByRequest

  /** Marker type to ensure call of `startFiber`. */
  final class Started private[Service] {
    override def toString = "Service.Started"
  }
  private val Started = new Started()
}
