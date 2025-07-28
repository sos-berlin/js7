package js7.base.service

import cats.effect.{Deferred, ExitCode, IO, Outcome, Resource, ResourceIO}
import izumi.reflect.Tag
import js7.base.catsutils.CatsDeadline
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Problem
import js7.base.service.Service.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Atomic, DelayConf, ProgramTermination}
import scala.util.{Failure, Success, Try}

trait Service:
  service =>

  private val started = Atomic(false)
  private val stopped = Deferred.unsafe[IO, Try[Unit]]

  protected def start: IO[Started]
  protected def stop: IO[Unit]

  /** Returns an error when Service has failed. */
  final def untilStopped: IO[Unit] =
    IO.defer:
      if !started.get() then
        IO.raiseError:
          Problem.pure(s"$service.untilStopped but service has not been started").throwable
      else
        stopped.get.flatMap(_
          .fold(IO.raiseError, IO(_)))

  /** When this Service stopps, cancel the body and fail. */
  final def failWhenStopped[R](body: IO[R]): IO[R] =
    body.start.flatMap: fiber =>
      IO
        .race(
          service.untilStopped.attempt.flatMap: attempted =>
            fiber.cancel *>
              attempted.match
                case Left(throwable) => IO.raiseError(throwable)
                case Right(()) => IO.unit,
          fiber.joinWithNever)
        .flatMap:
          case Left(()) => IO.raiseError(new RuntimeException(s"$service terminated unexpectedly"))
          case Right(r) => IO.pure(r)

  /** Like `startService`, and logs start and stop at info level. */
  protected final def startServiceAndLog(logger: Logger, args: String = "")(run: IO[Unit])
  : IO[Started] =
    startService:
      logInfoStartAndStop(logger, args):
        run

  private def logInfoStartAndStop[A](logger: Logger, args: String = "")(body: IO[A])
  : IO[A] =
    IO.defer:
      logger.info(s"$service${args.nonEmpty ?? s"($args)"} started")
      body.guaranteeCase:
        case Outcome.Errored(_) => IO.unit // startService has logged the error
        case Outcome.Canceled() => IO(logger.info(s"◼️ $service canceled"))
        case Outcome.Succeeded(_) => IO(logger.info(s"$service stopped"))

  /** Run the provided service until it terminates. */
  protected final def startService(run: IO[Unit]): IO[Started] =
    CorrelId.bindNew:
      CatsDeadline.now.flatMap: since =>
        logger.debugIO(s"$service run"):
          run
        .guaranteeCase:
          case Outcome.Errored(t) =>
            since.elapsed
              .flatMap: elapsed =>
                IO:
                  val msg = s"$service died after ${elapsed.pretty}: ${t.toStringWithCauses}"
                  if !t.isInstanceOf[MainServiceTerminationException] then
                    // A service should not die. The caller should watch untilStopped!
                    logger.error(msg, t.nullIfNoStackTrace)
              .*>(stopped.complete(Failure(t)))
              .void

          case Outcome.Canceled() =>
            stopped.complete(Failure(Problem.pure(s"$service canceled").throwable))
              .void

          case Outcome.Succeeded(_) =>
            stopped.complete(Success(())).void
    .start
    .flatMap: fiber =>
      service match
        case service: js7.base.service.StoppableByRequest => service.onFiberStarted(fiber)
        case _ => IO.unit
    .as(Started)


object Service:

  private val logger = Logger[this.type]
  private val defaultRestartDelayConf: DelayConf =
    RestartAfterFailureService.defaultRestartConf

  def resource[Svc <: Service](newService: => Svc): ResourceIO[Svc] =
    resource(IO(newService))

  def resource[Svc <: Service](newService: IO[Svc]): ResourceIO[Svc] =
    Resource.make(
      acquire =
        newService.flatTap: service =>
          if service.started.getAndSet(true) then
            IO.raiseError(IllegalStateException(s"$toString service started twice"))
          else
            logger.traceF(s"$service start"):
              service.start
                .onError: t =>
                  IO:
                    // Maybe duplicate, but some tests don't propagate this error and silently deadlock
                    logger.error(s"$service start: ${t.toStringWithCauses}", t.nullIfNoStackTrace))(
      release = service =>
        service.stop.logWhenItTakesLonger(s"stopping $service"))

  def restartAfterFailure[Svc <: Service: Tag](
    restartDelayConf: DelayConf = defaultRestartDelayConf,
    runDelayConf: DelayConf = defaultRestartDelayConf)
    (serviceResource: ResourceIO[Svc])
  : ResourceIO[RestartAfterFailureService[Svc]] =
    resource:
      new RestartAfterFailureService(Some(restartDelayConf), Some(runDelayConf))(serviceResource)

  def simple(body: IO[Unit | ExitCode | ProgramTermination]): SimpleMainService =
    new SimpleMainService with StoppableByCancel:
      def run =
        body.map(ProgramTermination.fromUnitOrExitCode)

  trait StoppableByRequest extends Service, js7.base.service.StoppableByRequest

  trait StoppableByCancel extends StoppableByRequest:
    override protected[service] val stoppableByCancel = true


  /** Marker type to ensure call of `startService`. */
  final class Started private[Service]:
    override def toString = "Service.Started"

  private val Started: Started = new Started


  //type Empty = Empty.type
  //object Empty extends Service:
  //  def resource: ResourceIO[Empty] =
  //    Service.resource(IO.pure(Empty))
  //
  //  protected lazy val start = startService(IO.unit)
  //
  //  protected def stop = IO.unit
