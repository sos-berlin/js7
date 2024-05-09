package js7.base.service

import cats.effect.{Deferred, IO, Outcome, Resource, ResourceIO}
import izumi.reflect.Tag
import js7.base.catsutils.CatsDeadline
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.problem.Problem
import js7.base.service.Service.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.*
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
        IO.raiseError(Problem.pure(
          s"$service.untilStopped but service has not been started").throwable)
      else
        stopped.get.flatMap(_
          .fold(IO.raiseError, IO(_)))

  protected final def startServiceAndLog(logger: Logger.Underlying, args: String = "")(run: IO[Unit])
  : IO[Started] =
    startService(
      logInfoStartAndStop(logger, service.toString, args)(
        run))

  /** Run the provided service until it terminates. */
  protected final def startService(run: IO[Unit]): IO[Started] =
    CorrelId
      .bindNew(logger.debugIO(s"$service run"):
        CatsDeadline.now.flatMap: since =>
          run
            .guaranteeCase:
              case Outcome.Errored(t) =>
                since.elapsed
                  .flatMap: elapsed =>
                    IO:
                      val msg = s"$service died after ${elapsed.pretty}: ${t.toStringWithCauses}"
                      if t.isInstanceOf[MainServiceTerminationException] then
                        logger.debug(msg)
                      else
                        // A service should not die
                        logger.error(msg, t.nullIfNoStackTrace)
                  .*>(stopped.complete(Failure(t)))
                  .void

              case Outcome.Canceled() =>
                stopped.complete(Failure(Problem.pure(s"$service canceled").throwable))
                  .void

              case Outcome.Succeeded(_) =>
                stopped.complete(Success(())).void)
      .start
      .flatMap: fiber =>
        service match
          case service: js7.base.service.StoppableByRequest => service.onFiberStarted(fiber)
          case _ => IO.unit
      .as(Started)


object Service:
  private val defaultRestartDelays: Seq[FiniteDuration] =
    RestartAfterFailureService.defaultRestartDelays

  private[service] object Empty extends Service:
    protected lazy val start = startService(IO.unit)
    protected def stop = IO.unit

  private val logger = Logger[this.type]

  def resource[S <: Service](newService: IO[S]): ResourceIO[S] =
    Resource.make(
      acquire =
        newService.flatTap(service =>
          if service.started.getAndSet(true) then
            IO.raiseError(new IllegalStateException(s"$toString started twice"))
          else
            logger.traceF(s"$service start")(
              service.start
                .onError(t =>
                  // Maybe duplicate, but some tests don't propagate this error and silently deadlock
                  IO(logger.error(s"$service start => ${t.toStringWithCauses}"))))))(
      release =
        service => service.stop
          .logWhenItTakesLonger(s"stopping $service"))

  private def logInfoStartAndStop[A](
    logger: Logger.Underlying,
    serviceName: String,
    args: String = "")
    (body: IO[A])
  : IO[A] =
    IO.defer:
      val a = args.nonEmpty ?? s"($args)"
      logger.info(s"$serviceName$a started")
      body.guaranteeCase:
        case Outcome.Errored(_) => IO.unit // start logs the error
        case Outcome.Canceled() => IO(logger.info(s"⚫ $serviceName canceled"))
        case Outcome.Succeeded(_) => IO(logger.info(s"$serviceName stopped"))

  def restartAfterFailure[S <: Service: Tag](
    startDelays: Seq[FiniteDuration] = defaultRestartDelays,
    runDelays: Seq[FiniteDuration] = defaultRestartDelays)
    (serviceResource: ResourceIO[S])
  : ResourceIO[RestartAfterFailureService[S]] =
    resource(IO(
      new RestartAfterFailureService(startDelays, runDelays)(serviceResource)))

  trait StoppableByRequest extends Service, js7.base.service.StoppableByRequest

  trait StoppableByCancel extends StoppableByRequest:
    override protected[service] val stoppableByCancel = true

  /** Marker type to ensure call of `startFiber`. */
  final class Started private[Service]:
    override def toString = "Service.Started"
  private val Started = new Started()
