package js7.common.system.startup

import cats.effect.{Resource, Sync, SyncIO}
import com.typesafe.config.Config
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.log.{CorrelId, Logger}
import js7.base.service.{MainService, Service}
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.{Allocated, ProgramTermination}
import js7.common.system.ThreadPools
import js7.common.system.startup.JavaMain.shutdownHookResource
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object ServiceMain
{
  coupleScribeWithSlf4j()

  private lazy val logger = Logger[this.type]

  def blockingRun[S <: MainService](
    name: String,
    config: Config,
    serviceResource: Scheduler => Resource[Task, S],
    use: S => Task[ProgramTermination] = (_: S).untilTerminated,
    timeout: Duration = Duration.Inf)
  : ProgramTermination =
    threadPoolResource[SyncIO](name, config)
      .use(implicit scheduler => SyncIO {
        serviceResource(scheduler)
          .toAllocated
          .flatMap(allocated =>
            shutdownHookResource[Task](config, name)(onJavaShutdown(allocated))
              .use(_ =>
                use(allocated.allocatedThing))
              .guarantee(allocated.stop))
          .runSyncUnsafe(timeout)/*blocking*/
      }).unsafeRunSync()

  private def threadPoolResource[F[_]: Sync](
    name: String,
    config: Config,
    commonScheduler: Option[Scheduler] = None)
  : Resource[F, Scheduler] =
    commonScheduler
      .map(scheduler => Resource.eval(Sync[F].delay(CorrelId.enableScheduler(scheduler))))
      .getOrElse(
        ThreadPools.standardSchedulerResource[F](name, config))

  private def onJavaShutdown[S <: Service](allocatedService: Allocated[Task, S])
    (implicit s: Scheduler)
  : Unit = {
    logger.warn(s"Trying to shut down JS7 $allocatedService due to Java shutdown")
    Await.ready(allocatedService.stop.runToFuture, Duration.Inf).value.get match {
      case Failure(throwable) =>
        logger.error(s"onJavaShutdown: ${throwable.toStringWithCauses}",
          throwable.nullIfNoStackTrace)
      case Success(_) =>
    }
  }
}
