package js7.common.system.startup

import cats.effect.{Resource, Sync, SyncIO}
import cats.syntax.flatMap.*
import com.typesafe.config.Config
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.log.{CorrelId, Logger}
import js7.base.service.Service
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.SyncResource.syntax.RichResource
import js7.common.system.ThreadPools
import js7.common.system.startup.JavaMain.shutdownHookResource
import monix.eval.Task
import monix.execution.Scheduler

object StatefulServiceMain
{
  coupleScribeWithSlf4j()

  private lazy val logger = Logger[this.type]

  def run[S <: Service](
    name: String,
    config: Config,
    commonScheduler: Option[Scheduler] = None,
    serviceResource: Scheduler => Resource[Task, S],
    use: S => Task[Unit] = (_: S).untilStopped)
  : Unit =
    resource(name, config, commonScheduler, serviceResource)
      .useSync { case (scheduler, serviceResource) =>
        implicit val s = scheduler
        serviceResource
          .use(use)
          .runSyncUnsafe()
      }

  private def resource[S <: Service](
    name: String,
    config: Config,
    commonScheduler: Option[Scheduler] = None,
    serviceResource: Scheduler => Resource[Task, S])
  : Resource[SyncIO, (Scheduler, Resource[Task, S])] =
    threadPoolResource[SyncIO](name, config, commonScheduler)
      .map(implicit scheduler =>
        scheduler -> serviceResource(scheduler)
          .flatTap(service =>
            shutdownHookResource[Task](config, name)(
              onJavaShutdown(service))))

  private def threadPoolResource[F[_]: Sync](
    name: String,
    config: Config,
    commonScheduler: Option[Scheduler])
  : Resource[F, Scheduler] =
    commonScheduler
      .map(scheduler => Resource.eval(Sync[F].delay(CorrelId.enableScheduler(scheduler))))
      .getOrElse(
        ThreadPools.standardSchedulerResource[F](name, config))

  private def onJavaShutdown[S <: Service](service: S)(implicit s: Scheduler): Unit = {
    logger.warn(s"Trying to shut down JS7 $service due to Java shutdown")
    service
      .stop
      .runAsyncUncancelable {
        case Left(throwable) => logger.error(s"onJavaShutdown: ${throwable.toStringWithCauses}",
          throwable.nullIfNoStackTrace)
        case Right(_) =>
      }
    service.untilStopped.runToFuture.awaitInfinite
  }
}
