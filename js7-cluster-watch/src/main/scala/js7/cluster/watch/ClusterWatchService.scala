package js7.cluster.watch

import cats.effect.Resource
import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.problem.Checked
import js7.base.service.Service
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import js7.cluster.watch.ClusterWatchService.*
import js7.cluster.watch.api.ClusterWatchProblems.ClusterWatchRequestDoesNotMatchProblem
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.configuration.Js7Configuration.defaultConfig
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterNodeApi, ClusterState, ClusterWatchId, ClusterWatchMessage, ClusterWatchRequest, ClusterWatchRunId}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*
import scala.util.Failure

final class ClusterWatchService private[ClusterWatchService](
  val clusterWatchId: ClusterWatchId,
  nodeApis: Nel[HttpClusterNodeApi],
  now: () => MonixDeadline,
  keepAlive: FiniteDuration,
  retryDelays: Seq[FiniteDuration])
extends Service.StoppableByRequest
{
  private val clusterWatch = new ClusterWatch(now)
  val clusterWatchRunId = ClusterWatchRunId.random()

  protected def start =
    Task.defer {
      logger.info(s"$clusterWatchId ($clusterWatchRunId) for ${nodeApis.toList.mkString(", ")}")
      startService(
        Observable
          .fromIterable(
            for (nodeApi <- nodeApis.toList) yield
              observeAgainAndAgain(nodeApi)(
                nodeObservable(nodeApi).map(nodeApi -> _)))
          .merge
          .mapEval { case (nodeApi, msg) =>
            handleMessage(nodeApi, msg)
          }
        .takeUntilEval(untilStopRequested)
          .completedL
          .guaranteeCase(exitCase => Task(
            logger.info(s"$clusterWatchId for ${nodeApis.toList.mkString(", ")} => $exitCase"))))
    }

  private def observeAgainAndAgain[A](nodeApi: ClusterNodeApi)(observable: Observable[A])
  : Observable[A] =
    Observable.tailRecM(())(_ =>
      Observable
        .pure(Right(observable
          .onErrorHandleWith { t =>
            logger.warn(s"$nodeApi => ${t.toStringWithCauses}")
            Observable.empty[A]
          }))
        .appendAll(
          Observable.evalDelayed(
            1.s /*???*/,
            Left(())))
    ).flatten

  private def nodeObservable(nodeApi: ClusterNodeApi): Observable[ClusterWatchMessage] =
    Observable
      .fromTask(logger.traceTask("nodeObservable", nodeApi)(
        nodeApi
          .retryUntilReachable()(
            nodeApi.retryIfSessionLost()(
              nodeApi.clusterWatchMessageObservable(clusterWatchId, keepAlive = Some(keepAlive))))
          .materialize.map {
            case Failure(t: HttpException) if t.statusInt == 503 /*Service unavailable*/ =>
              Failure(t) // Trigger onErrorRestartLoop
            case o => HttpClient.failureToChecked(o)
          }
          .dematerialize
          .map(_.orThrow)))
      .flatten

  private def handleMessage(nodeApi: HttpClusterNodeApi, msg: ClusterWatchMessage): Task[Unit] =
    msg.correlId.bind(logger.traceTask("handleMessage", msg)(Task.defer(
      clusterWatch.handleMessage(msg).flatMap(checked =>
        msg match {
          case msg: ClusterWatchRequest =>
            HttpClient
              .liftProblem(nodeApi
                .retryIfSessionLost()(nodeApi
                  .executeClusterWatchingCommand(
                    ClusterWatchConfirm(
                      msg.requestId, clusterWatchId, clusterWatchRunId,
                      checked.left.toOption))
                  .void))
              .map {
                case Left(problem @ ClusterWatchRequestDoesNotMatchProblem) =>
                  // Already confirmed by this or another ClusterWatch
                  logger.info(s"$nodeApi $problem")
                case Left(problem) =>
                  logger.warn(s"$nodeApi $problem")
                case Right(()) =>
              }
              .onErrorHandle(t =>
                logger.error(s"$nodeApi ${t.toStringWithCauses}", t.nullIfNoStackTrace))
          case _ =>
            for (problem <- checked.left) logger.warn(s"$nodeApi $problem — msg=$msg")
            Task.unit
        }))))

  override def toString = "ClusterWatchService"

  def unsafeClusterState(): Checked[ClusterState] =
    clusterWatch.unsafeClusterState()
}

object ClusterWatchService
{
  private val logger = Logger(getClass)

  def resource(
    clusterWatchId: ClusterWatchId,
    apiResources: Resource[Task, Nel[HttpClusterNodeApi]],
    config: Config)
  : Resource[Task, ClusterWatchService] =
    resource2(clusterWatchId, apiResources, config.withFallback(defaultConfig))

  private def resource2(
    clusterWatchId: ClusterWatchId,
    apiResources: Resource[Task, Nel[HttpClusterNodeApi]],
    config: Config)
  : Resource[Task, ClusterWatchService] =
    Resource.suspend(Task {
      val keepAlive = config.finiteDuration("js7.web.client.keep-alive").orThrow
      val retryDelays = config.getDurationList("js7.journal.cluster.watch.retry-delays")
        .asScala.map(_.toFiniteDuration).toVector
      apiResources
        .flatMap(nodeApis =>
          Service.resource(
            Task.deferAction(scheduler => Task(
              new ClusterWatchService(
                clusterWatchId,
                nodeApis,
                () => scheduler.now,
                keepAlive = keepAlive,
                retryDelays = retryDelays)))))
    })
}
