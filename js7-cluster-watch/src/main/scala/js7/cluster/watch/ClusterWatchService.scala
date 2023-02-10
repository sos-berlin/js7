package js7.cluster.watch

import cats.data.NonEmptySeq
import cats.effect.Resource
import com.typesafe.config.Config
import js7.base.configutils.Configs.RichConfig
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.problem.Checked
import js7.base.service.{RestartAfterFailureService, Service}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.utils.{DelayConf, Delayer}
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import js7.cluster.watch.ClusterWatchService.*
import js7.cluster.watch.api.ClusterWatchProblems.ClusterWatchRequestDoesNotMatchProblem
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.configuration.Js7Configuration.defaultConfig
import js7.data.cluster.ClusterEvent.ClusterFailedOver
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterNodeApi, ClusterState, ClusterWatchId, ClusterWatchRequest, ClusterWatchRunId}
import js7.data.node.NodeId
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
  retryDelays: NonEmptySeq[FiniteDuration])
extends Service.StoppableByRequest
{
  private val clusterWatch = new ClusterWatch(now)
  val clusterWatchRunId = ClusterWatchRunId.random()
  private val delayConf = DelayConf(retryDelays, resetWhen = retryDelays.last)

  protected def start =
    Task.defer {
      logger.info(
        s"↘ $clusterWatchId is starting for ${nodeApis.toList.mkString(", ")} ($clusterWatchRunId) ...")
      startService(
        Observable
          .fromIterable(
            for (nodeApi <- nodeApis.toList) yield
              observeAgainAndAgain(nodeApi)(
                clusterWatchRequestObservable(nodeApi).map(nodeApi -> _)))
          .merge
          .mapEval { case (nodeApi, msg) =>
            processRequest(nodeApi, msg)
          }
        .takeUntilEval(untilStopRequested)
          .completedL
          .*>(Task(logger.info(
            s"↙ $clusterWatchId started for ${nodeApis.toList.mkString(", ")}"))))
    }

  private def observeAgainAndAgain[A](nodeApi: ClusterNodeApi)(observable: Observable[A])
  : Observable[A] =
    Delayer.observable[Task](delayConf)
      .flatMap(_ =>
        observable
          .onErrorHandleWith { t =>
            logger.warn(s"⟲ $nodeApi => ${t.toStringWithCauses}")
            Observable.empty
          })

  private def clusterWatchRequestObservable(nodeApi: ClusterNodeApi): Observable[ClusterWatchRequest] =
    logger.traceObservable("clusterWatchRequestObservable", nodeApi)(
      Observable
        .fromTask(nodeApi
          .retryUntilReachable()(
            nodeApi.retryIfSessionLost()(
              nodeApi.clusterWatchRequestObservable(clusterWatchId, keepAlive = Some(keepAlive))))
          .materialize.map {
            case Failure(t: HttpException) if t.statusInt == 503 /*Service unavailable*/ =>
              Failure(t) // Trigger onErrorRestartLoop
            case o => HttpClient.failureToChecked(o)
          }
          .dematerialize
          .map(_.orThrow))
        .flatten)

  private def processRequest(nodeApi: HttpClusterNodeApi, request: ClusterWatchRequest): Task[Unit] =
    /*request.correlId.bind — better log the ClusterWatch's CorrelId in Cluster node*/(
    logger.debugTask("processRequest", request)(
      Task(clusterWatch.processRequest(request)).flatMap(checked =>
        HttpClient
          .liftProblem(nodeApi
            .retryIfSessionLost()(nodeApi
              .executeClusterWatchingCommand(
                ClusterWatchConfirm(
                  request.requestId, clusterWatchId, clusterWatchRunId,
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
            logger.error(s"$nodeApi ${t.toStringWithCauses}", t.nullIfNoStackTrace)))))

  def confirmNodeLoss(lostNodeId: NodeId): Checked[Unit] =
    clusterWatch.confirmNodeLoss(lostNodeId)

  override def toString = "ClusterWatchService"

  def clusterState(): Checked[ClusterState] =
    clusterWatch.clusterState()

  def clusterFailedOverRequested(): Option[ClusterFailedOver] =
    clusterWatch.clusterFailedOverRequested()
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

  def restartableResource(
    clusterWatchId: ClusterWatchId,
    apiResources: Resource[Task, Nel[HttpClusterNodeApi]],
    config: Config)
  : Resource[Task, RestartAfterFailureService[ClusterWatchService]] =
    Resource.suspend(Task {
      val cfg = config.withFallback(defaultConfig)
      val startDelays = Nil // Do not restart on start failure
      val runDelays = cfg
        .getDurationList("js7.journal.cluster.watch.restart-after-failure-delays")
        .asScala.map(_.toFiniteDuration).toVector

      Service.restartAfterFailure(startDelays = startDelays, runDelays = runDelays)(
        resource2(clusterWatchId, apiResources, cfg))
    })

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
                retryDelays = NonEmptySeq.fromSeq(retryDelays) getOrElse NonEmptySeq.of(1.s))))))
    })
}
