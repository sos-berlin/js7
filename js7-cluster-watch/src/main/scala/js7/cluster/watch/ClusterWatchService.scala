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
import js7.cluster.watch.ClusterWatch.Confirmed
import js7.cluster.watch.ClusterWatchService.*
import js7.cluster.watch.api.ClusterWatchProblems.ClusterWatchRequestDoesNotMatchProblem
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.configuration.Js7Configuration.defaultConfig
import js7.data.cluster.ClusterEvent.ClusterNodeLostEvent
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterState, ClusterWatchId, ClusterWatchRequest, ClusterWatchRunId}
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
  retryDelays: NonEmptySeq[FiniteDuration],
  val eventBus: ClusterWatchEventBus)
extends Service.StoppableByRequest
{
  private val clusterWatch = new ClusterWatch(now, eventBus = eventBus)
  val clusterWatchRunId = ClusterWatchRunId.random()
  private val delayConf = DelayConf(retryDelays, resetWhen = retryDelays.last)

  protected def start =
    startServiceAndLog(logger, nodeApis.toList.mkString(", "))(
      run)

  private def run: Task[Unit] =
    Observable
      .fromIterable(
        for (nodeApi <- nodeApis.toList) yield {
          val nodeWatch = new NodeServer(nodeApi)
          nodeWatch.observable.map(nodeWatch -> _)
        })
      .merge
      .mapEval { case (nodeWatch, request) =>
        // Synchronize requests from both nodes
        request.correlId.bind(
          nodeWatch.processRequest(request))
      }
      .takeUntilEval(untilStopRequested)
      .completedL

  private final class NodeServer(nodeApi: HttpClusterNodeApi) {
    def observable: Observable[ClusterWatchRequest] =
      observeAgainAndAgain(clusterWatchRequestObservable)

    private def observeAgainAndAgain[A](observable: Observable[A]): Observable[A] =
      Delayer.observable[Task](delayConf)
        .flatMap(_ =>
          observable
            .onErrorHandleWith { t =>
              logger.warn(s"⟲ $nodeApi => ${t.toStringWithCauses}")
              Observable.empty
            })

    private def clusterWatchRequestObservable: Observable[ClusterWatchRequest] =
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

    def processRequest(request: ClusterWatchRequest): Task[Unit] =
      Task(clusterWatch.processRequest(request))
        .flatMap(respond(request, _))

    private def respond(request: ClusterWatchRequest, confirmed: Checked[Confirmed]): Task[Unit] =
      HttpClient
        .liftProblem(nodeApi
          .retryIfSessionLost()(nodeApi
            .executeClusterWatchingCommand(
              ClusterWatchConfirm(
                request.requestId, clusterWatchId, clusterWatchRunId,
                manualConfirmer = confirmed.toOption.flatMap(_.manualConfirmer),
                problem = confirmed.left.toOption))
            .void))
        .map {
          case Left(problem @ ClusterWatchRequestDoesNotMatchProblem) =>
            // Already confirmed by this or another ClusterWatch
            logger.info(s"❓$nodeApi $problem")
          case Left(problem) =>
            logger.warn(s"❓$nodeApi $problem")
          case Right(()) =>
        }
        .onErrorHandle(t =>
          logger.error(s"$nodeApi ${t.toStringWithCauses}", t.nullIfNoStackTrace))
  }

  def manuallyConfirmNodeLoss(lostNodeId: NodeId, confirmer: String): Checked[Unit] =
    clusterWatch.manuallyConfirmNodeLoss(lostNodeId, confirmer)

  def clusterNodeLossEventToBeConfirmed(lostNodeId: NodeId): Option[ClusterNodeLostEvent] =
    clusterWatch.clusterNodeLossEventToBeConfirmed(lostNodeId)

  override def toString = clusterWatchId.toString

  def clusterState(): Checked[ClusterState] =
    clusterWatch.clusterState()
}

object ClusterWatchService
{
  private val logger = Logger(getClass)

  def resource(
    clusterWatchId: ClusterWatchId,
    apiResources: Resource[Task, Nel[HttpClusterNodeApi]],
    config: Config,
    eventBus: ClusterWatchEventBus = new ClusterWatchEventBus)
  : Resource[Task, ClusterWatchService] =
    resource2(clusterWatchId, apiResources, config.withFallback(defaultConfig), eventBus)

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
    config: Config,
    eventBus: ClusterWatchEventBus = new ClusterWatchEventBus)
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
              retryDelays = NonEmptySeq.fromSeq(retryDelays) getOrElse NonEmptySeq.of(1.s),
                eventBus)))))
    })
}
