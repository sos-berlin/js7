package js7.cluster.watch

import cats.data.NonEmptyList
import cats.effect.{IO, Resource, ResourceIO}
import com.typesafe.config.Config
import fs2.Stream
import js7.base.catsutils.CatsEffectExtensions.left
import js7.base.configutils.Configs.RichConfig
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.service.{MainService, Service}
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.CatsUtils.syntax.mkString
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.utils.{Atomic, DelayConf, Delayer, ProgramTermination}
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import js7.cluster.watch.ClusterWatch.{Confirmed, OnUndecidableClusterNodeLoss}
import js7.cluster.watch.ClusterWatchService.*
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.configuration.Js7Configuration.defaultConfig
import js7.common.http.PekkoHttpClient
import js7.common.pekkohttp.web.MinimumWebServer
import js7.common.pekkoutils.Pekkos.actorSystemResource
import js7.data.cluster.ClusterEvent.ClusterNodeLostEvent
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchProblems.ClusterWatchRequestDoesNotMatchProblem
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterState, ClusterWatchId, ClusterWatchRequest, ClusterWatchRunId}
import js7.data.node.NodeId
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.FiniteDuration

final class ClusterWatchService private[ClusterWatchService](
  val clusterWatchId: ClusterWatchId,
  nodeApis: Nel[HttpClusterNodeApi],
  label: String,
  keepAlive: FiniteDuration,
  retryDelays: NonEmptyList[FiniteDuration],
  onClusterStateChanged: (HasNodes) => Unit,
  onUndecidableClusterNodeLoss: OnUndecidableClusterNodeLoss)
extends MainService, Service.StoppableByRequest:

  protected type Termination = ProgramTermination

  // Public for test
  val clusterWatch = new ClusterWatch(
    label = label,
    onClusterStateChanged = onClusterStateChanged,
    onUndecidableClusterNodeLoss = onUndecidableClusterNodeLoss)
  val clusterWatchRunId: ClusterWatchRunId = ClusterWatchRunId.random()
  private val delayConf = DelayConf(retryDelays, resetWhen = retryDelays.last)

  protected def start =
    startServiceAndLog(logger, nodeApis.mkString(", ")):
      run

  val untilTerminated: IO[ProgramTermination] =
    untilStopped.as(ProgramTermination())

  private def run: IO[Unit] =
    Stream
      .iterable:
        for nodeApi <- nodeApis.toList yield
          val nodeWatch = new NodeServer(nodeApi)
          nodeWatch.stream.map(nodeWatch -> _)
      .parJoinUnbounded
      .evalMap: (nodeWatch, request) =>
        // Synchronize requests from both nodes
        request.correlId.bind:
          nodeWatch.processRequest(request)
      .interruptWhenF(untilStopRequested)
      .compile
      .drain


  private final class NodeServer(nodeApi: HttpClusterNodeApi):
    private val streamFailed = Atomic(false)

    def stream: Stream[IO, ClusterWatchRequest] =
      streamAgainAndAgain(clusterWatchRequestStream)

    private def streamAgainAndAgain[A](stream: Stream[IO, A]): Stream[IO, A] =
      Delayer.stream[IO](delayConf).flatMap: _ =>
        stream.handleErrorWith: t =>
          logger.warn(s"🔴 $nodeApi => ${t.toStringWithCauses}")
          streamFailed := true
          Stream.empty
      //.interruptWhenF(untilStopRequested)

    private def clusterWatchRequestStream: Stream[IO, ClusterWatchRequest] =
      logger.traceStream("clusterWatchRequestStream", nodeApi):
        Stream
          .eval(nodeApi
            .retryUntilReachable():
              nodeApi.clusterWatchRequestStream(clusterWatchId, keepAlive = Some(keepAlive))
                /*.map(_.interruptWhenF(untilStopRequested))*/)
            .attempt.evalMap:
              case Left(t: HttpException) if t.statusInt == 503 /*Service unavailable*/ =>
                IO.left(t) // Trigger onErrorRestartLoop
              case attempted =>
                IO:
                  if attempted.isRight && streamFailed.getAndSet(false) then
                    logger.info(s"🟢 $nodeApi is being watched again")
                  HttpClient.attemptedToChecked(attempted)
            .rethrow
            .map(_.orThrow)
          .flatten

    def processRequest(request: ClusterWatchRequest): IO[Unit] =
      clusterWatch.processRequest(request)
        .flatMap(respond(request, _))

    private def respond(request: ClusterWatchRequest, confirmed: Checked[Confirmed]): IO[Unit] =
      HttpClient
        .liftProblem:
          nodeApi.retryIfSessionLost:
            nodeApi.executeClusterWatchingCommand:
              ClusterWatchConfirm(
                request.requestId, clusterWatchId, clusterWatchRunId,
                manualConfirmer = confirmed.toOption.flatMap(_.manualConfirmer),
                problem = confirmed.left.toOption)
            .void
        .map:
          case Left(problem @ ClusterWatchRequestDoesNotMatchProblem) =>
            // Already confirmed by this or another ClusterWatch
            logger.info(s"❓$nodeApi $problem")
          case Left(problem) =>
            logger.warn(s"❓$nodeApi $problem")
          case Right(()) =>
        .handleError: t =>
          logger.error(s"$nodeApi ${t.toStringWithCauses}", t.nullIfNoStackTrace)

  end NodeServer


  def manuallyConfirmNodeLoss(lostNodeId: NodeId, confirmer: String): IO[Checked[Unit]] =
    clusterWatch.manuallyConfirmNodeLoss(lostNodeId, confirmer)

  def clusterNodeLossEventToBeConfirmed(lostNodeId: NodeId): Option[ClusterNodeLostEvent] =
    clusterWatch.clusterNodeLossEventToBeConfirmed(lostNodeId)

  override def toString = s"ClusterWatchService($clusterWatchId)"

  def clusterState(): Checked[ClusterState] =
    clusterWatch.clusterState()


object ClusterWatchService:
  private val logger = Logger[this.type]

  def programResource(conf: ClusterWatchConf): ResourceIO[ClusterWatchService] =
    import conf.{clusterNodeAdmissions, config, httpsConfig}
    for
      given ActorSystem <- actorSystemResource(name = "ClusterWatch", config)
      _ <- MinimumWebServer.service(conf)
      service <- resource(
        conf.clusterWatchId,
        apisResource = clusterNodeAdmissions
          .traverse(admission => PekkoHttpClient
            .resource(admission.uri, uriPrefixPath = "", httpsConfig, name = "ClusterNode")
            .flatMap(HttpClusterNodeApi.resource(admission, _, uriPrefix = "controller"))),
        config)
    yield service

  def resource(
    clusterWatchId: ClusterWatchId,
    apisResource: ResourceIO[Nel[HttpClusterNodeApi]],
    config: Config,
    label: String = "",
    onClusterStateChanged: (HasNodes) => Unit = _ => (),
    onUndecidableClusterNodeLoss: OnUndecidableClusterNodeLoss = _ => IO.unit)
  : ResourceIO[ClusterWatchService] =
    resource2(
      clusterWatchId, apisResource, config.withFallback(defaultConfig), label = label,
      onClusterStateChanged, onUndecidableClusterNodeLoss)

  private def resource2(
    clusterWatchId: ClusterWatchId,
    apisResource: ResourceIO[Nel[HttpClusterNodeApi]],
    config: Config,
    label: String,
    onClusterStateChanged: (HasNodes) => Unit,
    onUndecidableClusterNodeLoss: OnUndecidableClusterNodeLoss)
  : ResourceIO[ClusterWatchService] =
    Resource.suspend(IO:
      for
        nodeApis <- apisResource
        service <-
          Service.resource:
            new ClusterWatchService(
              clusterWatchId,
              nodeApis,
              label = label,
              keepAlive =
                config.finiteDuration("js7.web.client.keep-alive").orThrow,
              retryDelays =
                config.nonEmptyFiniteDurations("js7.journal.cluster.watch.retry-delays").orThrow,
              onClusterStateChanged,
              onUndecidableClusterNodeLoss)
      yield
        service)
