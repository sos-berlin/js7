package js7.cluster.watch

import cats.data.NonEmptyList
import cats.effect.{IO, ResourceIO}
import com.typesafe.config.Config
import fs2.Stream
import js7.base.catsutils.CatsEffectExtensions.{left, right}
import js7.base.config.Js7Config.defaultConfig
import js7.base.configutils.Configs.RichConfig
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.problem.Checked.RichCheckedF
import js7.base.service.{MainService, Service}
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.CatsUtils.syntax.mkString
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichEitherF, RichThrowable}
import js7.base.utils.{Atomic, DelayConf, Delayer, ProgramTermination}
import js7.base.web.HttpClient
import js7.base.web.HttpClient.HttpException
import js7.cluster.watch.ClusterWatch.{Confirmed, OnUndecidableClusterNodeLoss}
import js7.cluster.watch.ClusterWatchService.*
import js7.cluster.watch.api.HttpClusterNodeApi
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


final class ClusterWatchService private(
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
    checkActiveIsLost = checkActiveIsLost,
    onUndecidableClusterNodeLoss = onUndecidableClusterNodeLoss)
  val clusterWatchRunId: ClusterWatchRunId = ClusterWatchRunId.random()
  private val delayConf = DelayConf(retryDelays, resetWhen = retryDelays.last)

  protected def start =
    startServiceAndLog(logger, nodeApis.mkString(", ")):
      run

  val untilTerminated: IO[ProgramTermination] =
    untilStopped.as(ProgramTermination())

  private def run: IO[Unit] =
    Stream.iterable(nodeApis.toList).map: nodeApi =>
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

  private def checkActiveIsLost(clusterState: ClusterState.HasNodes): IO[Checked[Unit]] =
    IO.right(())
    // No safe way to differentiate between a living node with bad reachability or //
    // bad behaviour and a dead node. //
    //
    // The active node is the one that is currently being watched.
    // If it is not reachable, it is not in the cluster state.
    // If it is reachable, it is in the cluster state.
    //
    // If the active node is not in the cluster state, it is not reachable.
    // If the active node is in the cluster state, it is reachable.
    //
    //import clusterState.{activeId, activeUri}
    //IO:
    //  nodeApis.find(_.baseUri == activeUri).toRight:
    //    Problem.pure(s"🔥 Active node $activeUri is not in list of nodeApis") // Must not happen
    //.flatMapT: nodeApi =>
    //  Logger.info(s"Check if $activeId $activeUri is still alive ...")
    //  nodeApi.clusterNodeState
    //    .as(Left(ClusterWatchActiveStillAliveProblem))
    //    .handleErrorWith:
    //      case e: Exception
    //        if e.getMessage.contains("java.net.ConnectException: Connection refused") =>
    //        logger.info(s"✔︎ $activeId $activeUri does not connect and seems to be lost ✔︎")
    //        IO.right(())
    //      case t =>
    //        logger.warn(s"checkActiveIsLost: $ClusterWatchActiveStillAliveProblem $activeUri: ${
    //          t.toStringWithCauses}")
    //        IO.left(ClusterWatchActiveStillAliveProblem)

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
              nodeApi.clusterWatchRequestStream(clusterWatchId,
                keepAlive = Some(keepAlive),
                dontLog = !PekkoHttpClient.logHeartbeat)
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
      nodeApi.executeClusterWatchingCommand:
        ClusterWatchConfirm(
          request.requestId, clusterWatchId, clusterWatchRunId,
          manualConfirmer = confirmed.toOption.flatMap(_.manualConfirmer),
          problem = confirmed.left.toOption)
      .rightAs(())
      .handleProblemWith:
        case problem @ ClusterWatchRequestDoesNotMatchProblem =>
          // Already confirmed by this or another ClusterWatch
          IO(logger.info(s"❓$nodeApi $problem"))
        case problem =>
          IO(logger.warn(s"❓$nodeApi $problem"))
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
      service <-
        service(
          conf.clusterWatchId,
          apisResource = clusterNodeAdmissions.traverse: admission =>
            HttpClusterNodeApi.resource(admission, httpsConfig, uriPrefix = "controller"),
          config)
    yield
      service

  def service(
    clusterWatchId: ClusterWatchId,
    apisResource: ResourceIO[Nel[HttpClusterNodeApi]],
    config: Config,
    label: String = "",
    onClusterStateChanged: HasNodes => Unit = _ => (),
    onUndecidableClusterNodeLoss: OnUndecidableClusterNodeLoss = _ => IO.unit)
  : ResourceIO[ClusterWatchService] =
    for
      nodeApis <- apisResource
      service <-
        val config_ = config.withFallback(defaultConfig)
        Service.resource:
          new ClusterWatchService(
            clusterWatchId,
            nodeApis,
            label = label,
            keepAlive = config_.finiteDuration("js7.web.client.keep-alive").orThrow,
            retryDelays =
              config_.nonEmptyFiniteDurations("js7.journal.cluster.watch.retry-delays").orThrow,
            onClusterStateChanged,
            onUndecidableClusterNodeLoss)
    yield
      service
