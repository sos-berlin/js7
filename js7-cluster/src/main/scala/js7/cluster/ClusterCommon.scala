package js7.cluster

import cats.effect.{IO, Outcome, Resource, ResourceIO}
import cats.syntax.all.*
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.eventbus.EventPublisher
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.problem.{Checked, Problem}
import js7.base.system.startup.Halt.haltJava
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{OneTimeToken, OneTimeTokenProvider, ScalaUtils, SetOnce}
import js7.cluster.ClusterCommon.*
import js7.cluster.ClusterConf.ClusterProductName
import js7.core.license.LicenseChecker
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterPassiveLost}
import js7.data.cluster.ClusterState.{Coupled, FailedOver, HasNodes, SwitchedOver}
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeLossNotConfirmedProblem, ClusterPassiveLostWhileFailedOverTestingProblem, ClusterWatchInactiveNodeProblem}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterState}
import js7.data.event.ClusterableState
import js7.data.node.{NodeId, NodeNameToPassword}
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag

private[cluster] final class ClusterCommon private(
  val clusterWatchCounterpart: ClusterWatchCounterpart,
  val clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
  clusterConf: ClusterConf,
  licenseChecker: LicenseChecker,
  val testEventBus: EventPublisher[Any],
  val activationInhibitor: ActivationInhibitor):

  import clusterConf.ownId

  private val _clusterWatchSynchronizer = SetOnce[ClusterWatchSynchronizer]
  val couplingTokenProvider = OneTimeTokenProvider.unsafe()

  def stop: IO[Unit] =
    IO.defer:
      _clusterWatchSynchronizer.toOption.fold(IO.unit)(_.stop)

  def requireValidLicense: IO[Checked[Unit]] =
    IO(licenseChecker.checkLicense(ClusterProductName))

  def clusterWatchSynchronizer(clusterState: ClusterState.HasNodes): IO[ClusterWatchSynchronizer] =
    IO:
      _clusterWatchSynchronizer
        .toOption
        .getOrElse(initialClusterWatchSynchronizer(clusterState))

  def initialClusterWatchSynchronizer(clusterState: ClusterState.HasNodes): ClusterWatchSynchronizer =
    _clusterWatchSynchronizer.toOption match
      case Some(o) =>
        // Only after ClusterFailedOver or ClusterSwitchedOver,
        // because PassiveClusterNode has already started the ClusterWatchSynchronizer
        assertThat(clusterState.isInstanceOf[FailedOver] || clusterState.isInstanceOf[SwitchedOver])
        o

      case None =>
        import clusterState.setting
        val result = new ClusterWatchSynchronizer(
          ownId,
          clusterWatchCounterpart,
          setting.timing)
        _clusterWatchSynchronizer := result
        result

  def tryEndlesslyToSendCommand(admission: Admission, command: ClusterCommand): IO[Unit] =
    val name = command.getClass.simpleScalaName
    tryEndlesslyToSendCommand(clusterNodeApi(admission, name), _ => command)

  def tryEndlesslyToSendCommand[C <: ClusterCommand](
    apiResource: ResourceIO[ClusterNodeApi],
    toCommand: OneTimeToken => C)
  : IO[Unit] =
    clusterConf.delayConf.runIO: delayer =>
      val name = toCommand(OneTimeToken("TOKEN")).toShortString
      val since = now
      apiResource
        .use: api =>
          api
            .retryIfSessionLost:
              api.loginUntilReachable(onlyIfNotLoggedIn = true) *>
                couplingTokenProvider.resource.use: token =>
                  api.executeClusterCommand(toCommand(token))
            .map((_: ClusterCommand.Response) => ())
            .onErrorRestartLoop(()): (throwable, _, retry) =>
              logger.log(delayer.logLevel,
                s"${delayer.symbol} $name command failed with ${throwable.toStringWithCauses}")
              if !throwable.isInstanceOf[java.net.SocketException]
                && throwable.getStackTrace.nonEmpty
              then logger.debug(throwable.toString, throwable)
              // TODO ClusterFailed event?
              // TODO Handle heartbeat timeout?
              delayer.sleep *> retry(())
            .guaranteeCase:
              case Outcome.Succeeded(_) => IO:
                logger.log(delayer.relievedLogLevel,
                  s"🟢 $name command succeeded after ${since.elapsed.pretty}")
              case Outcome.Canceled() => IO:
                logger.log(delayer.relievedLogLevel,
                  s"◼️ $name command canceled after ${since.elapsed.pretty}")
              case _ => IO.unit

  def inhibitActivationOfPeer[S <: ClusterableState[S]](aggregate: S)(using NodeNameToPassword[S])
  : IO[Checked[Option[ClusterState]]] =
    locally:
      for
        clusterState <- aggregate.clusterState.checkedSubtype[Coupled]
        maybeUserAndPassword <- aggregate.clusterNodeToUserAndPassword(clusterState.activeId)
      yield
        inhibitActivationOfPeer(clusterState, maybeUserAndPassword)
    .sequence

  def inhibitActivationOfPeer(clusterState: HasNodes, peersUserAndPassword: Option[UserAndPassword])
  : IO[Option[ClusterState]] =
    ActivationInhibitor.inhibitActivationOfPassiveNode(
      clusterState.setting, peersUserAndPassword, clusterNodeApi)

  def ifClusterWatchAllowsActivation[S <: ClusterableState[S]](
    ownId: NodeId,
    event: ClusterNodeLostEvent,
    aggregate: S)
    (body: IO[Checked[Boolean]])
    (using NodeNameToPassword[S])
  : IO[Checked[Boolean]] =
    logger.traceIOWithResult:
      // TODO inhibitActivationOfPeer solange andauern lassen, also wiederholen,
      //  bis das Event ausgegeben worden ist, vielleicht noch etwas darüber hinaus.
      IO(aggregate.clusterState.checkedSubtype[HasNodes]).flatMapT: clusterState =>
        val peerId = clusterState.setting.other(ownId)
        IO(aggregate.clusterNodeToUserAndPassword(ownId))
          .flatMapT: peersUserAndPassword =>
            val admission = Admission(clusterState.setting.idToUri(peerId), peersUserAndPassword)
            ActivationInhibitor
              .tryInhibitActivationOfPeer(
                ownId, peerId, admission, clusterNodeApi, clusterState.setting.timing)
              .flatMap:
                case Left(problem) =>
                  logger.debug(s"⛔️ $problem")
                  IO.right(false)
                case Right(()) =>
                  activationInhibitor.tryToActivate:
                    activate(clusterState, event):
                      body

  private def activate(
    clusterState: ClusterState.HasNodes,
    event: ClusterNodeLostEvent)
    (body: IO[Checked[Boolean]])
  : IO[Checked[Boolean]] =
    logger.traceIOWithResult:
      IO.pure(clusterState.applyEvent(event)).flatMapT:
        case updatedClusterState: HasNodes =>
          clusterWatchSynchronizer(clusterState)
            .flatMap:
              _.applyEvent(event, updatedClusterState,
                // Changed ClusterWatch must only confirm when taught and sure !!!
                clusterWatchIdChangeAllowed = event.isInstanceOf[ClusterFailedOver])
            .flatMap:
              case Left(problem) =>
                if problem.is(ClusterNodeLossNotConfirmedProblem)
                  || problem.is(ClusterWatchInactiveNodeProblem) then
                  logger.warn:
                    s"⛔ ClusterWatch did not agree to '${event.getClass.simpleScalaName}' event: $problem"
                  testEventBus.publish(ClusterWatchDisagreedToActivation)
                  if event.isInstanceOf[ClusterPassiveLost] then
                    val msg = "🟥 While this node has lost the passive node" +
                      " and is waiting for ClusterWatch's agreement, " +
                      "the passive node failed over"
                    if clusterConf.testDontHaltWhenPassiveLostRejected then
                      IO.left(ClusterPassiveLostWhileFailedOverTestingProblem) // For test only
                    else
                      haltJava(msg, restart = true, warnOnly = true)
                  else
                    IO.right(false)  // Ignore heartbeat loss
                else
                  IO.left(problem)

              case Right(None) =>
                logger.debug:
                  s"No ClusterWatch confirmation required for '${event.getClass.simpleScalaName}' event"
                body

              case Right(maybeConfirm) =>
                maybeConfirm match
                  case None =>
                    logger.info(
                      s"ClusterWatch agreed to '${event.getClass.simpleScalaName}' event")
                  case Some(confirm) =>
                    logger.info(
                      s"${confirm.confirmer} agreed to '${event.getClass.simpleScalaName}' event")
                testEventBus.publish(ClusterWatchAgreedToActivation)
                body
        case ClusterState.Empty => IO.left(Problem.pure(
          "ClusterState:Empty in ifClusterWatchAllowsActivation ??"))


private[js7] object ClusterCommon:
  private val logger = Logger[this.type]

  def resource(
    clusterWatchCounterpart: ClusterWatchCounterpart,
    clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
    clusterConf: ClusterConf,
    licenseChecker: LicenseChecker,
    testEventPublisher: EventPublisher[Any])
  : ResourceIO[ClusterCommon] =
    for
      activationInhibitor <- ActivationInhibitor.resource(
        testFailInhibitActivationWhileTrying =
          clusterConf.testFailInhibitActivationWhileTrying)
      common <- Resource.make(
        acquire = IO:
          new ClusterCommon(
            clusterWatchCounterpart,
            clusterNodeApi, clusterConf, licenseChecker, testEventPublisher,
            activationInhibitor))(
        release = _.stop)
    yield
      common

  def clusterEventAndStateToString(event: ClusterEvent, state: ClusterState): String =
    s"ClusterEvent: $event --> $state"

  case object ClusterWatchAgreedToActivation
  case object ClusterWatchDisagreedToActivation
