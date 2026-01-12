package js7.cluster

import cats.effect.{IO, Outcome, ResourceIO}
import cats.syntax.all.*
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.eventbus.EventPublisher
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.problem.Checked
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{OneTimeToken, OneTimeTokenProvider, ScalaUtils, SetOnce}
import js7.cluster.ActivationConsentChecker.Consent
import js7.cluster.ClusterCommon.*
import js7.cluster.ClusterConf.ClusterProductName
import js7.core.license.LicenseChecker
import js7.data.cluster.ClusterEvent.ClusterNodeLostEvent
import js7.data.cluster.ClusterState.{Coupled, FailedOver, HasNodes, SwitchedOver}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterState}
import js7.data.event.ClusterableState
import js7.data.node.NodeNameToPassword
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag

private final class ClusterCommon private(
  val clusterWatchCounterpart: ClusterWatchCounterpart,
  val clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
  clusterConf: ClusterConf,
  licenseChecker: LicenseChecker,
  val activationConsentChecker: ActivationConsentChecker)

extends Service.TrivialReleasable:

  export activationConsentChecker.testEventBus
  import clusterConf.ownId

  private val _clusterWatchSynchronizer = SetOnce[ClusterWatchSynchronizer]
  val couplingTokenProvider = OneTimeTokenProvider.unsafe()

  protected def release: IO[Unit] =
    IO.defer:
      _clusterWatchSynchronizer.foldMap(_.stop)

  def requireValidLicense: IO[Checked[Unit]] =
    IO(licenseChecker.checkLicense(ClusterProductName))

  def clusterWatchSynchronizer(clusterState: ClusterState.HasNodes): IO[ClusterWatchSynchronizer] =
    IO:
      _clusterWatchSynchronizer.toOption.getOrElse:
        initialClusterWatchSynchronizer(clusterState)

  def initialClusterWatchSynchronizer(clusterState: ClusterState.HasNodes)
  : ClusterWatchSynchronizer =
    _clusterWatchSynchronizer.toOption match
      case Some(o) =>
        // Only after ClusterFailedOver or ClusterSwitchedOver,
        // because PassiveClusterNode has already started the ClusterWatchSynchronizer
        assertThat(clusterState.isInstanceOf[FailedOver] || clusterState.isInstanceOf[SwitchedOver])
        o

      case None =>
        import clusterState.setting
        val result = new ClusterWatchSynchronizer(ownId, clusterWatchCounterpart, setting.timing)
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
      apiResource.use: api =>
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

  /** Ask peer and ClusterWatch about consent that this node is or gets active. */
  def checkConsent[S <: ClusterableState[S]](
    event: ClusterNodeLostEvent,
    aggregate: S)
    (body: IO[Checked[Unit]])
    (using NodeNameToPassword[S])
  : IO[Checked[Consent]] =
    activationConsentChecker.checkConsent(event, aggregate, clusterWatchSynchronizer).flatTapT:
      case Consent.Rejected => IO.right(())
      case Consent.Given =>
        body


private object ClusterCommon:
  private val logger = Logger[this.type]

  def resource(
    clusterWatchCounterpart: ClusterWatchCounterpart,
    clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
    clusterConf: ClusterConf,
    licenseChecker: LicenseChecker,
    testEventBus: EventPublisher[Any])
  : ResourceIO[ClusterCommon] =
    for
      activationConsentChecker <-
        ActivationConsentChecker.resource(clusterNodeApi, clusterConf, testEventBus)
      common <- Service:
        ClusterCommon(
          clusterWatchCounterpart, clusterNodeApi, clusterConf, licenseChecker,
          activationConsentChecker)
    yield
      common

  def clusterEventAndStateToString(event: ClusterEvent, state: ClusterState): String =
    s"ClusterEvent: $event --> $state"
