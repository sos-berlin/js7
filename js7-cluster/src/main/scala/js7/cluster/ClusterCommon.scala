package js7.cluster

import cats.effect.{IO, Outcome, Resource, ResourceIO}
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
import js7.base.utils.{Delayer, OneTimeToken, OneTimeTokenProvider, SetOnce}
import js7.cluster.ClusterCommon.*
import js7.cluster.ClusterConf.ClusterProductName
import js7.core.license.LicenseChecker
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterPassiveLost}
import js7.data.cluster.ClusterState.{FailedOver, HasNodes, SwitchedOver}
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeLossNotConfirmedProblem, ClusterPassiveLostWhileFailedOverProblem, ClusterWatchInactiveNodeProblem}
import js7.data.cluster.{ClusterCommand, ClusterEvent, ClusterNodeApi, ClusterState}
import org.apache.pekko.util.Timeout
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag

private[cluster] final class ClusterCommon private(
  val clusterWatchCounterpart: ClusterWatchCounterpart,
  val clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
  clusterConf: ClusterConf,
  licenseChecker: LicenseChecker,
  val testEventBus: EventPublisher[Any])(
  implicit val journalActorAskTimeout: Timeout):

  import clusterConf.ownId

  val activationInhibitor = new ActivationInhibitor
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
    Delayer.start[IO](clusterConf.delayConf).flatMap: delayer =>
      val name = toCommand(OneTimeToken("TOKEN")).toShortString
      val since = now
      apiResource
        .use: api =>
          api
            .retryIfSessionLost():
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
                  s"⚫️ $name command canceled after ${since.elapsed.pretty}")
              case _ => IO.unit

  def inhibitActivationOfPeer(clusterState: HasNodes, peersUserAndPassword: Option[UserAndPassword])
  : IO[Option[FailedOver]] =
    ActivationInhibitor.inhibitActivationOfPassiveNode(
      clusterState.setting, peersUserAndPassword, clusterNodeApi)

  def ifClusterWatchAllowsActivation(
    clusterState: ClusterState.HasNodes,
    event: ClusterNodeLostEvent)
    (body: IO[Checked[Boolean]])
  : IO[Checked[Boolean]] =
    logger.traceIOWithResult(
      activationInhibitor.tryToActivate(
        ifInhibited = IO.right(false),
        activate = activate(clusterState, event)(body)))

  private def activate(
    clusterState: ClusterState.HasNodes,
    event: ClusterNodeLostEvent)
    (body: IO[Checked[Boolean]])
  : IO[Checked[Boolean]] =
    logger.traceIOWithResult:
      IO.pure(clusterState.applyEvent(event))
        .flatMapT:
          case updatedClusterState: HasNodes =>
            clusterWatchSynchronizer(clusterState)
              .flatMap(_.applyEvent(event, updatedClusterState,
                // Changed ClusterWatch must only confirm when taught and sure !!!
                clusterWatchIdChangeAllowed = event.isInstanceOf[ClusterFailedOver]))
              .flatMap:
                case Left(problem) =>
                  if problem.is(ClusterNodeLossNotConfirmedProblem)
                    || problem.is(ClusterWatchInactiveNodeProblem) then
                    logger.warn(
                      s"⛔ ClusterWatch did not agree to '${event.getClass.simpleScalaName}' event: $problem")
                    testEventBus.publish(ClusterWatchDisagreedToActivation)
                    if event.isInstanceOf[ClusterPassiveLost] then
                      val msg = "🟥 While this node has lost the passive node" +
                        " and is waiting for ClusterWatch's agreement, " +
                        "the passive node failed over"
                      if clusterConf.testDontHaltWhenPassiveLostRejected then
                        IO.left(ClusterPassiveLostWhileFailedOverProblem) // For test only
                      else
                        haltJava(msg, restart = true, warnOnly = true)
                    else
                      IO.right(false)  // Ignore heartbeat loss
                  else
                    IO.left(problem)

                case Right(None) =>
                  logger.debug(
                    s"No ClusterWatch confirmation required for '${event.getClass.simpleScalaName}' event")
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
    testEventPublisher: EventPublisher[Any])(
    implicit pekkoTimeout: Timeout)
  : ResourceIO[ClusterCommon] =
      Resource.make(
        IO(new ClusterCommon(
          clusterWatchCounterpart,
          clusterNodeApi, clusterConf, licenseChecker, testEventPublisher)))(
        release = _.stop)

  def clusterEventAndStateToString(event: ClusterEvent, state: ClusterState): String =
    s"ClusterEvent: $event --> $state"

  case object ClusterWatchAgreedToActivation
  case object ClusterWatchDisagreedToActivation
