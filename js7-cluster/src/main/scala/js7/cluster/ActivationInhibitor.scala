package js7.cluster

import cats.effect.std.Supervisor
import cats.effect.{IO, ResourceIO}
import js7.base.auth.{Admission, UserAndPassword}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.monixutils.AsyncVariable
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.DelayConf
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.cluster.ActivationConsentChecker.Consent
import js7.cluster.ActivationInhibitor.*
import js7.common.http.PekkoHttpClient
import js7.data.Problems.ClusterActivationInhibitedByPeerProblem
import js7.data.cluster.ClusterCommand.ClusterInhibitActivation
import js7.data.cluster.{ClusterNodeApi, ClusterSetting, ClusterState}
import js7.data.node.NodeId
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NoStackTrace

/** Inhibits activation of cluster node for the specified duration. */
private[cluster] final class ActivationInhibitor private(
  supervisor: Supervisor[IO],
  testFailInhibitActivationWhileTrying: Option[String]):

  private val _state = AsyncVariable[State](Initial)

  def startActive: IO[Unit] =
    startAs(Active)

  def startPassive: IO[Unit] =
    startAs(Passive)

  private def startAs(state: State): IO[Unit] =
    IO.defer:
      logger.debug(s"startAs $state")
      _state.update:
        case Initial =>
          IO.pure(state)
        case s =>
          IO.raiseError:
            new IllegalStateException(s"ActivationInhibitor startAs($state): Already '$s''")
      .void

  def tryToActivate(activate: IO[Checked[Consent]]): IO[Checked[Consent]] =
    logger.debugIOWithResult:
      _state.updateCheckedWithResult:
        case Initial | Active | Passive =>
          activate.flatMap:
            case o @ (Left(_) | Right(Consent.Rejected)) => IO:
              logger.debug(s"⛔ tryToActivate: Passive — because activation function returned $o")
              o.map(Passive -> _)
            case Right(Consent.Given) =>
              IO:
                logger.debug("✔︎ tryToActivate: Active — because activation function succeeded")
                Right(Active -> Consent.Given)

        case inhibited: Inhibited =>
          IO:
            logger.info("⛔ Activation of this cluster node has been inhibited by the peer ⛔")
            Right(inhibited -> Consent.Rejected)

  /** Tries to inhibit activation for `duration`.
    * @return true if activation is or has been inhibited, false if already active
    */
  def inhibitActivation(duration: FiniteDuration): IO[Boolean] =
    logger.debugIOWithResult:
      _state.updateWithResult:
        case Active =>
          logger.debug(s"This node is already active, ❗️ THE PEER MUST NOT ACTIVATE ❗️")
          IO.pure(Active -> false)

        case state @ (Initial | Passive | _: Inhibited) =>
          setInhibitionTimer(duration).as:
            val depth = state match
              case Inhibited(n) => n + 1
              case _ => 1
            val updated = Inhibited(depth)
            logger.debug(s"Inhibit activation for ${duration.pretty}: $updated")
            updated -> true
      .timeoutTo(500.ms/*???*/, IO:
        // TODO Maybe replace the timeout with a TryingToActivate state?
        logger.info("⚠️  inhibitActivation: This cluster node seems to be trying to activate, " +
          "so we reject the request")
        testFailInhibitActivationWhileTrying.fold(false): key =>
          throw new InhibitActivationFailsForTestingException)

  private def setInhibitionTimer(duration: FiniteDuration): IO[Unit] =
    supervisor.supervise:
      IO.sleep(duration) *>
        _state.update:
          case Inhibited(1) =>
            IO:
              logger.debug(s"Inhibition timer expired, becoming Passive")
              Passive
          case Inhibited(n) =>
            IO:
              val updated = Inhibited(n - 1)
              logger.debug(s"Inhibition timer expired, becoming $updated")
              updated
          case state =>
            // May happen in rare case of race condition (?)
            IO:
              logger.error(s"inhibitActivation timeout after ${duration.pretty
                }: expected Inhibited but got '$state'")
              state
    .void

  @TestOnly
  private[cluster] def state: IO[Option[State]] =
    _state.lockedValue.map(Some(_))
      .timeoutTo(100.ms, IO.none) // Lock detection


private[cluster] object ActivationInhibitor:
  private val logger = Logger[this.type]

  def resource(testFailInhibitActivationWhileTrying: Option[String] = None)
  : ResourceIO[ActivationInhibitor] =
    Supervisor[IO].map:
      new ActivationInhibitor(_, testFailInhibitActivationWhileTrying)

  def inhibitActivationOfPassiveNode(
    setting: ClusterSetting,
    peersUserAndPassword: Option[UserAndPassword],
    clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi])
  : IO[Option[ClusterState]] =
    val admission = Admission(setting.passiveUri, peersUserAndPassword)
    logger.infoIO(s"inhibitActivationOfPassiveNode ${setting.passiveUri}"):
      DelayConf.default.runIO: delayer =>
        clusterNodeApi(admission, "inhibitActivationOfPassiveNode")
          .evalTap:
            _.loginUntilReachable()
          .use:
            _.executeClusterCommand:
              ClusterInhibitActivation(setting.timing.inhibitActivationDuration)
          .map(_.clusterState)
          .onErrorRestartLoop(()): (throwable, _, retry) =>
            // TODO Code mit loginUntilReachable usw. zusammenfassen.
            //  Stacktrace unterdrücken wenn isNotIgnorableStackTrace
            val msg = "While trying to reach the other cluster node due to restart: " +
              throwable.toStringWithCauses
            logger.warn(msg)
            for t <- throwable.ifStackTrace if PekkoHttpClient.hasRelevantStackTrace(t) do
              logger.debug(msg, t)
            delayer.sleep >> retry(())
      .map(Result(_))
    .map(_.peerClusterState)

  /** Asks the peer node to inhibit activation for `duration`.
    * @return Some(peerClusterState), if the peer responded as an active node,
    *         <br>
    *         None otherwise. The peer may inhibit activation.
    */
  def tryInhibitActivationOfPeer(
    ownId: NodeId,
    peerId: NodeId,
    admission: Admission,
    clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
    inhibitActivationDuration: FiniteDuration)
  : IO[Checked[Unit]] =
    logger.infoIOWithResult(s"tryInhibitActivationOfPeer $peerId ${admission.uri}"):
      clusterNodeApi(admission, "tryInhibitActivationOfPeer").use: api =>
        api.login() *>
          api.executeClusterCommand:
            ClusterInhibitActivation(inhibitActivationDuration)
          .map(_.clusterState)
          .map:
            case None => Checked.unit
            case Some(clusterState) =>
              Left(ClusterActivationInhibitedByPeerProblem(ownId, peerId, clusterState))
      .handleError: throwable =>
        // We can only safely check if the peer is active. !!!
        // All other response mean, we doesn't know for sure.
        // Then we must allow the call to continue it's (otherwise checked!) activation.
        // TODO On any error, we assume the peer is not alive or not active.
        //  We should not be sure about this.
        logger.info(s"Peer $peerId ${admission.uri} seems no longer to be active: ${
          admission.uri} failed: ${throwable.toStringWithCauses}")
        Checked.unit


  private[cluster] sealed trait State
  private[cluster] case object Initial extends State
  private[cluster] case object Active extends State
  private[cluster] case object Passive extends State
  private[cluster] case class Inhibited(depth: Int) extends State:
    assertThat(depth >= 1)

  private final class Result(val peerClusterState: Option[ClusterState]):
    override def toString =
      peerClusterState match
        case None => "✔︎ Activation allowed"
        case Some(clusterState) =>
          s"⛔️ Activation inhibited due to peer clusterState=${clusterState.toShortString}"

  private final class InhibitActivationFailsForTestingException private[ActivationInhibitor]
  extends
    RuntimeException(s"🟪 inhibitActivation fails due to testFailInhibitActivationWhileTrying"),
    NoStackTrace
