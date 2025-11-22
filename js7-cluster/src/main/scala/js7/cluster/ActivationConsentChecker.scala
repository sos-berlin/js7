package js7.cluster

import cats.effect.{IO, ResourceIO}
import js7.base.auth.Admission
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.eventbus.EventPublisher
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.system.startup.Halt.haltJava
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.ActivationConsentChecker.*
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterPassiveLost}
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeLossNotConfirmedProblem, ClusterPassiveLostWhileFailedOverTestingProblem, ClusterWatchInactiveNodeProblem}
import js7.data.cluster.{ClusterNodeApi, ClusterState}
import js7.data.event.ClusterableState
import js7.data.node.{NodeId, NodeNameToPassword}
import scala.reflect.ClassTag

private class ActivationConsentChecker(
  val activationInhibitor: ActivationInhibitor,
  clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
  clusterConf: ClusterConf,
  val testEventBus: EventPublisher[Any]):

  private val ownId = clusterConf.ownId

  def checkConsent[S <: ClusterableState[S]](
    event: ClusterNodeLostEvent,
    aggr: S,
    /*TODO*/clusterWatchSynchronizer: ClusterState.HasNodes => IO[ClusterWatchSynchronizer])
    (using NodeNameToPassword[S])
  : IO[Checked[Consent]] =
    logger.traceIOWithResult:
      // TODO inhibitActivationOfPeer solange andauern lassen, also wiederholen,
      //  bis das Event ausgegeben worden ist, vielleicht noch etwas darüber hinaus.
      IO.pure:
        aggr.clusterState.checkedSubtype[HasNodes].flatMap: clusterState =>
          val peerId = clusterState.setting.other(ownId)
          aggr.clusterNodeToUserAndPassword(ownId).map: peersUserAndPassword =>
            val admission = Admission(clusterState.setting.idToUri(peerId), peersUserAndPassword)
            (clusterState, peerId, admission)
        .flatMap: (clusterState, peerId, admission) =>
          clusterState.applyEvent(event).flatMap:
            _.checkedSubtype[ClusterState.IsNodeLost]
          .map:
            (_, peerId, admission)
      .flatMapT: (updatedClusterState, peerId, admission) =>
        checkConsent2(peerId, admission, event, updatedClusterState, clusterWatchSynchronizer)

  private def checkConsent2[S <: ClusterableState[S]](
    peerId: NodeId,
    admission: Admission,
    event: ClusterNodeLostEvent,
    updatedClusterState: ClusterState.IsNodeLost,
    clusterWatchSynchronizer: ClusterState.HasNodes => IO[ClusterWatchSynchronizer])
  : IO[Checked[Consent]] =
    activationInhibitor.tryToActivate:
      inhibitActivationOfPeer(peerId, admission, updatedClusterState).flatMapT:
        case Consent.Rejected =>
          IO.right(Consent.Rejected)

        case Consent.Given =>
          // TODO Stop providing acknowledgments ?
          askClusterWatch(event, updatedClusterState, clusterWatchSynchronizer)

  private def inhibitActivationOfPeer(
    peerId: NodeId,
    admission: Admission,
    updatedClusterState: ClusterState.IsNodeLost)
  : IO[Checked[Consent]] =
    ActivationInhibitor
      .tryInhibitActivationOfPeer(
        ownId, peerId, admission, clusterNodeApi,
        inhibitActivationDuration = updatedClusterState.setting.timing.inhibitActivationDuration)
      .map:
        case Left(problem) =>
          logger.info(s"⛔️ $problem")
          Right(Consent.Rejected)

        case Right(()) =>
          Right(Consent.Given)

  private def askClusterWatch(
    event: ClusterNodeLostEvent,
    updatedClusterState: ClusterState.IsNodeLost,
    clusterWatchSynchronizer: ClusterState.HasNodes => IO[ClusterWatchSynchronizer])
  : IO[Checked[Consent]] =
    clusterWatchSynchronizer(updatedClusterState).flatMap:
      _.applyEvent(event, updatedClusterState,
        // Changed ClusterWatch must only confirm when taught and sure !!!
        clusterWatchIdChangeAllowed = event.isInstanceOf[ClusterFailedOver])
    .flatMap:
      case Left(problem) =>
        if problem.is(ClusterNodeLossNotConfirmedProblem)
          || problem.is(ClusterWatchInactiveNodeProblem) then
          logger.warn(s"⛔ ClusterWatch did not agree to '${
            event.getClass.simpleScalaName}' event: $problem")
          testEventBus.publish(ClusterWatchDisagreedToActivation)
          if event.isSubtypeOf[ClusterPassiveLost] then
            val msg = "🟥 While this node has lost the passive node" +
              " and is waiting for ClusterWatch's agreement, " +
              "the passive node failed over"
            if clusterConf.testDontHaltWhenPassiveLostRejected then
              IO.left(ClusterPassiveLostWhileFailedOverTestingProblem) // For test only
            else
              haltJava(msg, restart = true, warnOnly = true)
          else
            IO.right(Consent.Rejected) // Ignore heartbeat loss
        else
          IO.left(problem)

      case Right(None) =>
        logger.debug(s"No ClusterWatch confirmation required for '${
          event.getClass.simpleScalaName}' event")
        IO.right(Consent.Given)

      case Right(maybeConfirm) =>
        maybeConfirm match
          case None =>
            logger.info(s"ClusterWatch agreed to '${event.getClass.simpleScalaName}' event")
          case Some(confirm) =>
            logger.info:
              s"${confirm.confirmer} agreed to '${event.getClass.simpleScalaName}' event"
        testEventBus.publish(ClusterWatchAgreedToActivation)
        IO.right(Consent.Given)


object ActivationConsentChecker:
  private val logger = Logger[this.type]

  private[cluster] enum Consent:
    case Rejected
    case Given

  case object ClusterWatchAgreedToActivation
  case object ClusterWatchDisagreedToActivation
