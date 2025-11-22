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
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeLossNotConfirmedProblem, ClusterPassiveLostWhileFailedOverTestingProblem, ClusterWatchInactiveNodeProblem}
import js7.data.cluster.{ClusterNodeApi, ClusterState}
import js7.data.event.ClusterableState
import js7.data.node.{NodeId, NodeNameToPassword}
import scala.concurrent.duration.FiniteDuration
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
        aggr.toPeerAndAdmission(ownId).flatMap: (peerId, admission, clusterState) =>
          // Apply ClusterNodeLostEvent //
          clusterState.applyEvent(event).flatMap:
            _.checkedSubtype[ClusterState.IsNodeLost]
          .map((peerId, admission, _))
      .flatMapT: (peerId, admission, nodeLossClusterState) =>
        checkConsent2(peerId, admission, event, nodeLossClusterState, clusterWatchSynchronizer)
      .flatTapT: consent =>
        IO.right(testEventBus.publish(consent))

  private def checkConsent2[S <: ClusterableState[S]](
    peerId: NodeId,
    admission: Admission,
    event: ClusterNodeLostEvent,
    nodeLossClusterState: ClusterState.IsNodeLost,
    clusterWatchSynchronizer: ClusterState.HasNodes => IO[ClusterWatchSynchronizer])
  : IO[Checked[Consent]] =
    activationInhibitor.tryToActivate:
      inhibitActivationOfPeer(peerId, admission,
        duration = nodeLossClusterState.setting.timing.inhibitActivationDuration
      ).flatMapT:
        case Consent.Rejected => IO.right(Consent.Rejected)
        case Consent.Given =>
          askClusterWatch(event, nodeLossClusterState, clusterWatchSynchronizer)

  private def inhibitActivationOfPeer(
    peerId: NodeId,
    admission: Admission,
    duration: FiniteDuration)
  : IO[Checked[Consent]] =
    ActivationInhibitor
      .tryInhibitActivationOfPeer(
        ownId, peerId, admission, clusterNodeApi,
        duration = duration)
      .map:
        case Left(problem) =>
          logger.info(s"⛔️ inhibitActivationOfPeer: $problem")
          Right(Consent.Rejected)

        case Right(()) =>
          Right(Consent.Given)

  private def askClusterWatch(
    event: ClusterNodeLostEvent,
    nodeLossClusterState: ClusterState.IsNodeLost,
    clusterWatchSynchronizer: ClusterState.HasNodes => IO[ClusterWatchSynchronizer])
  : IO[Checked[Consent]] =
    clusterWatchSynchronizer(nodeLossClusterState).flatMap:
      _.applyEvent(event, nodeLossClusterState,
        // Changed ClusterWatch must only confirm when taught and sure !!!
        clusterWatchIdChangeAllowed = event.isInstanceOf[ClusterFailedOver])
    .flatMap:
      case Left(problem) =>
        if problem.is(ClusterNodeLossNotConfirmedProblem)
          || problem.is(ClusterWatchInactiveNodeProblem)
        then
          logger.warn(s"⛔ ClusterWatch did not agree to '${
            event.getClass.simpleScalaName}' event: $problem")
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

      case Right(maybeConfirmation) =>
        maybeConfirmation match
          case None =>
            logger.info(s"ClusterWatch agreed to '${event.getClass.simpleScalaName}' event")
          case Some(confirmation) =>
            logger.info:
              s"${confirmation.confirmer} agreed to '${event.getClass.simpleScalaName}' event"
        IO.right(Consent.Given)


object ActivationConsentChecker:
  private val logger = Logger[this.type]

  // Public for testing
  enum Consent:
    case Rejected
    case Given
