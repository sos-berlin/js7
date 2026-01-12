package js7.cluster

import cats.effect.{IO, ResourceIO}
import js7.base.auth.Admission
import js7.base.catsutils.CatsEffectExtensions.{recoverFromProblemAndRetry, *}
import js7.base.eventbus.EventPublisher
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.system.startup.Halt.haltJava
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.ActivationConsentChecker.*
import js7.data.cluster.ClusterEvent.{ClusterNodeLostEvent, ClusterPassiveLost}
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeLossNotConfirmedProblem, ClusterPassiveLostWhileFailedOverTestingProblem, ClusterWatchInactiveNodeProblem, ClusterWatchNotAskingProblem}
import js7.data.cluster.{ClusterNodeApi, ClusterState}
import js7.data.event.ClusterableState
import js7.data.node.{NodeId, NodeNameToPassword}
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

private final class ActivationConsentChecker private(
  val activationInhibitor: ActivationInhibitor,
  clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
  clusterConf: ClusterConf,
  val testEventBus: EventPublisher[Any]):

  private val ownId = clusterConf.ownId

  def checkConsent[S <: ClusterableState[S]](
    event: ClusterNodeLostEvent,
    aggr: S,
    clusterWatchSynchronizer: ClusterState.HasNodes => IO[ClusterWatchSynchronizer])
    (using NodeNameToPassword[S])
  : IO[Checked[Consent]] =
    logger.traceIOWithResult("checkConsent", event.toShortString):
      // TODO inhibitActivationOfPeer solange andauern lassen, also wiederholen,
      //  bis das Event ausgegeben worden ist, vielleicht noch etwas darüber hinaus.
      IO:
        aggr.toPeerAndAdmission(ownId).flatMap: (peerId, admission, clusterState) =>
          // Pre-apply ClusterNodeLostEvent //
          clusterState.applyEvent(event).flatMap:
            _.checkedSubtype[ClusterState.IsNodeLost]
          .map((peerId, admission, _))
      .flatMapT: (peerId, admission, nodeLossClusterState) =>
        checkConsent2(peerId, admission, event, nodeLossClusterState, clusterWatchSynchronizer)
      .flatTapT: consent =>
        IO.right(testEventBus.publish(consent))

  /** Check consent of both ClusterWatch and peer in a two-phase-commit.
    *
    * First ask both ClusterWatch and peer concurrently to prepare for a ClusterNodeLostEvent event.
    * If both agree, then notify ClusterWatch.
    *
    * The peer must not be active.
    *
    * In the first phase, the ClusterWatch is sent a ClusterWatchAskNodeLoss command, while
    * the peer get a ClusterInhibitActivation command.
    * The latter is expected to fail, which is interpreted as a non-active peer.
    *
    * In the second (commit) phase, the ClusterWatch is sent a ClusterWatchCommitNodeLoss command,
    * which may fail.
    *
    * The transaction is time-limited.
    *
    * After the ClusterWatch has been successfully notified,
    * the caller must emit the Cluster<NodeLostEvent.
    */
  private def checkConsent2[S <: ClusterableState[S]](
    peerId: NodeId,
    admission: Admission,
    event: ClusterNodeLostEvent,
    nodeLossClusterState: ClusterState.IsNodeLost,
    clusterWatchSynchronizer: ClusterState.HasNodes => IO[ClusterWatchSynchronizer])
  : IO[Checked[Consent]] =
    activationInhibitor.tryToActivate:
      // Ask peer //
      inhibitActivationOfPeer(peerId, admission,
        duration = nodeLossClusterState.setting.timing.inhibitActivationDuration
      ).raceBoth:
        // Concurrently ask ClusterWatch //
        askClusterWatch(event, nodeLossClusterState, clusterWatchSynchronizer, commit = false)
      .map:
        case Left(pair) => pair
        case Right(pair) => pair.swap
      .flatMap:
        // One has answered, the other (a Fiber) is still on the way
        case (Right(Consent.Given), other) =>
          other.joinStd.flatMap:
            case Right(Consent.Given) =>
              // We could check for timeout here, but we let the ClusterWatch do it
              // Commit //
              askClusterWatch(event, nodeLossClusterState, clusterWatchSynchronizer, commit = true)
              // Now, if succeeded, the caller MUST emit the event //

            case bad @ (Left(_) | Right(Consent.Rejected)) =>
              IO.pure(bad)

        case (bad @ (Left(_) | Right(Consent.Rejected)), other) =>
          other.cancel.as(bad)
      .recoverFromProblemAndRetry(()): (problem, _, retry) =>
        if problem is ClusterWatchNotAskingProblem then
          // Time elapsed
          logger.debug(s"⟲  Delay 1s after ClusterWatchNotAskingProblem, then retry ...")
          retry(()).delayBy(1.s/*TODO*/)
        else
          IO.left(problem)

  private def inhibitActivationOfPeer(
    peerId: NodeId,
    admission: Admission,
    duration: FiniteDuration)
  : IO[Checked[Consent]] =
    ActivationInhibitor
      .tryInhibitActivationOfPeer(ownId, peerId, admission, clusterNodeApi, duration)

  private def askClusterWatch(
    event: ClusterNodeLostEvent,
    nodeLossClusterState: ClusterState.IsNodeLost,
    clusterWatchSynchronizer: ClusterState.HasNodes => IO[ClusterWatchSynchronizer],
    commit: Boolean)
  : IO[Checked[Consent]] =
    clusterWatchSynchronizer(nodeLossClusterState).flatMap:
      _.askNodeLostEvent(event, nodeLossClusterState, commit = commit)
    .flatMap:
      case Left(problem) =>
        if problem.is(ClusterNodeLossNotConfirmedProblem)
          || problem.is(ClusterWatchInactiveNodeProblem)
        then
          logger.warn(s"⛔ ClusterWatch did not agree to ${
            event.getClass.simpleScalaName} event: $problem")
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

  def resource(
    clusterNodeApi: (Admission, String) => ResourceIO[ClusterNodeApi],
    clusterConf: ClusterConf,
    testEventBus: EventPublisher[Any])
  : ResourceIO[ActivationConsentChecker] =
    for
      activationInhibitor <- ActivationInhibitor.resource(
        testFailInhibitActivationWhileTrying =
          clusterConf.testFailInhibitActivationWhileTrying)
    yield
      ActivationConsentChecker(activationInhibitor, clusterNodeApi, clusterConf, testEventBus)


  // Public for testing
  enum Consent:
    case Rejected
    case Given
