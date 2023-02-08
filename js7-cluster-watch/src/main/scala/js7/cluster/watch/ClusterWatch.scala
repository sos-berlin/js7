package js7.cluster.watch

import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.{AsyncVariable, MonixDeadline}
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.watch.ClusterWatch.*
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeLossNotConfirmedProblem, ClusterWatchEventMismatchProblem, ClusterWatchInactiveNodeProblem, NoClusterNodeLostProblem, UntaughtClusterWatchProblem}
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, HasNodes, PassiveLost}
import js7.data.cluster.{ClusterWatchCheckEvent, ClusterWatchRequest}
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.util.chaining.scalaUtilChainingOps

final class ClusterWatch(
  now: () => MonixDeadline,
  requireManualNodeLossConfirmation: Boolean = false)
{
  private val stateVar = AsyncVariable[Option[State]](None)

  // TODO `processRequest` wird synchron genutzt und braucht nicht mehr asynchron zu sein
  def processRequest(request: ClusterWatchRequest): Task[Checked[Completed]] =
    Task.pure(request.checked)
      .flatMapT(_ => processRequest2(request))

  private def processRequest2(request: ClusterWatchRequest): Task[Checked[Completed]] = {
    import request.{from, clusterState as reportedClusterState}
    val maybeEvent = request match {
      case o: ClusterWatchCheckEvent => Some(o.event)
      case _ => None
    }

    lazy val opString = s"${maybeEvent getOrElse "heartbeat"} --> $reportedClusterState"

    update(from, opString) {
      case None =>
        // Not yet initialized then no ClusterFailedOver
        if (maybeEvent.exists(_.isInstanceOf[ClusterFailedOver]))
          Left(UntaughtClusterWatchProblem)
        else
          teach(from, reportedClusterState)

      case Some(state) =>
        def clusterWatchInactiveNodeProblem =
          ClusterWatchInactiveNodeProblem(from,
            state.clusterState, state.lastHeartbeat.elapsed, opString)

        if (state.clusterState == reportedClusterState) {
          if (maybeEvent.nonEmpty) {
            logger.debug(
              s"$from: Ignore probably duplicate event for already reached clusterState=${
                state.clusterState}")
          }
          Right(reportedClusterState)
        } else
          maybeEvent
            .match_ {
              case Some(_: ClusterSwitchedOver) =>
                // ClusterSwitchedOver is applied by each node.
                // ClusterSwitchedOver is considered reliable.
                Checked.unit

              case Some(ClusterFailedOver(failedActiveId, _, _)) =>
                state.clusterState match {
                  case PassiveLost(setting) if setting.activeId == failedActiveId =>
                    Left(ClusterFailOverWhilePassiveLostProblem)

                  case clusterState =>
                    (from == clusterState.passiveId && !state.isLastHeartbeatStillValid) !!
                      clusterWatchInactiveNodeProblem
                }

              case _ =>
                (from == state.clusterState.activeId) !! clusterWatchInactiveNodeProblem
            }
            .flatMap(_ =>
              state.clusterState.applyEvents(maybeEvent.map(NoKey <-: _))
                .match_ {
                  case Left(problem) =>
                    logger.warn(s"$from: $problem")
                    Left(ClusterWatchEventMismatchProblem(
                      maybeEvent, state.clusterState, reportedClusterState = reportedClusterState))

                  case Right(clusterState) =>
                    for (event <- maybeEvent) logger.info(s"$from: $event")
                    maybeEvent match {
                      case Some(event: ClusterNodeLostEvent)
                        if requireManualNodeLossConfirmation && !state.isNodeLossConfirmed =>
                        Left(ClusterNodeLossNotConfirmedProblem(event))
                      case _ =>
                        if (clusterState == reportedClusterState) {
                          logger.info(s"$from changes ClusterState to $reportedClusterState")
                          Right(reportedClusterState)
                        } else {
                          // The node may have died just between sending the event to
                          // ClusterWatch and persisting it. Then we have a different state.
                          val previouslyActive = state.clusterState.activeId.string
                          // TODO Warning may occur due to different ClusterWatchId. But why?
                          logger.warn(s"$from forced ClusterState to $reportedClusterState " +
                            s"maybe because heartbeat of up to now active $previouslyActive " +
                            s"is too long ago (${state.lastHeartbeat.elapsed.pretty})")
                          Right(reportedClusterState)
                        }
                      }
                })
            .tap {
              case Left(problem) => logger.warn(s"$from: $problem")
              case Right(_) =>
            }
    }.rightAs(Completed)
  }

  private def teach(from: NodeId, clusterState: HasNodes) = {
    logger.info(s"$from teaches clusterState=$clusterState")
    Right(clusterState)
  }

  private def update(from: NodeId, operationString: => String)
    (body: Option[State] => Checked[HasNodes])
  : Task[Checked[HasNodes]] =
    stateVar
      .updateCheckedWithResult[Checked[HasNodes]](current => Task {
        logger.trace(s"$from: $operationString${
          current.fold("")(o => ", after " + o.lastHeartbeat.elapsed.pretty)}")

        body(current) match {
          case Left(problem: ClusterNodeLossNotConfirmedProblem) =>
            val updatedState = current.map(state => state.copy(
              lastHeartbeat = // Update lastHeartbeat only when `from` is active
                current.filter(_.clusterState.activeId != from).fold(now())(_.lastHeartbeat),
              lossRejected = Some(LossRejected(problem.event))))
            Right(updatedState -> /*result*/Left(problem))

          case Left(problem) =>
            Left(problem)

          case Right(updatedClusterState) =>
            val updatedState = Some(State(
              updatedClusterState,
              lastHeartbeat = now(),
              lossRejected = // Keep lossRejected iff clusterState is unchanged
                current.filter(_.clusterState == updatedClusterState).flatMap(_.lossRejected)))
            Right(updatedState -> /*result*/Right(updatedClusterState))
        }
      })
      .map(_.flatten)

  def confirmNodeLoss(lostNodeId: NodeId): Task[Checked[Unit]] =
    stateVar
      .updateChecked(current => Task(current
        .toRight(NoClusterNodeLostProblem)
        .flatMap(_.confirmNodeLoss(lostNodeId))
        .map(Some(_))))
      .rightAs(())

  @TestOnly
  private[cluster] def isActive(id: NodeId): Checked[Boolean] =
    unsafeClusterState().map(_.activeId == id)

  def unsafeClusterState(): Checked[HasNodes] =
    stateVar.get
      .toChecked(UntaughtClusterWatchProblem)
      .map(_.clusterState)

  override def toString =
    "ClusterWatch(" +
      stateVar.get.fold("untaught")(state =>
        state.clusterState.toShortString + ", " +
          state.lastHeartbeat.elapsed.pretty + " ago") +
      ")"
}

object ClusterWatch
{
  private val logger = Logger(getClass)

  private[ClusterWatch] final case class State(
    clusterState: HasNodes,
    lastHeartbeat: MonixDeadline,
    lossRejected: Option[LossRejected])
  {
    def isLastHeartbeatStillValid =
      (lastHeartbeat + clusterState.timing.clusterWatchHeartbeatValidDuration).hasTimeLeft

    def confirmNodeLoss(lostNodeId: NodeId): Checked[State] =
      matchRejectedNodeLostEvent(lostNodeId)
        .toRight(NoClusterNodeLostProblem)
        .map(rejected => copy(
          lossRejected = Some(rejected.copy(
            nodeLossConfirmed = true))))

    private def matchRejectedNodeLostEvent(lostNodeId: NodeId): Option[LossRejected] =
      (clusterState, lossRejected) match {
        case (Coupled(setting), Some(rejected @ LossRejected(ClusterPassiveLost(passiveId), _)))
          if lostNodeId == setting.passiveId && lostNodeId == passiveId =>
          Some(rejected)

        case (Coupled(setting), Some(rejected @ LossRejected(failedOver: ClusterFailedOver, _)))
          if lostNodeId == setting.activeId && lostNodeId == failedOver.lostNodeId =>
          Some(rejected)

        case _ => None
      }

    def isNodeLossConfirmed =
      lossRejected match {
        case Some(LossRejected(_, nodeLossConfirmed)) => nodeLossConfirmed
        case _ => false
      }
  }

  private[ClusterWatch] final case class LossRejected(
    event: ClusterNodeLostEvent,
    nodeLossConfirmed: Boolean = false)
}
