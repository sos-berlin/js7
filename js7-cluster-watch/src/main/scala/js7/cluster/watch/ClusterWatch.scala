package js7.cluster.watch

import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.{AsyncVariable, MonixDeadline}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.watch.ClusterWatch.*
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterFailOverWhilePassiveLostProblem, ClusterNodeLossNotAcknowledgedProblem, ClusterWatchEventMismatchProblem, ClusterWatchHeartbeatMismatchProblem, ClusterWatchInactiveNodeProblem, InvalidClusterWatchHeartbeatProblem, NoClusterNodeLostProblem, UntaughtClusterWatchProblem}
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterNodeLostEvent, ClusterPassiveLost, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, HasNodes, PassiveLost}
import js7.data.cluster.{ClusterState, ClusterWatchCheckEvent, ClusterWatchMessage}
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.util.chaining.scalaUtilChainingOps

final class ClusterWatch(
  now: () => MonixDeadline,
  requireLostAck: Boolean = false)
{
  private val stateVar = AsyncVariable[Option[State]](None)

  def logout() = Task.pure(Completed)

  @TestOnly
  private[cluster] def isActive(id: NodeId): Task[Checked[Boolean]] =
    clusterState.map(_.map(_.activeId == id))

  def clusterState: Task[Checked[HasNodes]] =
    stateVar.value
      .map(_
        .toChecked(Problem("ClusterWatch not yet started"))
        .map(_.clusterState))

  // TODO Das wird synchron genutzt und braucht nicht mehr asynchron zu sein

  def handleMessage(msg: ClusterWatchMessage): Task[Checked[Completed]] = {
    import msg.{from, clusterState as reportedClusterState}
    val maybeEvent = msg match {
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
              s"$from: Ignore probably duplicate events for already reached clusterState=${
                state.clusterState}")
          }
          Right(reportedClusterState)
        } else
          maybeEvent
            .match_ {
              // ClusterSwitchedOver must arrive as single events.
              case Some(_: ClusterSwitchedOver) =>
                // ClusterSwitchedOver is applied by each node.
                // ClusterSwitchedOver is considered reliable.
                Checked.unit

              // ClusterFailedOver must arrive as single events.
              case Some(ClusterFailedOver(failedActiveId, _, _)) =>
                state.clusterState match {
                  case PassiveLost(setting) if setting.activeId == failedActiveId =>
                    Left(ClusterFailOverWhilePassiveLostProblem)

                  case clusterState =>
                    (from == clusterState.passiveId && !state.isLastHeartbeatStillValid) !!
                      clusterWatchInactiveNodeProblem
                }

              case _ =>
                state.clusterState match {
                  case o: HasNodes =>
                    (from == o.activeId) !! clusterWatchInactiveNodeProblem
                }
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
                    if (clusterState == reportedClusterState) {
                      maybeEvent
                        .collect { case o: ClusterNodeLostEvent => o }
                        .match_ {
                          case Some(event) if requireLostAck && !state.isLostNodeAcknowledged =>
                            Left(ClusterNodeLossNotAcknowledgedProblem(event))
                          case _ =>
                            logger.info(s"$from changes ClusterState to $reportedClusterState")
                            Right(reportedClusterState)
                        }
                    } else {
                      // The node may have died just between sending the event to ClusterWatch and
                      // persisting it. Then we have a different state.
                      val previouslyActive = state.clusterState.activeId.string
                      logger.warn(s"$from forced ClusterState to $reportedClusterState " +
                        s"because heartbeat of up to now active $previouslyActive is " +
                        s"too long ago (${state.lastHeartbeat.elapsed.pretty})")
                      Right(reportedClusterState)
                    }
                  })
          .tap {
            case Left(problem) => logger.warn(s"$from: $problem")
            case Right(_) =>
          }
    }.rightAs(Completed)
  }

  def heartbeat(from: NodeId, reportedClusterState: HasNodes): Task[Checked[Completed]] = {
    lazy val opString = s"heartbeat $reportedClusterState"
    update(from, opString)(current =>
      if (!reportedClusterState.isNonEmptyActive(from))
        Left(InvalidClusterWatchHeartbeatProblem(from, reportedClusterState))
      else
        current
          .match_ {
            case None =>
              // Initial state, we know nothing. So we accept the first visitor.
              // The `body` checks the trustworthiness.
              teach(from, reportedClusterState)

            case Some(state) =>
              if (state.canBeTheActiveNode(from))
                Checked.unit
              else {
                val problem = ClusterWatchInactiveNodeProblem(from,
                  state.clusterState, state.lastHeartbeat.elapsed, opString)
                val msg = s"$from: $problem"
                logger.warn(msg)
                Left(problem)
              }
          }
          .flatMap(_ =>
            current match {
              case Some(state @ State(clusterState, _, _)) =>
                if (reportedClusterState == clusterState)
                  Right(reportedClusterState)
                else if (!state.isLastHeartbeatStillValid) {
                  logger.warn(s"$from: Heartbeat changed $clusterState")
                  Right(reportedClusterState)
                } else {
                  // May occur also when active node terminates after
                  // emitting a ClusterEvent and before applyEvents to ClusterWatch,
                  // and the active node is restarted within the clusterWatchReactionTimeout !!!
                  val problem = ClusterWatchHeartbeatMismatchProblem(clusterState,
                    reportedClusterState = reportedClusterState)
                  logger.warn(s"$from: $problem")
                  Left(problem)
                }

              case _ =>
                Right(reportedClusterState)
          })
    ).map(_.toCompleted)
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
          case Left(problem: ClusterNodeLossNotAcknowledgedProblem) =>
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

  def acknowledgeLostNode(lostNodeId: NodeId): Task[Checked[Unit]] =
    stateVar
      .updateChecked(current => Task(current
        .toRight(NoClusterNodeLostProblem)
        .flatMap(_.acknowledgeLostNode(lostNodeId))
        .map(Some(_))))
      .rightAs(())

  @TestOnly
  private[watch] def currentClusterState: Option[ClusterState] =
    stateVar.get.map(_.clusterState)

  override def toString = "ClusterWatch"
}

object ClusterWatch
{
  private val logger = Logger(getClass)

  private[cluster] case class State(
    clusterState: HasNodes,
    lastHeartbeat: MonixDeadline,
    lossRejected: Option[LossRejected])
  {
    def isLastHeartbeatStillValid =
      (lastHeartbeat + clusterState.timing.clusterWatchHeartbeatValidDuration).hasTimeLeft

    /** false iff `nodeId` cannot be the active node.  */
    def canBeTheActiveNode(nodeId: NodeId): Boolean =
      this match {
        case State(clusterState: HasNodes, _, _) if nodeId == clusterState.activeId =>
          true // Sure

        case state @ State(_: Coupled, _, _) =>
          !state.isLastHeartbeatStillValid // Not sure, because no heartbeat

        case _ =>
          false // Sure
      }

    def acknowledgeLostNode(lostNodeId: NodeId): Checked[State] =
      matchRejectedNodeLostEvent(lostNodeId)
        .toRight(NoClusterNodeLostProblem)
        .map(rejected => copy(
          lossRejected = Some(rejected.copy(
            lostNodeAcknowledged = true))))

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

    def isLostNodeAcknowledged =
      lossRejected match {
        case Some(LossRejected(_, lostNodeAcknowledged)) => lostNodeAcknowledged
        case _ => false
      }
  }

  private[watch] final case class LossRejected(
    event: ClusterNodeLostEvent,
    lostNodeAcknowledged: Boolean = false)
}
