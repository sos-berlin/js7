package js7.core.cluster

import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixDeadline
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem, ProblemCode}
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.core.cluster.ClusterWatch.*
import js7.data.cluster.ClusterEvent.{ClusterFailedOver, ClusterSwitchedOver}
import js7.data.cluster.ClusterState.{Coupled, HasNodes, PassiveLost}
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.util.chaining.scalaUtilChainingOps

final class ClusterWatch(controllerId: ControllerId, scheduler: Scheduler)
extends ClusterWatchApi
{
  private val stateMVar = MVar[Task].of(None: Option[State]).memoize

  logger.trace(toString)

  @TestOnly
  private[cluster] def isActive(id: NodeId): Task[Checked[Boolean]] =
    get.map(_.map {
      case o: ClusterState.HasNodes => o.activeId == id
      case ClusterState.Empty => sys.error("ClusterState must not be Empty")
    })

  def get: Task[Checked[ClusterState]] =
    stateMVar.flatMap(_.read)
      .map(_
        .toChecked(Problem(
          s"ClusterWatch not yet started for Controller '$controllerId'"))
        .map(_.clusterState))

  def applyEvents(clusterWatchEvents: ClusterWatchEvents): Task[Checked[Completed]] = {
    import clusterWatchEvents.{events, from, clusterState as reportedClusterState}

    lazy val opString = s"event ${events.mkString(", ")} --> $reportedClusterState"

    update(from, opString) {
      case None =>
        // Not yet initialized then no ClusterFailedOver
        if (events.exists(_.event.isInstanceOf[ClusterFailedOver]))
          Left(UntaughtClusterWatchProblem)
        else
          teach(from, reportedClusterState)

      case Some(state) =>
        def clusterWatchInactiveNodeProblem =
          ClusterWatchInactiveNodeProblem(from,
            state.clusterState, state.lastHeartbeat.elapsed, opString)

        if (state.clusterState == reportedClusterState) {
          logger.info(
            s"Node '$from': Ignore probably duplicate events for already reached clusterState=${state.clusterState}")
          Right(reportedClusterState)
        } else
          events
            .match_ {
              // ClusterSwitchedOver must arrive as single events.
              case Seq(_: ClusterSwitchedOver) =>
                // ClusterSwitchedOver is applied by each node.
                // ClusterSwitchedOver is considered reliable.
                Checked.unit

              // ClusterFailedOver must arrive as single events.
              case Seq(ClusterFailedOver(failedActiveId, _, _)) =>
                state.clusterState match {
                  case ClusterState.Empty => Left(Problem("ClusterState.Empty ??"))
                  case PassiveLost(setting) if setting.activeId == failedActiveId =>
                    Left(ClusterFailOverWhilePassiveLostProblem)

                  case o: HasNodes =>
                    (from == o.passiveId && !state.isLastHeartbeatStillValid) !!
                      clusterWatchInactiveNodeProblem
                }

              case _ =>
                state.clusterState match {
                  case ClusterState.Empty => Checked.unit
                  case o: HasNodes =>
                    (from == o.activeId) !! clusterWatchInactiveNodeProblem
                }
            }
            .flatMap { _ =>
              state.clusterState.applyEvents(events.map(NoKey <-: _))
                .match_ {
                  case Left(problem) =>
                    logger.warn(s"Node '$from': $problem")
                    Left(ClusterWatchEventMismatchProblem(
                      events, state.clusterState, reportedClusterState = reportedClusterState))

                  case Right(clusterState) =>
                    for (event <- events) logger.info(s"Node '$from': $event")
                    if (clusterState == reportedClusterState) {
                      logger.info(s"Node '$from' changed ClusterState to $reportedClusterState")
                      Right(reportedClusterState)
                    } else {
                      // The node may have died just between sending the event to ClusterWatch and
                      // persisting it. Then we have a different state.
                      val previouslyActive = state.clusterState match {
                        case o: HasNodes => o.activeId.string
                        case ClusterState.Empty => "Empty"
                      }
                      logger.warn(s"Node '$from' forced ClusterState to $reportedClusterState " +
                        s"because heartbeat of up to now active node '$previouslyActive' is too long ago " +
                        s"(${state.lastHeartbeat.elapsed.pretty})")
                      Right(reportedClusterState)
                    }
                  }
            }
          .tap {
            case Left(problem) => logger.warn(s"Node '$from': $problem")
            case Right(_) =>
          }
    }.rightAs(Completed)
  }

  def heartbeat(from: NodeId, reportedClusterState: ClusterState): Task[Checked[Completed]] = {
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
                val msg = s"Node '$from': $problem"
                logger.warn(msg)
                Left(problem)
              }
          }
          .flatMap(_ =>
            current match {
              case Some(state @ State(clusterState: HasNodes, _)) =>
                if (reportedClusterState == clusterState)
                  Right(reportedClusterState)
                else if (!state.isLastHeartbeatStillValid) {
                  logger.warn(s"Node '$from': Heartbeat changed $clusterState")
                  Right(reportedClusterState)
                } else {
                  // May occur also when active node terminates after
                  // emitting a ClusterEvent and before applyEvents to ClusterWatch,
                  // and the active node is restarted within the clusterWatchReactionTimeout !!!
                  val problem = ClusterWatchHeartbeatMismatchProblem(clusterState,
                    reportedClusterState = reportedClusterState)
                  logger.warn(s"Node '$from': $problem")
                  Left(problem)
                }

              case _ =>
                Right(reportedClusterState)
          })
    ).map(_.toCompleted)
  }

  private def teach(from: NodeId, clusterState: ClusterState) = {
    logger.info(s"Node '$from' teaches clusterState=$clusterState")
    Right(clusterState)
  }

  private def update(from: NodeId, operationString: => String)
    (body: Option[State] => Checked[ClusterState])
  : Task[Checked[ClusterState]] =
    stateMVar.flatMap(mvar =>
      mvar.take.flatMap { current =>
        logger.trace(s"Node '$from': $operationString${
          current.fold("")(o => ", after " + o.lastHeartbeat.elapsed.pretty)}")
        body(current) match {
          case Left(problem) =>
            mvar.put(current)
              .map(_ => Left(problem))
          case Right(updated) =>
            mvar.put(Some(State(updated, now)))
              .map(_ => Right(updated))
        }
      })

  private def now = MonixDeadline.now(scheduler)

  override def toString = s"ClusterWatch($controllerId)"
}

object ClusterWatch
{
  private val logger = Logger(getClass)

  private[cluster] case class State(clusterState: ClusterState, lastHeartbeat: MonixDeadline)
  {
    def isLastHeartbeatStillValid =
      this match {
        case State(clusterState: HasNodes, lastHeartbeat) =>
          (lastHeartbeat + clusterState.timing.clusterWatchHeartbeatValidDuration).hasTimeLeft

        case State(ClusterState.Empty, _) => false // Should not happen
      }

    /** false iff `nodeId` cannot be the active node.  */
    def canBeTheActiveNode(nodeId: NodeId): Boolean =
      this match {
        case State(clusterState: HasNodes, _) if nodeId == clusterState.activeId =>
          true // Sure

        case state @ State(_: Coupled, _) =>
          !state.isLastHeartbeatStillValid // Not sure, because no heartbeat

        case _ =>
          false // Sure
      }
  }

  private lazy val isClusterWatchProblemCode = Set[ProblemCode](
    UntaughtClusterWatchProblem.code,
    ClusterWatchHeartbeatMismatchProblem.code,
    ClusterWatchEventMismatchProblem.code,
    ClusterWatchInactiveNodeProblem.code,
    InvalidClusterWatchHeartbeatProblem.code,
    ClusterFailOverWhilePassiveLostProblem.code)

  def isClusterWatchProblem(problem: Problem): Boolean =
    problem.maybeCode exists isClusterWatchProblemCode

  final case object UntaughtClusterWatchProblem extends Problem.ArgumentlessCoded

  final case class ClusterWatchHeartbeatMismatchProblem(
    currentClusterState: ClusterState,
    reportedClusterState: ClusterState)
  extends Problem.Coded
  {
    //"Controller's ClusterState $currentClusterState does not match registered $clusterState")
    def arguments = Map(
      "currentClusterState" -> currentClusterState.toString,
      "reportedClusterState" -> reportedClusterState.toString)
  }
  object ClusterWatchHeartbeatMismatchProblem extends Problem.Coded.Companion

  final case class ClusterWatchEventMismatchProblem(
    events: Seq[ClusterEvent],
    currentClusterState: ClusterState,
    reportedClusterState: ClusterState)
  extends Problem.Coded
  {
    //"Controller's ClusterState $currentClusterState does not match registered $clusterState")
    def arguments = Map(
      "events" -> events.mkString(", "),
      "currentClusterState" -> currentClusterState.toString,
      "reportedClusterState" -> reportedClusterState.toString)
  }
  object ClusterWatchEventMismatchProblem extends Problem.Coded.Companion

  final case class ClusterWatchInactiveNodeProblem(from: NodeId, clusterState: ClusterState,
    lastHeartbeatDuration: FiniteDuration, operation: String)
  extends Problem.Coded {
    def arguments = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString,
      "lastHeartbeat" -> lastHeartbeatDuration.pretty,
      "operation" -> operation)
  }
  object ClusterWatchInactiveNodeProblem extends Problem.Coded.Companion

  final case class InvalidClusterWatchHeartbeatProblem(from: NodeId, clusterState: ClusterState)
  extends Problem.Coded {
    def arguments = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString)
  }
  object InvalidClusterWatchHeartbeatProblem extends Problem.Coded.Companion

  final case object ClusterFailOverWhilePassiveLostProblem extends Problem.ArgumentlessCoded
}
