package js7.core.cluster

import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixDeadline
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem, ProblemCode}
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichAny
import js7.core.cluster.ClusterWatch._
import js7.data.cluster.ClusterEvent.ClusterSwitchedOver
import js7.data.cluster.ClusterState.{Coupled, HasNodes}
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration

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
      .map(_.map(_.clusterState) toChecked Problem(s"ClusterWatch not yet started for Controller '$controllerId'"))

  def applyEvents(clusterWatchEvents: ClusterWatchEvents): Task[Checked[Completed]] = {
    import clusterWatchEvents.{events, from, clusterState => reportedClusterState}
    val fromMustBeActive = clusterWatchEvents.events match {
      case Seq(_: ClusterSwitchedOver) => false // Both nodes issue the (same) event
      case _ => true
    }
    update(from, fromMustBeActive = fromMustBeActive,
      s"event ${events.mkString(", ")} --> $reportedClusterState") {
      case None =>
        // Not yet initialized: we accept anything
        // FIXME But we cannot safely rely on the first visitor!
        Right(reportedClusterState)

      case Some(current) =>
        if (current.clusterState == reportedClusterState) {
          logger.info(s"Node '$from': Ignore probably duplicate events for already reached clusterState=${current.clusterState}")
        } else {
          current.clusterState.applyEvents(events.map(NoKey <-: _)) match {
            case Left(problem) =>
              logger.error(s"Node '$from': $problem")
              logger.error(s"Node '$from': " +
                ClusterWatchEventMismatchProblem(events, current.clusterState, reportedClusterState = reportedClusterState))

            case Right(clusterState) =>
              for (event <- events) logger.info(s"Node '$from': $event")
              if (current.isLastHeartbeatStillValid && clusterState != reportedClusterState)
                logger.error(s"Node '$from': " +
                  ClusterWatchEventMismatchProblem(events, clusterState, reportedClusterState = reportedClusterState))
          }
        }
        Right(reportedClusterState)
    }.map(_.toCompleted)
  }

  def heartbeat(from: NodeId, reportedClusterState: ClusterState): Task[Checked[Completed]] =
    update(from, fromMustBeActive = true, s"heartbeat $reportedClusterState")(current =>
      if (!reportedClusterState.isNonEmptyActive(from))
        Left(InvalidClusterWatchHeartbeatProblem(from, reportedClusterState))
      else
        current match {
          case Some(state @ State(clusterState: HasNodes, _))
            if state.isLastHeartbeatStillValid && reportedClusterState != clusterState =>
            // May occur also when active node terminates after
            // emitting a ClusterEvent and before applyEvents to ClusterWatch,
            // and the active node is restarted within the heartbeatValidDuration !!!
            val problem = ClusterWatchHeartbeatMismatchProblem(clusterState, reportedClusterState = reportedClusterState)
            logger.error(s"Node '$from': $problem")
            Left(problem)

          case _ =>
            Right(reportedClusterState)
        }
    ).map(_.toCompleted)

  private def update(from: NodeId, fromMustBeActive: Boolean, operationString: => String)
    (body: Option[State] => Checked[ClusterState])
  : Task[Checked[ClusterState]] =
    stateMVar.flatMap(mvar =>
      mvar.take.flatMap { current =>
        logger.trace(s"Node '$from': $operationString${current.fold("")(o => ", after " + o.lastHeartbeat.elapsed.pretty)}")
        current
          .match_ {
            case None =>
              // Initial state, we known nothing. So we accept the first visitor (?)
              // TODO But we cannot know whether the first visitor is right
              Checked.unit

            case Some(state) =>
              if (fromMustBeActive && !couldBeActive(from, state)) {
                val problem = ClusterWatchInactiveNodeProblem(from,
                  state.clusterState, state.lastHeartbeat.elapsed, operationString)
                val msg = s"Node '$from': $problem"
                logger.error(msg)
                Left(problem)
              } else
                Checked.unit
          }
          .flatMap(_ => body(current)) match {
            case Left(problem) =>
              mvar.put(current)
                .map(_ => Left(problem))
            case Right(updated) =>
              if (!current.exists(_.clusterState == updated)) {
                logger.info(s"Node '$from' changed ClusterState to $updated")
              }
              mvar.put(Some(State(updated, now)))
                .map(_ => Right(updated))
          }
      })

  private def couldBeActive(nodeId: NodeId, state: State): Boolean =
    state match {
      case State(clusterState: HasNodes, _)
        if clusterState.isNonEmptyActive(nodeId) =>
        true // Sure

      case state @ State(_: Coupled, _) =>
        !state.isLastHeartbeatStillValid // Not sure, because no heartbeat

      case _ =>
        false // Sure
    }

  private def now = MonixDeadline.now(scheduler)

  override def toString = s"ClusterWatch($controllerId)"
}

object ClusterWatch
{
  private val logger = Logger(getClass)

  private case class State(clusterState: ClusterState, lastHeartbeat: MonixDeadline)
  {
    def isLastHeartbeatStillValid =
      this match {
        case State(clusterState: HasNodes, lastHeartbeat) =>
          (lastHeartbeat + clusterState.timing.heartbeat).hasTimeLeft

        case State(ClusterState.Empty, _) => false // Should not happen
      }
  }

  private lazy val isClusterWatchProblemCode = Set[ProblemCode](
    ClusterWatchHeartbeatMismatchProblem.code,
    ClusterWatchEventMismatchProblem.code,
    ClusterWatchInactiveNodeProblem.code,
    InvalidClusterWatchHeartbeatProblem.code)

  def isClusterWatchProblem(problem: Problem): Boolean =
    problem.maybeCode exists isClusterWatchProblemCode

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

  final case class InvalidClusterWatchHeartbeatProblem(from: NodeId, clusterState: ClusterState) extends Problem.Coded {
    def arguments = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString)
  }
  object InvalidClusterWatchHeartbeatProblem extends Problem.Coded.Companion
}
