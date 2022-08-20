package js7.core.cluster.watch

import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixutils.MonixDeadline
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem, ProblemCode}
import js7.base.time.ScalaTime.*
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.core.cluster.watch.ClusterWatch.*
import js7.data.cluster.ClusterEvent.ClusterSwitchedOver
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration

final class ClusterWatch(controllerId: ControllerId, now: () => MonixDeadline)
extends ClusterWatchApi
{
  private val lock = AsyncLock("ClusterWatch")
  @volatile private var _state: Option[State] = None

  logger.trace(toString)

  def logout() = Task.pure(Completed)

  @TestOnly
  private[cluster] def isActive(id: NodeId): Task[Checked[Boolean]] =
    clusterState.map(_.map {
      case o: ClusterState.HasNodes => o.activeId == id
      case ClusterState.Empty => sys.error("ClusterState must not be Empty")
    })

  def applyEvents(clusterWatchEvents: ClusterWatchEvents): Task[Checked[Completed]] = {
    import clusterWatchEvents.{checkOnly, events, from, clusterState as reportedClusterState}
    val fromMustBeActive = clusterWatchEvents.events match {
      case Seq(_: ClusterSwitchedOver) => false
      case _ => true
    }
    update(from, fromMustBeActive = fromMustBeActive, reportedClusterState,
      checkOnly = checkOnly,
      s"event ${events.mkString(", ")} --> $reportedClusterState"
    ) { state =>
      state.clusterState.applyEvents(events.map(NoKey <-: _)) match {
        case Left(problem) =>
          if (!checkOnly) {
            logger.error(s"Node '$from': $problem")
            val prblm = ClusterWatchEventMismatchProblem(events, state.clusterState,
              reportedClusterState)
            logger.error(s"Node '$from': $prblm")
          }

        case Right(clusterState) =>
          if (clusterState != reportedClusterState) {
            val problem = ClusterWatchEventMismatchProblem(events, clusterState,
              reportedClusterState)
            if (state.isLastHeartbeatStillValid) {
              logger.error(s"Node '$from': $problem")
            } else {
              logger.warn(s"Node '$from': $problem")
            }
          }
      }
      Right(Completed)
    }
  }

  def heartbeat(from: NodeId, reportedClusterState: ClusterState): Task[Checked[Completed]] =
    if (!reportedClusterState.isNonEmptyActive(from))
      Task.pure(Left(InvalidClusterWatchHeartbeatProblem(from, reportedClusterState)))
    else
      update(from, fromMustBeActive = true, reportedClusterState, checkOnly = false,
        s"heartbeat $reportedClusterState"
      ) { state =>
        state.clusterState match {
          case hasNodes: HasNodes
            if state.isLastHeartbeatStillValid && reportedClusterState != hasNodes =>
            // May occur also when active node terminates after
            // emitting a ClusterEvent and before applyEvents to ClusterWatch,
            // and the active node is restarted within the heartbeatValidDuration !!!
            val problem = ClusterWatchHeartbeatMismatchProblem(hasNodes,
              reportedClusterState = reportedClusterState)
            logger.error(s"Node '$from': $problem")
            Left(problem)

          case _ =>
            Right(Completed)
        }
      }

  private def update(
    from: NodeId,
    fromMustBeActive: Boolean,
    reportedClusterState: ClusterState,
    checkOnly: Boolean,
    operationString: => String)
    (body: State => Checked[Completed])
  : Task[Checked[Completed]] =
    lock.lock(Task {
      val maybeState = _state
      logger.trace(
        s"Node '$from': $operationString${maybeState.fold("")(o =>
          ", after " + o.lastHeartbeat.elapsed.pretty)}")
      maybeState
        .match_ {
          case None =>
            logger.info(s"Node '$from' initialized ClusterState to $operationString")
            _state = Some(State(reportedClusterState, now()))
            Right(Completed)

          case Some(state) =>
            state.update(from, fromMustBeActive, operationString)(body(state))
        }
        .map { _ =>
          if (!checkOnly) {
            if (!_state.map(_.clusterState).contains(reportedClusterState)) {
              logger.info(s"Node '$from' changed ClusterState to $reportedClusterState")
            }
            _state = Some(State(reportedClusterState, now()))
          }
          Completed
        }
    })

  override def toString = s"ClusterWatch($controllerId)"

  def clusterState: Task[Checked[ClusterState]] =
    Task(sync.clusterState)

  @TestOnly object test {
    def clusterState: ClusterState =
      sync.clusterState.orThrow
  }

  object sync {
    def clusterState: Checked[ClusterState] =
      _state
        .map(_.clusterState)
        .toChecked(Problem(
          s"ClusterWatch not yet started for Controller '$controllerId'"))
  }
}

object ClusterWatch
{
  private val logger = Logger[this.type]

  private lazy val isClusterWatchProblemCode = Set[ProblemCode](
    ClusterWatchHeartbeatMismatchProblem.code,
    ClusterWatchEventMismatchProblem.code,
    ClusterWatchInactiveNodeProblem.code,
    InvalidClusterWatchHeartbeatProblem.code)

  def isClusterWatchProblem(problem: Problem): Boolean =
    problem.maybeCode exists isClusterWatchProblemCode

  private case class State(clusterState: ClusterState, lastHeartbeat: MonixDeadline) {
    def update(
      from: NodeId,
      fromMustBeActive: Boolean,
      operationString: => String)
      (body: => Checked[Completed])
    : Checked[Completed] = {
      logger.trace(s"Node '$from': $operationString after ${lastHeartbeat.elapsed.pretty}")
      mustBeStillActive(from, operationString).when(fromMustBeActive)
        .flatMap(_ => body) match {
          case Left(problem) => Left(problem)
          case Right(completed) => Right(completed)
        }
    }

    private def mustBeStillActive(from: NodeId, logLine: => String): Checked[Unit] =
      clusterState match {
        case hasNodes: HasNodes if isLastHeartbeatStillValid && !hasNodes.isNonEmptyActive(from) =>
          val problem = ClusterWatchInactiveNodeProblem(from, hasNodes, lastHeartbeat.elapsed,
            logLine)
          logger.error(s"Node '$from': $problem")
          Left(problem)
        case _ =>
          Right(())
      }

    def isLastHeartbeatStillValid =
      clusterState match {
        case hasNodes: HasNodes =>
          (lastHeartbeat + hasNodes.timing.heartbeatValidDuration).hasTimeLeft
        case _ => false
      }
  }

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
}
