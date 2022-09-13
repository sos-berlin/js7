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
      .map(_
        .toChecked(Problem(
          s"ClusterWatch not yet started for Controller '$controllerId'"))
        .map(_.clusterState))

  def applyEvents(clusterWatchEvents: ClusterWatchEvents): Task[Checked[Completed]] = {
    import clusterWatchEvents.{events, from, clusterState => reportedClusterState}
    val fromMustBeActive = clusterWatchEvents.events match {
      case Seq(_: ClusterSwitchedOver) => false // Both nodes issue the (same) event
      case _ => true
    }
    update(from, fromMustBeActive = fromMustBeActive,
      s"event ${events.mkString(", ")} --> $reportedClusterState") {
      case None =>
        // Not yet initialized then no ClusterFailedOver
        if (events.exists(_.event.isInstanceOf[ClusterFailedOver]))
          Left(UntaughtClusterWatchProblem)
        else
          teach(from, reportedClusterState)

      case Some(current) =>
        if (current.clusterState == reportedClusterState) {
          logger.info(
            s"Node '$from': Ignore probably duplicate events for already reached clusterState=${current.clusterState}")
          Right(reportedClusterState)
        } else
          current.clusterState.applyEvents(events.map(NoKey <-: _)) match {
            case Left(problem) =>
              logger.error(s"Node '$from': $problem")
              val problem2 = ClusterWatchEventMismatchProblem(
                events, current.clusterState, reportedClusterState = reportedClusterState)
              logger.error(s"Node '$from': $problem2")
              Left(problem2)

            case Right(clusterState) =>
              for (event <- events) logger.info(s"Node '$from': $event")
              if (clusterState == reportedClusterState) {
                logger.info(s"Node '$from' changed ClusterState to $reportedClusterState")
                Right(reportedClusterState)
              } else
              if (!current.isLastHeartbeatStillValid) {
                // The node may have died just between sending the event to ClusterWatch and
                // persisting it. The we have different state.
                // TODO Maybe not safe.
                logger.warn(s"Node '$from' forced ClusterState to $reportedClusterState")
                Right(reportedClusterState)
              } else {
                val problem = ClusterWatchEventMismatchProblem(
                  events, clusterState, reportedClusterState = reportedClusterState)
                logger.error(s"Node '$from': $problem")
                Left(problem)
              }
          }
    }.map(_.toCompleted)
  }

  def heartbeat(from: NodeId, reportedClusterState: ClusterState): Task[Checked[Completed]] =
    update(from, fromMustBeActive = true, s"heartbeat $reportedClusterState")(current =>
      if (!reportedClusterState.isNonEmptyActive(from))
        Left(InvalidClusterWatchHeartbeatProblem(from, reportedClusterState))
      else
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
              // and the active node is restarted within the clusterWatchClientHeartbeatTimeout !!!
              val problem = ClusterWatchHeartbeatMismatchProblem(clusterState,
                reportedClusterState = reportedClusterState)
              logger.error(s"Node '$from': $problem")
              Left(problem)
            }

          case _ =>
            Right(reportedClusterState)
        }
    ).map(_.toCompleted)

  private def update(from: NodeId, fromMustBeActive: Boolean, operationString: => String)
    (body: Option[State] => Checked[ClusterState])
  : Task[Checked[ClusterState]] =
    stateMVar.flatMap(mvar =>
      mvar.take.flatMap { current =>
        logger.trace(s"Node '$from': $operationString${
          current.fold("")(o => ", after " + o.lastHeartbeat.elapsed.pretty)}")
        current
          .match_ {
            case None =>
              // Initial state, we known nothing. So we accept the first visitor (?)
              // TODO But we cannot know whether the first visitor is right
              Checked.unit

            case Some(state) =>
              if (!fromMustBeActive || state.couldBeActive(from))
                Checked.unit
              else {
                val problem = ClusterWatchInactiveNodeProblem(from,
                  state.clusterState, state.lastHeartbeat.elapsed, operationString)
                val msg = s"Node '$from': $problem"
                logger.error(msg)
                Left(problem)
              }
          }
          .flatMap(_ => body(current)) match {
            case Left(problem) =>
              mvar.put(current)
                .map(_ => Left(problem))
            case Right(updated) =>
              //if (!current.exists(_.clusterState == updated)) {
              //  logger.info(s"Node '$from' changed ClusterState to $updated")
              //}
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

  private case class State(clusterState: ClusterState, lastHeartbeat: MonixDeadline)
  {
    def isLastHeartbeatStillValid =
      this match {
        case State(clusterState: HasNodes, lastHeartbeat) =>
          (lastHeartbeat + clusterState.timing.heartbeat).hasTimeLeft

        case State(ClusterState.Empty, _) => false // Should not happen
      }

    def couldBeActive(nodeId: NodeId): Boolean =
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
    InvalidClusterWatchHeartbeatProblem.code)

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
}
