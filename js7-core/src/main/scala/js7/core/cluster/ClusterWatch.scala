package js7.core.cluster

import js7.base.generic.Completed
import js7.base.monixutils.MonixDeadline
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem, ProblemCode}
import js7.base.time.ScalaTime._
import js7.common.scalautil.Logger
import js7.core.cluster.ClusterWatch._
import js7.data.cluster.{ClusterEvent, ClusterNodeId, ClusterState}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly
final class ClusterWatch(controllerId: ControllerId, scheduler: Scheduler)

extends ClusterWatchApi
{
  private val timeout = 5.s // FIXME Take from ClusterWatchHearbeat/Events
  private val stateMVar = MVar[Task].of(None: Option[State]).memoize

  logger.trace(toString)

  @TestOnly
  private[cluster] def isActive(id: ClusterNodeId): Task[Checked[Boolean]] =
    get.map(_.map {
      case o: ClusterState.HasNodes => o.activeId == id
      case _ => sys.error("ClusterState must be a HasNodes")
    })

  def get: Task[Checked[ClusterState]] =
    stateMVar.flatMap(_.read)
      .map(_.map(_.clusterState) toChecked Problem(s"ClusterWatch not yet started for Controller '$controllerId'"))

  /**
    * @param reportedClusterState the expected ClusterState after applying the `events` */
  def applyEvents(from: ClusterNodeId, events: Seq[ClusterEvent], reportedClusterState: ClusterState, force: Boolean): Task[Checked[Completed]] =
    update(from, force, s"ControllerId '$controllerId': applyEvents($from, $events, $reportedClusterState)") {
      case None =>  // Not yet initialized: we accept anything
        Right(reportedClusterState)
      case Some(current) =>
        if (current.clusterState == reportedClusterState) {
          logger.info(s"$toString: Ignored probably duplicate events for already reached clusterState=$current")
        } else {
          current.clusterState.applyEvents(events.map(NoKey <-: _)) match {
            case Left(problem) =>
              logger.warn(s"$toString, '$from' node: $problem")
              val superproblem: Problem = ClusterWatchEventMismatchProblem(events, current.clusterState, reportedClusterState = reportedClusterState)
              logger.warn(s"$toString, '$from' node: $superproblem")
            case Right(clusterState) =>
              if (clusterState != reportedClusterState)
                logger.warn(toString + ": " +
                  ClusterWatchEventMismatchProblem(events, clusterState, reportedClusterState = reportedClusterState))
          }
        }
        Right(reportedClusterState)
    } .map(_.toCompleted)

  def heartbeat(from: ClusterNodeId, reportedClusterState: ClusterState): Task[Checked[Completed]] =
    update(from, false, s"heartbeat($from, $reportedClusterState)")(current =>
      if (!reportedClusterState.isNonEmptyActive(from))
        Left(InvalidClusterWatchHeartbeatProblem(from, reportedClusterState))
      else {
        for (State(clusterState, _) <- current if clusterState != reportedClusterState)
          logger.warn(s"$toString, '$from' node: " +
            ClusterWatchHeartbeatMismatchProblem(clusterState, reportedClusterState = reportedClusterState))
        Right(reportedClusterState)
      }
    ).map(_.toCompleted)

  private def update(from: ClusterNodeId, force: Boolean, logLine: => String)(body: Option[State] => Checked[ClusterState])
  : Task[Checked[ClusterState]] =
    stateMVar.flatMap(mvar =>
      mvar.take.flatMap { current =>
        logger.trace(s"$toString, '$from' node: $logLine, after ${current.fold("—")(_.lastHeartbeat.elapsed.pretty)}")
        mustBeStillActive(from, current, force)
          .left.flatMap(problem =>
            if (force) Right(Completed) else Left(problem))
          .flatMap(_ => body(current)) match {
            case Left(problem) =>
              mvar.put(current)
                .map(_ => Left(problem))
            case Right(updated) =>
              if (!current.exists(_.clusterState == updated)) {
                logger.info(s"$toString: '$from' node changed ClusterState to $updated")
              }
              mvar.put(Some(State(updated, now)))
                .map(_ => Right(updated))
          }
      })

  private def mustBeStillActive(from: ClusterNodeId, state: Option[State], force: Boolean): Checked[Completed.type] =
    state match {
      case Some(State(clusterState, lastHeartbeat))
        if !clusterState.isNonEmptyActive(from) && (lastHeartbeat + timeout).hasTimeLeft =>
        val problem = ClusterWatchHeartbeatFromInactiveNodeProblem(from, clusterState)
        val msg = s"$toString, '$from' node: $problem"
        if (force) logger.debug(msg) else logger.warn(msg)
        Left(problem)
      case _ =>
        Right(Completed)
    }

  private def now = MonixDeadline.now(scheduler)

  override def toString = s"ClusterWatch(controllerId=$controllerId)"
}

object ClusterWatch
{
  private val logger = Logger(getClass)

  private case class State(clusterState: ClusterState, lastHeartbeat: MonixDeadline)

  private lazy val isClusterWatchProblemCode = Set[ProblemCode](
    ClusterWatchHeartbeatMismatchProblem.code,
    ClusterWatchEventMismatchProblem.code,
    ClusterWatchHeartbeatFromInactiveNodeProblem.code,
    InvalidClusterWatchHeartbeatProblem.code)

  def isClusterWatchProblem(problem: Problem): Boolean =
    problem.codeOption exists isClusterWatchProblemCode

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

  final case class ClusterWatchHeartbeatFromInactiveNodeProblem(from: ClusterNodeId, clusterState: ClusterState) extends Problem.Coded {
    def arguments = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString)
  }
  object ClusterWatchHeartbeatFromInactiveNodeProblem extends Problem.Coded.Companion

  final case class InvalidClusterWatchHeartbeatProblem(from: ClusterNodeId, clusterState: ClusterState) extends Problem.Coded {
    def arguments = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString)
  }
  object InvalidClusterWatchHeartbeatProblem extends Problem.Coded.Companion
}
