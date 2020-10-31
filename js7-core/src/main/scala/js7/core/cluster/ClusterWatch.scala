package js7.core.cluster

import js7.base.generic.Completed
import js7.base.monixutils.MonixDeadline
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem, ProblemCode}
import js7.base.time.ScalaTime._
import js7.common.scalautil.Logger
import js7.core.cluster.ClusterWatch._
import js7.data.cluster.{ClusterEvent, ClusterState}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.node.NodeId
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import org.jetbrains.annotations.TestOnly

final class ClusterWatch(controllerId: ControllerId, scheduler: Scheduler)
extends ClusterWatchApi
{
  private val timeout = 3.s // FIXME Take from ClusterWatchHearbeat/Events
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
    // TODO force=true nur bei ClusterSwitchedOver. Das können wir selbst erkennen, dann lastHearbeat mit alter Zeit ungültig machen.
    //  Dann eine Warnung/Fehler weniger, weil letzter Herzschlag vor ClusterSwitchedOver nicht mehr gilt.
    import clusterWatchEvents.{checkOnly, events, force, from, clusterState => reportedClusterState}
    // FIXME Geänderten ClusterState nur für die Frist setzen.
    //  Während der Frist kann ein folgendes Event oder ein folgender Herzschlag auf vorherigen oder reservierten ClusterState aufsetzen ?
    //  Dann kann ifClusterWatchAllowsActivation vorab prüfen, ob PassiveLost oder FailedOver möglich ist.
    //  Bei PassiveLost (vom Aktiven) kann währenddessen ein anderes ClusterEvent kommen. WIE VERHINDERN WIR DAS?
    update(from, force = force, checkOnly = checkOnly, s"event ${events.mkString(", ")} --> $reportedClusterState") {
      case None =>  // Not yet initialized: we accept anything
        Right(reportedClusterState)
      case Some(current) =>
        if (current.clusterState == reportedClusterState) {
          logger.info(s"Node '$from': Ignore probably duplicate events for already reached clusterState=${current.clusterState}")
        } else {
          current.clusterState.applyEvents(events.map(NoKey <-: _)) match {
            case Left(problem) =>
              if (!checkOnly) {
                logger.error(s"Node '$from': $problem")
                logger.error(s"Node '$from': " +
                  ClusterWatchEventMismatchProblem(events, current.clusterState, reportedClusterState = reportedClusterState))
              }
            case Right(clusterState) =>
              if (clusterState != reportedClusterState)
                logger.error(s"Node '$from': " +
                  ClusterWatchEventMismatchProblem(events, clusterState, reportedClusterState = reportedClusterState))
          }
        }
        Right(reportedClusterState)
    } .map(_.toCompleted)
  }

  def heartbeat(from: NodeId, reportedClusterState: ClusterState): Task[Checked[Completed]] =
    update(from, false, checkOnly = false/*???*/, s"heartbeat $reportedClusterState")(current =>
      if (!reportedClusterState.isNonEmptyActive(from))
        Left(InvalidClusterWatchHeartbeatProblem(from, reportedClusterState))
      else {
        for (State(clusterState, _) <- current if clusterState != reportedClusterState)
          logger.error(s"Node '$from': " +
            ClusterWatchHeartbeatMismatchProblem(clusterState, reportedClusterState = reportedClusterState))
        Right(reportedClusterState)
      }
    ).map(_.toCompleted)

  private def update(from: NodeId, force: Boolean, checkOnly: Boolean, operationString: => String)(body: Option[State] => Checked[ClusterState])
  : Task[Checked[ClusterState]] =
    stateMVar.flatMap(mvar =>
      mvar.take.flatMap { current =>
        logger.trace(s"Node '$from': $operationString, after ${current.fold("—")(_.lastHeartbeat.elapsed.pretty)}")
        mustBeStillActive(from, current, force, operationString)
          .left.flatMap(problem =>
            if (force) Right(Completed) else Left(problem))
          .flatMap(_ => body(current)) match {
            case Left(problem) =>
              mvar.put(current)
                .map(_ => Left(problem))
            case Right(updated) =>
              if (!current.exists(_.clusterState == updated)) {
                logger.info(s"Node '$from' changed ClusterState to $updated")
              }
              mvar.put(if (checkOnly) current else Some(State(updated, now)))
                .map(_ => Right(updated))
          }
      })

  private def mustBeStillActive(from: NodeId, state: Option[State], force: Boolean, logLine: => String): Checked[Completed.type] =
    state match {
      case Some(State(clusterState, lastHeartbeat))
      if !clusterState.isNonEmptyActive(from) && (lastHeartbeat + timeout).hasTimeLeft =>
        val problem = ClusterWatchInactiveNodeProblem(from, clusterState, logLine)
        val msg = s"Node '$from': $problem"
        if (force) logger.debug(msg) else logger.error(msg)
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

  final case class ClusterWatchInactiveNodeProblem(from: NodeId, clusterState: ClusterState, operation: String) extends Problem.Coded {
    def arguments = Map(
      "from" -> from.string,
      "clusterState" -> clusterState.toString,
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
