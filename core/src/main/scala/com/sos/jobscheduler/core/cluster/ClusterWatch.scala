package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.monixutils.MonixDeadline
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem, ProblemCode}
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.cluster.ClusterWatch._
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.common.Uri
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.master.MasterId
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.Seq

final class ClusterWatch(masterId: MasterId, scheduler: Scheduler)
extends ClusterWatchApi
{
  private val timeout = 5.s // FIXME Take from ClusterWatchHearbeat/Events
  private val stateMVar = MVar[Task].of(None: Option[State]).memoize

  logger.trace(s"MasterId '$masterId'")

  def isActive(primary: Uri): Task[Checked[Boolean]] =
    get.map(_.map(_.isActive(primary)))

  def get: Task[Checked[ClusterState]] =
    stateMVar.flatMap(_.read)
      .map(_.map(_.clusterState) toChecked Problem(s"ClusterWatch not yet started for Master '$masterId'"))

  /**
    * @param reportedClusterState the expected ClusterState after applying the `events` */
  def applyEvents(from: Uri, events: Seq[ClusterEvent], reportedClusterState: ClusterState): Task[Checked[Completed]] =
    (update(from, s"MasterId '$masterId': applyEvents($from, $events, $reportedClusterState)") {
      case None =>  // Not yet initialized: we accept anything
        Right(reportedClusterState)
      case Some(current) =>
        current.clusterState.applyEvents(events.map(NoKey <-: _)) match {
          case Left(problem) =>
            logger.warn(s"Master '${masterId.string}': $problem")
            val superproblem: Problem = ClusterWatchEventMismatchProblem(events, current.clusterState, reportedClusterState = reportedClusterState)
            logger.warn(s"Master '${masterId.string}': $superproblem")
            Right(reportedClusterState)
          case Right(clusterState) =>
            Right(clusterState)
        }
    })
    .map(_.toCompleted)

  def heartbeat(from: Uri, reportedClusterState: ClusterState): Task[Checked[Completed]] =
    update(from, s"MasterId '$masterId': heartbeat($from, $reportedClusterState)")(current =>
      if (!reportedClusterState.isActive(from))
        Left(ClusterWatchHeartbeatFromInactiveNodeProblem(from, reportedClusterState))
      else {
        current match {
          case Some(State(clusterState, _)) if clusterState != reportedClusterState =>
            logger.warn(ClusterWatchHeartbeatMismatchProblem(clusterState, reportedClusterState = reportedClusterState).toString)
          case _ =>
        }
        Right(reportedClusterState)
      }
    ).map(_.toCompleted)

  private def update(from: Uri, logLine: => String)(body: Option[State] => Checked[ClusterState]): Task[Checked[ClusterState]] =
    stateMVar.flatMap(mvar =>
      mvar.take.flatMap { current =>
        logger.trace(s"$logLine age=${current.fold("—")(_.lastHeartbeat.elapsed.pretty)}")
        mustBeStillActive(from, current)
          .flatMap(_ => body(current)) match {
            case Left(problem) =>
              mvar.put(current)
                .map(_ => Left(problem))
            case Right(updated) =>
              if (!current.contains(updated)) {
                logger.info(s"ClusterWatch($masterId) ClusterState changed to $updated")
              }
              mvar.put(Some(State(updated, now)))
                .map(_ => Right(updated))
          }
      })

  private def mustBeStillActive(from: Uri, state: Option[State]) =
    state match {
      case Some(State(clusterState: ClusterState.HasActiveNode, lastHeartbeat))
        if clusterState.activeUri != from && (lastHeartbeat + timeout).hasTimeLeft =>
        val problem = OtherClusterNodeStillActiveProblem(clusterState.activeUri, wannabeActiveUri = from)
        logger.warn(problem.toString)
        Left(problem)
      case _ =>
        Right(Completed)
    }

  private def now = MonixDeadline.now(scheduler)
}

object ClusterWatch
{
  private val logger = Logger(getClass)

  private case class State(clusterState: ClusterState, lastHeartbeat: MonixDeadline)

  private lazy val isClusterWatchProblemCode = Set[ProblemCode](
    ClusterWatchHeartbeatMismatchProblem.code,
    ClusterWatchEventMismatchProblem.code,
    OtherClusterNodeStillActiveProblem.code,
    ClusterWatchHeartbeatFromInactiveNodeProblem.code)

  def isClusterWatchProblem(problem: Problem): Boolean =
    problem.codeOption exists isClusterWatchProblemCode

  final case class ClusterWatchHeartbeatMismatchProblem(
    currentClusterState: ClusterState,
    reportedClusterState: ClusterState)
  extends Problem.Coded
  {
    //"Master's ClusterState $currentClusterState does not match registered $clusterState")
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
    //"Master's ClusterState $currentClusterState does not match registered $clusterState")
    def arguments = Map(
      "events" -> events.mkString(", "),
      "currentClusterState" -> currentClusterState.toString,
      "reportedClusterState" -> reportedClusterState.toString)
  }
  object ClusterWatchEventMismatchProblem extends Problem.Coded.Companion

  final case class OtherClusterNodeStillActiveProblem(activeUri: Uri, wannabeActiveUri: Uri) extends Problem.Coded {
    def arguments = Map(
      "activeUri" -> activeUri.string,
      "wannabeActiveUri" -> wannabeActiveUri.string)
  }
  object OtherClusterNodeStillActiveProblem extends Problem.Coded.Companion

  final case class ClusterWatchHeartbeatFromInactiveNodeProblem(inactiveUri: Uri, clusterState: ClusterState) extends Problem.Coded {
    def arguments = Map(
      "inactiveUri" -> inactiveUri.string,
      "clusterState" -> clusterState.toString)
  }
  object ClusterWatchHeartbeatFromInactiveNodeProblem extends Problem.Coded.Companion

  //final case class ClusterNodeDeactivatedProblem(uri: Uri, activeUri: Uri) extends Problem.Coded {
  //  def arguments = Map(
  //    "uri" -> uri.string,
  //    "activeUri" -> activeUri.string
  //  )
  //}
}
