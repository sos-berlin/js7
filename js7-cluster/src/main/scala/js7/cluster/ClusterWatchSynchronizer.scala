package js7.cluster

import akka.pattern.AskTimeoutException
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax.RichCheckedTask
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.cluster.ClusterWatchSynchronizer._
import js7.common.scalautil.Logger
import js7.common.system.startup.Halt.haltJava
import js7.core.cluster.{ClusterWatchEvents, HttpClusterWatch}
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterTiming}
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.atomic.AtomicAny
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.DropNew
import scala.util.{Failure, Success}

private final class ClusterWatchSynchronizer(
  ownId: NodeId,
  currentClusterState: Task[ClusterState],
  val clusterWatch: HttpClusterWatch,
  timing: ClusterTiming)
  (implicit scheduler: Scheduler)
{
  private val heartbeat = AtomicAny[CancelableFuture[Completed]](CancelableFuture.successful(Completed))

  def stop(): Unit =
    heartbeat.get().cancel()

  def applyEvents(events: Seq[ClusterEvent], clusterState: ClusterState, checkOnly: Boolean = false)
  : Task[Checked[Completed]] =
    stopHeartbeating >>
      clusterWatch.applyEvents(ClusterWatchEvents(from = ownId, events, clusterState, checkOnly = checkOnly))
        .flatMapT { completed =>
          clusterState match {
            case clusterState: HasNodes if clusterState.activeId == ownId && !checkOnly =>
              continueHeartbeating(clusterState)
                .map(Right(_))
            case _ =>
              // The ClusterSwitchedOver event will be written to the journal after applyEvents.
              // So persistence.clusterState will reflect the outdated ClusterState for a short while.
              Task.pure(Right(completed))
          }
        }

  def startHeartbeating: Task[Completed] =
    currentClusterState.flatMap {
      case clusterState: HasNodes => continueHeartbeating(clusterState)
      case _ => Task.pure(Completed)
    }

  def startHeartbeating(clusterState: HasNodes): Task[Checked[Completed]] =
    doACheckedHeartbeat(clusterState)
      .flatMapT(_ =>
        continueHeartbeating(clusterState)
          .map(Right.apply))

  private def continueHeartbeating(clusterState: HasNodes): Task[Completed] =
  {
    val nr = heartbeatSessionNr.next()

    def start = Task {
      logger.debug(s"Heartbeat ($nr) starts with $clusterState")
      val heartbeatFuture = sendHeartbeats
        .onCancelRaiseError(new CanceledException)
        .onErrorRecover { case _: CanceledException => Completed }
        .runToFuture
        .andThen {
          case Success(Completed) =>
            logger.debug(s"Heartbeat ($nr) stopped")
          case Failure(t) =>
            logger.warn(s"Error when sending heartbeat to ClusterWatch: ${t.toStringWithCauses}")
            logger.debug(s"Error when sending heartbeat to ClusterWatch: $t", t)
            haltJava(s"HALT due to unreachable ClusterWatch: ${t.toStringWithCauses}", restart = true)
        }
      heartbeat := heartbeatFuture
      Completed
    }

    def sendHeartbeats: Task[Nothing] =
      Observable.intervalAtFixedRate(timing.heartbeat, timing.heartbeat)
        .whileBusyBuffer(DropNew(bufferSize = 2))
        .flatMap(_ => Observable.fromTask(
          doAHeartbeat
            .onErrorHandleWith { t =>
              logger.warn(s"sendHeartbeats: ${t.toStringWithCauses}",
                if (t.isInstanceOf[AskTimeoutException]) null else t.nullIfNoStackTrace)
              Task.raiseError(t)
            }))
        .completedL
        .flatMap(_ => Task.raiseError(new AssertionError("sendHeartbeats terminated unexpectedly")))

    def doAHeartbeat: Task[Completed] =
      Task.defer {
        logger.trace(s"Heartbeat ($nr)")
        doACheckedHeartbeat(clusterState) map {
          case Left(problem) => haltJava(s"HALT because ClusterWatch reported: $problem", restart = true)
          case Right(Completed) => Completed
        }
      }

    stopHeartbeating >> doAHeartbeat >> start
  }

  private def doACheckedHeartbeat(clusterState: HasNodes): Task[Checked[Completed]] =
    clusterWatch.heartbeat(from = ownId, clusterState)
      .materializeIntoChecked

  def stopHeartbeating: Task[Completed] =
    Task.defer {
      val h = heartbeat.getAndSet(CancelableFuture.successful(Completed))
      h.cancel()
      Task.fromFuture(h).onErrorHandle(_ => Completed)
    }

  def uri = clusterWatch.baseUri
}

object ClusterWatchSynchronizer
{
  private val logger = Logger(getClass)
  private val heartbeatSessionNr = Iterator.from(1)

  private final class CanceledException extends Exception
}
