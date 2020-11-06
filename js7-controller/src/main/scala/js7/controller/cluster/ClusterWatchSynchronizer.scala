package js7.controller.cluster

import akka.pattern.AskTimeoutException
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax.RichCheckedTask
import js7.base.problem.Checked
import js7.base.utils.LockResource
import js7.base.utils.ScalaUtils.syntax._
import js7.common.scalautil.Logger
import js7.common.system.startup.Halt.haltJava
import js7.controller.cluster.ClusterWatchSynchronizer._
import js7.core.cluster.ClusterWatch.ClusterWatchInactiveNodeProblem
import js7.core.cluster.HttpClusterWatch
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
  private val lock = LockResource()

  def stop(): Unit =
    heartbeat.get().cancel()

  def applyEvents(events: Seq[ClusterEvent], clusterState: ClusterState, force: Boolean = false): Task[Checked[Completed]] =
    lock.use(_ =>
      clusterWatch.applyEvents(from = ownId, events, clusterState, force = force)
        .flatMapT { completed =>
          clusterState match {
            case clusterState: HasNodes if clusterState.activeId == ownId =>
              startHeartbeatingWhileLocked(clusterState)
                .map(Right(_))
            case _ =>
              // The ClusterSwitchedOver event will be written to the journal after applyEvents.
              // So persistence.clusterState will reflect the outdated ClusterState for a short while.
              Task.pure(Right(completed))
          }
        })

  def startHeartbeating: Task[Completed] =
    lock.use(_ => currentClusterState.flatMap {
      case clusterState: HasNodes => startHeartbeatingWhileLocked(clusterState)
      case _ => Task.pure(Completed)
    })

  def startHeartbeating(clusterState: HasNodes): Task[Completed] =
    lock.use(_ => startHeartbeatingWhileLocked(clusterState))

  private def startHeartbeatingWhileLocked(clusterState: HasNodes): Task[Completed] =
  {
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

    def doAHeartbeat: Task[Unit] =
      clusterWatch.heartbeat(from = ownId, clusterState)
        .materializeIntoChecked
        .flatMap(checked =>
          Task.now {
            for (problem <- checked.left) {
              if (problem is ClusterWatchInactiveNodeProblem) {
                haltJava(s"EMERGENCY STOP due to: $problem", restart = true)
              }
              // Ignore other errors and continue
              logger.warn(s"ClusterWatch heartbeat: $problem")
            }
          })

    doAHeartbeat.flatMap(_ =>
      Task {
        val heartbeatFuture = sendHeartbeats
          .onCancelRaiseError(new CancelledException)
          .onErrorRecover { case _: CancelledException => Completed }
          .runToFuture
          .andThen {
            case Success(Completed) =>
            case Failure(t) =>
              logger.warn(s"Error when sending heartbeat to ClusterWatch: ${t.toStringWithCauses}")
              logger.debug(s"Error when sending heartbeat to ClusterWatch: $t", t)
              haltJava(s"EMERGENCY STOP due unreachable ClusterWatch: ${t.toStringWithCauses}", restart = true)
          }
        val previousHeartbeat = heartbeat.getAndSet(heartbeatFuture)
        previousHeartbeat.cancel()
        Completed
      })
  }

  def stopHeartbeating: Task[Completed] =
    Task.defer {
      val h = heartbeat.get()
      h.cancel()
      Task.fromFuture(h).onErrorHandle(_ => Completed)
    }

  def uri = clusterWatch.baseUri
}

object ClusterWatchSynchronizer
{
  private val logger = Logger(getClass)

  private final class CancelledException extends Exception
}
