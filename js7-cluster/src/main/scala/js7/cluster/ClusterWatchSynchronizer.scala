package js7.cluster

import akka.pattern.AskTimeoutException
import cats.effect.ExitCase
import java.util.ConcurrentModificationException
import js7.base.generic.Completed
import js7.base.log.Logger.syntax._
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.cluster.ClusterWatchSynchronizer._
import js7.common.system.startup.Halt.haltJava
import js7.core.cluster.{ClusterWatchEvents, HttpClusterWatch}
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterTiming}
import js7.data.node.NodeId
import monix.catnap.MVar
import monix.eval.{Fiber, Task}
import monix.execution.atomic.AtomicAny
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.DropNew

private final class ClusterWatchSynchronizer(
  ownId: NodeId,
  val clusterWatch: HttpClusterWatch,
  timing: ClusterTiming)
{
  private val heartbeat = AtomicAny[Option[Heartbeat]](None)

  def applyEvents(events: Seq[ClusterEvent], clusterState: ClusterState): Task[Checked[Completed]] =
    if (events.isEmpty)
      Task.pure(Right(Completed))
    else
      stopHeartbeating >>
        clusterWatch.applyEvents(ClusterWatchEvents(from = ownId, events, clusterState))
          .flatMapT { completed =>
            clusterState match {
              case clusterState: HasNodes if clusterState.activeId == ownId
                /*TODO && !events.forall(_.isInstanceOf[ClusterSettingUpdated])*/ =>
                startHeartbeating(clusterState)
                  .map(Right(_))
              case _ =>
                // The ClusterSwitchedOver event will be written to the journal after applyEvents.
                // So persistence.clusterState will reflect the outdated ClusterState for a short while.
                Task.pure(Right(completed))
            }
          }

  def startHeartbeating(clusterState: HasNodes, dontWait: Boolean = false): Task[Completed] =
    logger.traceTask(Task.defer {
      val h = new Heartbeat(clusterState)
      heartbeat.getAndSet(Some(h))
        .fold(Task.completed)(_.stop) /*just in case*/
        .flatMap(_ => h.doAHeartbeat.unless(dontWait) *> h.startDontWait)
    })

  def stopHeartbeating(implicit enclosing: sourcecode.Enclosing): Task[Completed] =
    Task.defer {
      heartbeat.getAndSet(None)
        .fold(Task.completed)(_.stop)
    }

  private final class Heartbeat(clusterState: HasNodes)
  {
    private val nr = heartbeatSessionNr.next()
    private val stopping = MVar.empty[Task, Unit]().memoize
    private val heartbeat = MVar.empty[Task, Fiber[Completed]]().memoize

    def startDontWait =
      Task.defer {
        logger.debug(s"Heartbeat ($nr) continues with $clusterState")
        CorrelId
          .bindNew(sendHeartbeats
            .guaranteeCase {
              case ExitCase.Error(t) =>
                logger.warn(s"Sending heartbeat to ClusterWatch failed: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
                haltJava(
                  s"💥 HALT after sending heartbeat to ClusterWatch failed: ${t.toStringWithCauses}",
                  restart = true)
              case ExitCase.Canceled => Task {
                logger.debug("Canceled")
              }
              case ExitCase.Completed =>
                stopping.flatMap(_.tryRead).map { maybe =>
                  logger.debug(s"Heartbeat ($nr) stopped")
                  if (maybe.isEmpty) logger.error("Heartbeat stopped by itself")
                }
            })
          .start
          .tapEval(fiber =>
            heartbeat.flatMap(_.tryPut(fiber))
          .flatMap { ok =>
            if (ok) Task.unit
            else fiber.cancel >> Task.raiseError(new ConcurrentModificationException(
              "Tried to start Cluster heartbeating twice"))
          })
          .as(Completed)
      }

    def stop(implicit enclosing: sourcecode.Enclosing): Task[Completed] =
      logger.traceTask(s"Heartbeat ($nr) stop, called by ${enclosing.value}")(
        Task.defer {
        stopping
          .flatMap(_.tryPut(()))
          .flatMap(_ => heartbeat)
          .flatMap(_.tryTake)
          .flatMap(_.fold(Task.completed)(_.join))
      })

    private def sendHeartbeats: Task[Completed] =
      Observable.intervalAtFixedRate(timing.heartbeat, timing.heartbeat)
        .whileBusyBuffer(DropNew(bufferSize = 2))
        .flatMap(_ => Observable.fromTask(
          doAHeartbeat
            .onErrorHandleWith { t =>
              logger.warn(s"sendHeartbeats: ${t.toStringWithCauses}",
                if (t.isInstanceOf[AskTimeoutException]) null else t.nullIfNoStackTrace)
              Task.raiseError(t)
            }))
        .takeUntilEval(stopping.flatMap(_.read))
        .completedL
        .as(Completed)

    def doAHeartbeat: Task[Completed] =
      stopping.flatMap(_.tryRead).flatMap {
        case Some(()) => Task.completed
        case None =>
          logger.trace(s"Heartbeat ($nr)")
          doACheckedHeartbeat(clusterState) map {
            case Left(problem) =>
              haltJava(s"🔥 HALT because ClusterWatch reported: $problem", restart = true)
            case Right(Completed) => Completed
          }
      }
  }

  def doACheckedHeartbeat(clusterState: HasNodes): Task[Checked[Completed]] =
    clusterWatch.heartbeat(from = ownId, clusterState)
      .materializeIntoChecked

  def uri = clusterWatch.baseUri
}

object ClusterWatchSynchronizer
{
  private val logger = Logger(getClass)
  private val heartbeatSessionNr = Iterator.from(1)
}
