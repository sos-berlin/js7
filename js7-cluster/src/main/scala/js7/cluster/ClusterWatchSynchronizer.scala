package js7.cluster

import akka.pattern.AskTimeoutException
import cats.effect.ExitCase
import java.util.ConcurrentModificationException
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.AsyncVariable
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.ClusterWatchSynchronizer.*
import js7.common.system.startup.Halt.haltJava
import js7.core.cluster.{ClusterWatchEvents, HttpClusterWatch}
import js7.data.cluster.ClusterEvent.ClusterFailedOver
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterTiming}
import js7.data.node.NodeId
import monix.catnap.MVar
import monix.eval.{Fiber, Task}
import monix.execution.atomic.{Atomic, AtomicAny}
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.DropNew

private final class ClusterWatchSynchronizer private(ownId: NodeId, initialInlay: Inlay)
{
  def this(ownId: NodeId, clusterWatch: HttpClusterWatch, timing: ClusterTiming) =
    this(ownId, new Inlay(ownId, clusterWatch, timing))

  private val inlay = AsyncVariable(initialInlay)
  private val stopNesting = Atomic(0)
  private val stopNestingLock = AsyncLock()

  def start(clusterState: HasNodes): Task[Checked[Completed]] =
    inlay.value
      .flatMap(_
        .doACheckedHeartbeat(clusterState))
      .flatMapT(_ => Task.defer {
        logger.info("ClusterWatch agreed that this node is the active cluster node")
        inlay.value
          .flatMap(_.startHeartbeating(clusterState))
          .map(Right.apply)
      })

  def stop: Task[Completed] =
    inlay.value.flatMap(_
      .stop)

  def change(clusterState: ClusterState.HasNodes, clusterWatch: HttpClusterWatch)
  : Task[Unit] =
    logger.debugTask {
      assertThat(clusterState.activeId == ownId)
      suspendHeartbeat(
        Task.pure(clusterState),
        inlay
          .update(_
            .stop
            .as(new Inlay(ownId, clusterWatch, clusterState.timing)))
          .void)
    }

  def applyEvents(events: Seq[ClusterEvent], updatedClusterState: ClusterState)
  : Task[Checked[Completed]] =
    if (events.isEmpty)
      Task.pure(Right(Completed))
    else {
      val isFailover = events match {
        case Seq(o: ClusterFailedOver) => o.activatedId == ownId
        case _ => false
      }
      inlay.value.flatMap(myInlay =>
        suspendHeartbeat(
          Task.pure(updatedClusterState),
          myInlay.repeatWhenTooLong(
            myInlay.clusterWatch.applyEvents(
              ClusterWatchEvents(from = ownId, events, updatedClusterState))),
          isFailover = isFailover
          // A ClusterSwitchedOver event will be written to the journal after applyEvents.
          // So persistence.clusterState will reflect the outdated ClusterState for a short while.
        ))
    }

  def suspendHeartbeat[A](
    getClusterState: Task[ClusterState],
    operation: Task[A],
    isFailover: Boolean = false)
    (implicit enclosing: sourcecode.Enclosing)
  : Task[A] =
    logger.traceTask(
      startNestedSuspension *>
        operation
          .flatMap { result =>
            val continueHeartbeat = !isFailover || result.match_ {
              case Left(problem: Problem) =>
                logger.trace(
                  s"suspendHeartbeat: operation failed, heartbeating will not be resumed: $problem")
                false
              case _ => true
            }
            endNestedSuspension(inlay =>
              Task.when(continueHeartbeat)(
                getClusterState
                  .flatMap {
                    case clusterState: HasNodes if clusterState.activeId == ownId =>
                      inlay
                        .startHeartbeating(clusterState).void
                        .tapError(t => Task {
                          logger.warn(
                            s"suspendHeartbeat called by ${enclosing.value}: ${t.toStringWithCauses}",
                            t.nullIfNoStackTrace)
                        })
                    case _ => Task.unit
                  })
            ).as(result)
          }
          .guaranteeCase {
            case ExitCase.Completed => Task.unit
            case _ => endNestedSuspension(_ => Task.unit)
          })

  private def startNestedSuspension(implicit enclosing: sourcecode.Enclosing): Task[Unit] =
    stopNestingLock.lock(Task.defer {
      if (stopNesting.getAndIncrement() == 0)
        inlay.value.flatMap(_.stopHeartbeating).void
      else {
        assertThat(!isHeartbeating)
        Task.unit
      }
    })

  private def endNestedSuspension(onZeroNesting: Inlay => Task[Unit]): Task[Unit] =
    stopNestingLock.lock(Task.defer {
      assertThat(!isHeartbeating)
      Task.when(stopNesting.decrementAndGet() == 0)(
        inlay.value.flatMap(onZeroNesting))
    })

  def isHeartbeating =
    inlay.get.isHeartbeating

  def uri = inlay.get.uri
}

object ClusterWatchSynchronizer
{
  private val logger = Logger(getClass)
  private val heartbeatSessionNr = Iterator.from(1)

  private final class Inlay(
    ownId: NodeId,
    val clusterWatch: HttpClusterWatch,
    timing: ClusterTiming)
  {
    private val heartbeat = AtomicAny[Option[Heartbeat]](None)

    def uri = clusterWatch.baseUri

    def stop: Task[Completed] =
      stopHeartbeating *>
        clusterWatch.tryLogout

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

    def isHeartbeating =
      heartbeat.get().isDefined

    private final class Heartbeat(clusterState: HasNodes) {
      private val nr = heartbeatSessionNr.next()
      private val stopping = MVar.empty[Task, Unit]().memoize
      private val heartbeat = MVar.empty[Task, Fiber[Completed]]().memoize

      def startDontWait =
        CorrelId.bindNew(logger.debugTask(s"Heartbeat ($nr) fiber")(
          sendHeartbeats
            .guaranteeCase {
              case ExitCase.Error(t) =>
                logger.warn(s"Sending heartbeat to ClusterWatch failed: ${t.toStringWithCauses}", t.nullIfNoStackTrace)
                haltJava(
                  s"💥 HALT after sending heartbeat to ClusterWatch failed: ${t.toStringWithCauses}",
                  restart = true)

              case ExitCase.Canceled =>
                Task.unit

              case ExitCase.Completed =>
                stopping.flatMap(_.tryRead).map { maybe =>
                  if (maybe.isEmpty) logger.error("Heartbeat stopped by itself")
                }
            })
          .start
          .tapEval(fiber =>
            heartbeat
              .flatMap(_.tryPut(fiber))
              .flatMap(ok =>
                Task.when(!ok)(
                  fiber.cancel *>
                    Task.raiseError(new ConcurrentModificationException(
                      "Tried to start Cluster heartbeating twice")))))
          .as(Completed))

      def stop(implicit enclosing: sourcecode.Enclosing): Task[Completed] =
        logger.traceTask(s"Heartbeat ($nr) stop, called by ${enclosing.value}")(
          stopping
            .flatMap(_.tryPut(()))
            .flatMap(_ => heartbeat)
            .flatMap(_.tryTake)
            .flatMap(_.fold(Task.completed)(_.join))
            .logWhenItTakesLonger)

      private def sendHeartbeats: Task[Completed] =
        Observable.intervalAtFixedRate(timing.heartbeat)
          .whileBusyBuffer(DropNew(bufferSize = 2))
          // takeUntilEval before doAHeartbeat otherwise a heartbeat sticking in network congestion
          // would continue independently and arrive out of order (bad).
          .takeUntilEval(stopping.flatMap(_.read))
          .flatMap(_ => Observable.fromTask(
            doAHeartbeat
              .onErrorHandleWith { t =>
                logger.warn(s"sendHeartbeats: ${t.toStringWithCauses}",
                  if (t.isInstanceOf[AskTimeoutException]) null else t.nullIfNoStackTrace)
                Task.raiseError(t)
              }))
          .completedL
          .as(Completed)

      def doAHeartbeat: Task[Completed] =
        stopping.flatMap(_.tryRead).flatMap {
          case Some(()) => Task.completed
          case None =>
            logger.trace(s"Heartbeat ($nr) $clusterState")
            doACheckedHeartbeat(clusterState)
              .flatMap {
                case Right(Completed) => Task.pure(Completed)
                case Left(problem) =>
                stopping.flatMap(_.tryRead).map {
                  case Some(()) => Completed
                  case None =>
                    haltJava(s"🔥 HALT because ClusterWatch reported: $problem",
                      restart = true)
                }
              }
        }
    }

    def doACheckedHeartbeat(clusterState: HasNodes): Task[Checked[Completed]] =
      repeatWhenTooLong(clusterWatch
        .heartbeat(from = ownId, clusterState)
        .materializeIntoChecked)

    def repeatWhenTooLong[A](task: Task[A]): Task[A] =
      Task.tailRecM(())(_ => task
        .timed
        .map { case (duration, result) =>
          if (duration >= timing.clusterWatchReactionTimeout) {
            logger.info("ClusterWatch response time was too long: " + duration.pretty +
              ", retry after discarding " + result)
            Left(())
          } else
            Right(result)
        })
  }
}
