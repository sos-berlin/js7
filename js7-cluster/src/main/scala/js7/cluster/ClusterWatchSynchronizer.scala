package js7.cluster

import akka.pattern.AskTimeoutException
import cats.effect.ExitCase
import java.util.ConcurrentModificationException
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, SetOnce}
import js7.cluster.ClusterWatchSynchronizer.*
import js7.cluster.watch.api.ClusterWatchConfirmation
import js7.common.system.startup.Halt.haltJava
import js7.data.cluster.ClusterEvent.{ClusterPassiveLost, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterTiming}
import js7.data.node.NodeId
import monix.catnap.MVar
import monix.eval.{Fiber, Task}
import monix.execution.atomic.{Atomic, AtomicAny}
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.DropNew
import scala.annotation.tailrec

private final class ClusterWatchSynchronizer(
  ownId: NodeId,
  clusterWatch: ClusterWatchCounterpart,
  timing: ClusterTiming)
{
  private val suspendNesting = Atomic(0)
  private val suspendNestingLock = AsyncLock()
  private val registerClusterWatchId = SetOnce[RegisterClusterWatchId]
  private val heartbeat = AtomicAny[Option[Heartbeat]](None)

  // The calling ActiveClusterNode is expected to have locked clusterStateLock !!!
  def start(clusterState: HasNodes, registerClusterWatchId: RegisterClusterWatchId)
  : Task[Checked[Completed]] =
    logger.debugTask(Task.defer {
      this.registerClusterWatchId := registerClusterWatchId
      // Due to clusterWatchIdChangeAllowed = true, the ClusterWatch should always agree.
      // This is more to teach a recently started ClusterWatch.
      askClusterWatch(clusterState, registerClusterWatchId)
        .when(clusterState.setting.clusterWatchId.isDefined)
        .flatMapT(_ =>
          startHeartbeating(clusterState, registerClusterWatchId)
            .map(Right.apply))
    })

  private def askClusterWatch(
    clusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId)
  : Task[Checked[Completed]] =
    Task.defer {
      assertThat(clusterState.activeId == ownId)
      val logAsInfo = clusterState.isInstanceOf[HasNodes]
      if (logAsInfo) logger.info("Asking ClusterWatch")

      doACheckedHeartbeat(
        clusterState,
        registerClusterWatchId,
        clusterWatchIdChangeAllowed = true,
        alreadyLocked = true
      ).map(_.map {
        case None => Completed
        case Some(confirm) =>
          if (logAsInfo) logger.info(
            s"${confirm.confirmer} agreed that this node is the active cluster node")
          Completed
      })
    }

  def stop: Task[Unit] =
    stopHeartbeating

  def applyEvent(event: ClusterEvent, updatedClusterState: HasNodes)
  : Task[Checked[Option[ClusterWatchConfirmation]]] =
    event match {
      case _: ClusterPassiveLost =>
        suspendHeartbeat(Task.pure(updatedClusterState))(
          clusterWatch.applyEvent(event, updatedClusterState))

      case _ =>
        // ClusterSwitchedOver must be emitted by the passive cluster node,
        // so we do not suspend a heartbeat (because heartbeat restart would fail).
        // A ClusterSwitchedOver event will be written to the journal after applyEvent.
        // So persistence.clusterState will reflect the outdated ClusterState for a short while.
        clusterWatch
          .applyEvent(event, updatedClusterState)
          .flatTapT(_ => Task
            // ClusterWatchRegistered may be emitted by the background heartbeat, which
            // does not suspend heartbeat (it would suspend/kill itself).
            // So we send the updated ClusterState directly to the heartbeat (if running)
            .when(event.isInstanceOf[ClusterWatchRegistered])(Task {
              changeClusterState(updatedClusterState)
            })
            .as(Checked.unit))
    }

  def suspendHeartbeat[A](getClusterState: Task[ClusterState], forEvent: Boolean = false)
    (task: Task[A])
    (implicit enclosing: sourcecode.Enclosing)
  : Task[A] =
    logger.traceTask(
      startNestedSuspension *>
        task
          .<*(
            endNestedSuspension(
              getClusterState
                .flatMap {
                  case clusterState: HasNodes if clusterState.activeId == ownId =>
                    continueHeartbeating(
                      clusterState,
                      registerClusterWatchId.orThrow,
                      forEvent = forEvent
                    ).tapError(t => Task {
                      logger.warn(
                        s"suspendHeartbeat called by ${enclosing.value}: ${t.toStringWithCauses}",
                        t.nullIfNoStackTrace)
                    })
                  case _ => Task.unit
                }))
          .guaranteeCase {
            case ExitCase.Completed => Task.unit
            case _ => endNestedSuspension(Task.unit)
          })

  private def startNestedSuspension(implicit enclosing: sourcecode.Enclosing): Task[Unit] =
    suspendNestingLock.lock(Task.defer {
      if (suspendNesting.getAndIncrement() == 0)
        stopHeartbeating
      else {
        assertThat(!isHeartbeating)
        Task.unit
      }
    })

  private def endNestedSuspension(onZeroNesting: Task[Unit]): Task[Unit] =
    suspendNestingLock.lock(Task.defer {
      assertThat(!isHeartbeating)
      Task.when(suspendNesting.decrementAndGet() == 0)(
        onZeroNesting)
    })

  private def restartHeartbeat() =
    suspendNestingLock.lock(Task.defer {
      if (suspendNesting.getAndIncrement() == 0)
        stopHeartbeating
      else {
        assertThat(!isHeartbeating)
        Task.unit
      }
    })

  // forEvent = true: do not check and wait ClusterState after an event has applied.
  // We suppress this to simplify testing.
  def continueHeartbeating(
    clusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId,
    forEvent: Boolean)
  : Task[Unit] =
    logger.traceTask(
      doACheckedHeartbeat(clusterState, registerClusterWatchId, clusterWatchIdChangeAllowed = false)
        .rightAs(())
        .when(!forEvent && clusterState.setting.clusterWatchId.isDefined)
        .*>(Task.defer {
          val h = new Heartbeat(clusterState, registerClusterWatchId)
          heartbeat.getAndSet(Some(h))
            .fold(Task.unit)(_.stop) /*just in case*/
            .*>(h.start.void)
        }))

  def startHeartbeating(
    clusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId)
  : Task[Completed] =
    logger.traceTask(Task.defer {
      val h = new Heartbeat(clusterState, registerClusterWatchId)
      heartbeat.getAndSet(Some(h))
        .fold(Task.completed)(_.stop.as(Completed)) /*just in case*/
        .*>(h.start)
    })

  def stopHeartbeating(implicit enclosing: sourcecode.Enclosing): Task[Unit] =
    Task.defer {
      logger.trace(s"stopHeartbeating called by ${enclosing.value}")
      heartbeat.getAndSet(None)
        .fold(Task.unit)(_.stop)
    }

  def changeClusterState(clusterState: HasNodes): Unit = {
    @tailrec def loop(maybeHeartbeat: Option[Heartbeat]): Unit =
      maybeHeartbeat match {
        case None =>
        case Some(h) =>
          h.changeClusterState(clusterState)
          val h2 = heartbeat.get()
          if (h2 ne maybeHeartbeat) loop(h2)
      }

    loop(heartbeat.get())
  }

  def isHeartbeating =
    heartbeat.get().isDefined

  private final class Heartbeat(
    initialClusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId) {
    @volatile private var clusterState = initialClusterState
    private val nr = heartbeatSessionNr.next()
    private val stopping = MVar.empty[Task, Unit]().memoize
    private val heartbeat = MVar.empty[Task, Fiber[Unit]]().memoize

    def start: Task[Completed] =
      CorrelId.bindNew(logger.debugTask(s"Heartbeat ($nr) fiber")(
        sendHeartbeats
          .guaranteeCase {
            case ExitCase.Error(t) =>
              logger.warn(s"Sending heartbeat to ClusterWatch failed: ${t.toStringWithCauses}",
                t.nullIfNoStackTrace)
              haltJava(
                s"🔥 HALT after sending heartbeat to ClusterWatch failed: ${t.toStringWithCauses}",
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

    def stop(implicit enclosing: sourcecode.Enclosing): Task[Unit] =
      logger.traceTask(s"Heartbeat ($nr) stop, called by ${enclosing.value}")(
        stopping
          .flatMap(_.tryPut(()))
          .flatMap(_ => heartbeat)
          .flatMap(_.tryTake)
          .flatMap(_.fold(Task.unit)(_.join))
          .logWhenItTakesLonger)

    def changeClusterState(clusterState: HasNodes): Unit =
      this.clusterState = clusterState

    private def sendHeartbeats: Task[Unit] =
      Observable.intervalAtFixedRate(timing.clusterWatchHeartbeat)
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
        // Again takeUntilEval to cancel a sticking doAHeartbeat
        .takeUntilEval(stopping.flatMap(_.read))
        .completedL
        .as(Completed)

    private def doAHeartbeat: Task[Completed] =
      stopping.flatMap(_.tryRead).flatMap {
        case Some(()) => Task.completed
        case None =>
          val clusterState = this.clusterState
          //logger.trace(s"Heartbeat ($nr) $clusterState")
          doACheckedHeartbeat(
            clusterState, registerClusterWatchId, clusterWatchIdChangeAllowed = true
          ).flatMap {
            case Left(problem) =>
              stopping.flatMap(_.tryRead).map {
                case Some(()) => Completed
                case None =>
                  haltJava(s"🔥 HALT because ClusterWatch heartbeat failed: $problem",
                    restart = true)
              }

            case Right(_) =>
              Task.pure(Completed)
          }
      }
  }

  def doACheckedHeartbeat(
    clusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId,
    clusterWatchIdChangeAllowed: Boolean,
    alreadyLocked: Boolean = false)
  : Task[Checked[Option[ClusterWatchConfirmation]]] =
    clusterWatch
      .checkClusterState(
        clusterState,
        clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed)
      .materializeIntoChecked
      .flatMapT {
        case None =>
          Task.right(None)

        case Some(confirmation: ClusterWatchConfirmation) =>
          if (clusterState.setting.clusterWatchId contains confirmation.clusterWatchId)
            Task.right(Some(confirmation))
          else if (clusterWatchIdChangeAllowed)
            registerClusterWatchId(confirmation, alreadyLocked)
              .rightAs(Some(confirmation))
          else
          // Not expected
            Task.left(Problem(s"New ${confirmation.clusterWatchId} cannot be registered now"))
      }
}

object ClusterWatchSynchronizer
{
  private val logger = Logger(getClass)
  private val heartbeatSessionNr = Iterator.from(1)

  private type RegisterClusterWatchId = (ClusterWatchConfirmation, Boolean) => Task[Checked[Unit]]
}
