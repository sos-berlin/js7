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
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, SetOnce}
import js7.cluster.ClusterWatchSynchronizer.*
import js7.cluster.watch.api.AnyClusterWatch
import js7.common.system.startup.Halt.haltJava
import js7.core.cluster.watch.HttpClusterWatch
import js7.data.cluster.ClusterEvent.{ClusterPassiveLost, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{Coupled, HasNodes}
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterTiming}
import js7.data.node.NodeId
import monix.catnap.MVar
import monix.eval.{Fiber, Task}
import monix.execution.atomic.{Atomic, AtomicAny}
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.DropNew
import scala.annotation.tailrec

private final class ClusterWatchSynchronizer private(ownId: NodeId, initialInlay: Inlay)
{
  def this(ownId: NodeId, clusterWatch: AnyClusterWatch, timing: ClusterTiming) =
    this(ownId, new Inlay(clusterWatch, timing))

  private val inlay = AsyncVariable(initialInlay)
  private val suspendNesting = Atomic(0)
  private val suspendNestingLock = AsyncLock()
  private val registerClusterWatchId = SetOnce[RegisterClusterWatchId]

  // The calling ActiveClusterNode is expected to have locked clusterStateLock !!!
  def start(clusterState: HasNodes, registerClusterWatchId: RegisterClusterWatchId)
  : Task[Checked[Completed]] =
    logger.debugTask(Task.defer {
      this.registerClusterWatchId := registerClusterWatchId
      askClusterWatch(clusterState, registerClusterWatchId)
        .when(clusterState.setting.clusterWatchId.isDefined)
        .flatMapT(_ =>
          inlay.value
            .flatMap(_.startHeartbeating(clusterState, registerClusterWatchId))
            .map(Right.apply))
    })

  private def askClusterWatch(
    clusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId)
  : Task[Checked[Completed]] =
    Task.defer {
      val isCoupled = clusterState.isInstanceOf[Coupled]
      if (isCoupled) logger.info("Asking ClusterWatch")

      inlay.value
        .flatMap(_
          .doACheckedHeartbeat(
            clusterState,
            registerClusterWatchId,
            clusterWatchIdChangeAllowed = true,
            alreadyLocked = true))
        .map(_.map {
          case None => Completed
          case Some(confirm) =>
            if (isCoupled) logger.info(
              s"${confirm.clusterWatchId} agreed that this node is the active cluster node")
            Completed
        })
    }

  def stop: Task[Completed] =
    inlay.value.flatMap(_
      .stop)

  def change(clusterState: ClusterState.HasNodes, clusterWatch: AnyClusterWatch): Task[Unit] =
    logger.debugTask {
      assertThat(clusterState.activeId == ownId)
      suspendHeartbeat(Task.pure(clusterState))(
        inlay
          .update(_
            .stop
            .as(new Inlay(clusterWatch, clusterState.timing)))
          .void)
    }

  def applyEvent(event: ClusterEvent, updatedClusterState: HasNodes)
  : Task[Checked[Option[ClusterWatchConfirm]]] =
    Task.defer {
      inlay.value.flatMap(myInlay =>
        event match {
          case _: ClusterPassiveLost =>
            suspendHeartbeat(Task.pure(updatedClusterState))(
              myInlay.applyEvent(event, updatedClusterState))

          case _ =>
            // ClusterSwitchedOver must be emitted by the passive cluster node,
            // so we do not suspend a heartbeat (because heartbeat restart would fail).
            // A ClusterSwitchedOver event will be written to the journal after applyEvent.
            // So persistence.clusterState will reflect the outdated ClusterState for a short while.
            myInlay
              .applyEvent(event, updatedClusterState)
              .flatTapT(_ => Task
                // ClusterWatchRegistered may be emitted by the background heartbeat, which
                // does not suspend heartbeat (it would suspend/kill itself).
                // So we send the updated ClusterState directly to the heartbeat (if running)
                .when(event.isInstanceOf[ClusterWatchRegistered])(Task {
                  myInlay.changeClusterState(updatedClusterState)
                })
                .as(Checked.unit))
        })
    }

  def suspendHeartbeat[A](getClusterState: Task[ClusterState], forEvent: Boolean = false)
    (task: Task[A])
    (implicit enclosing: sourcecode.Enclosing)
  : Task[A] =
    logger.traceTask(
      startNestedSuspension *>
        task
          .<*(
            endNestedSuspension(inlay =>
              getClusterState
                .flatMap {
                  case clusterState: HasNodes if clusterState.activeId == ownId =>
                    inlay
                      .continueHeartbeating(
                        clusterState,
                        registerClusterWatchId.orThrow,
                        forEvent = forEvent)
                      .void
                      .tapError(t => Task {
                        logger.warn(
                          s"suspendHeartbeat called by ${enclosing.value}: ${t.toStringWithCauses}",
                          t.nullIfNoStackTrace)
                      })
                  case _ => Task.unit
                }))
          .guaranteeCase {
            case ExitCase.Completed => Task.unit
            case _ => endNestedSuspension(_ => Task.unit)
          })

  private def startNestedSuspension(implicit enclosing: sourcecode.Enclosing): Task[Unit] =
    suspendNestingLock.lock(Task.defer {
      if (suspendNesting.getAndIncrement() == 0)
        inlay.value.flatMap(_.stopHeartbeating).void
      else {
        assertThat(!isHeartbeating)
        Task.unit
      }
    })

  private def endNestedSuspension(onZeroNesting: Inlay => Task[Unit]): Task[Unit] =
    suspendNestingLock.lock(Task.defer {
      assertThat(!isHeartbeating)
      Task.when(suspendNesting.decrementAndGet() == 0)(
        inlay.value.flatMap(onZeroNesting))
    })

  private def restartHeartbeat() = {
    suspendNestingLock.lock(Task.defer {
      if (suspendNesting.getAndIncrement() == 0)
        inlay.value.flatMap(_.stopHeartbeating).void
      else {
        assertThat(!isHeartbeating)
        Task.unit
      }
    })
  }

  def isHeartbeating =
    inlay.get.isHeartbeating

  def uri = inlay.get.uri
}

object ClusterWatchSynchronizer
{
  private val logger = Logger(getClass)
  private val heartbeatSessionNr = Iterator.from(1)

  private type RegisterClusterWatchId = (ClusterWatchConfirm, Boolean) => Task[Checked[Unit]]

  private final class Inlay(
    val clusterWatch: AnyClusterWatch,
    timing: ClusterTiming)
  {
    private val heartbeat = AtomicAny[Option[Heartbeat]](None)

    def uri = clusterWatch match {
      case o: HttpClusterWatch => Some(o.baseUri)
      case _ => None
    }

    def stop: Task[Completed] =
      stopHeartbeating *>
        clusterWatch.tryLogout

    def applyEvent(event: ClusterEvent, updatedClusterState: HasNodes) =
      repeatWhenTooLong(
        clusterWatch.applyEvent(event, updatedClusterState))

    // forEvent = true: do not check and wait ClusterState after an event has applied.
    // We suppress this to simplify testing.
    def continueHeartbeating(
      clusterState: HasNodes,
      registerClusterWatchId: RegisterClusterWatchId,
      forEvent: Boolean)
    : Task[Completed] =
      logger.traceTask(
        doACheckedHeartbeat(clusterState, registerClusterWatchId)
          .rightAs(())
          .when(!forEvent && clusterState.setting.clusterWatchId.isDefined)
          .*>(Task.defer {
            val h = new Heartbeat(clusterState, registerClusterWatchId)
            heartbeat.getAndSet(Some(h))
              .fold(Task.completed)(_.stop) /*just in case*/
              .*>(h.start)
          }))

    def startHeartbeating(
      clusterState: HasNodes,
      registerClusterWatchId: RegisterClusterWatchId)
    : Task[Completed] =
      logger.traceTask(Task.defer {
        val h = new Heartbeat(clusterState, registerClusterWatchId)
        heartbeat.getAndSet(Some(h))
          .fold(Task.completed)(_.stop) /*just in case*/
          .*>(h.start)
      })

    def stopHeartbeating(implicit enclosing: sourcecode.Enclosing): Task[Completed] =
      Task.defer {
        logger.trace(s"stopHeartbeating called by ${enclosing.value}")
        heartbeat.getAndSet(None)
          .fold(Task.completed)(_.stop)
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
      registerClusterWatchId: RegisterClusterWatchId)
    {
      @volatile private var clusterState = initialClusterState
      private val nr = heartbeatSessionNr.next()
      private val stopping = MVar.empty[Task, Unit]().memoize
      private val heartbeat = MVar.empty[Task, Fiber[Completed]]().memoize

      def start: Task[Completed] =
        CorrelId.bindNew(logger.debugTask(s"Heartbeat ($nr) fiber")(
          sendHeartbeats
            .guaranteeCase {
              case ExitCase.Error(t) =>
                logger.warn(s"Sending heartbeat to ClusterWatch failed: ${t.toStringWithCauses}",
                  t.nullIfNoStackTrace)
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

      def changeClusterState(clusterState: HasNodes): Unit =
        this.clusterState = clusterState

      private def sendHeartbeats: Task[Completed] =
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
            logger.trace(s"Heartbeat ($nr) $clusterState")
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
      clusterWatchIdChangeAllowed: Boolean = false,
      alreadyLocked: Boolean = false)
    : Task[Checked[Option[ClusterWatchConfirm]]] =
      logger.debugTask(
        repeatWhenTooLong(clusterWatch
          .checkClusterState(
            clusterState,
            clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed)
          .materializeIntoChecked
        ).flatMapT {
          case None =>
            Task.right(None)

          case Some(confirm) =>
            if (clusterState.setting.clusterWatchId contains confirm.clusterWatchId)
              Task.right(Some(confirm))
            else if (clusterWatchIdChangeAllowed)
              registerClusterWatchId(confirm, alreadyLocked)
                .rightAs(Some(confirm))
            else
              // Not expected
              Task.left(Problem(s"New ${confirm.clusterWatchId} cannot be registered now"))
        })

    private def repeatWhenTooLong[A](task: Task[A]): Task[A] =
      Task.tailRecM(())(_ => task
        .timed
        .map { case (duration, result) =>
          if (duration >= timing.clusterWatchReactionTimeout) {
            logger.info("ClusterWatch response time was too long: " + duration.pretty +
              ", asking after discarding " + result)
            Left(())
          } else
            Right(result)
        })
  }
}
