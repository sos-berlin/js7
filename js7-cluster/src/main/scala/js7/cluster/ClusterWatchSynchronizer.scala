package js7.cluster

import cats.effect.std.Queue
import cats.effect.{Deferred, FiberIO, IO, Outcome}
import cats.syntax.option.*
import fs2.Stream
import java.util.ConcurrentModificationException
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.fs2utils.StreamExtensions.{interruptWhenF, prependOne}
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.tapError
import js7.base.problem.{Checked, Problem}
import js7.base.system.startup.Halt.haltJava
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.syntax.logWhenMethodTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, Atomic, SetOnce}
import js7.cluster.ClusterWatchSynchronizer.*
import js7.cluster.watch.api.ClusterWatchConfirmation
import js7.data.Problems.ClusterModuleShuttingDownProblem
import js7.data.cluster.ClusterEvent.{ClusterPassiveLost, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchProblems.ClusterPassiveLostWhileFailedOverTestingProblem
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterTiming}
import js7.data.node.NodeId
import org.apache.pekko.pattern.AskTimeoutException
import scala.annotation.tailrec

private final class ClusterWatchSynchronizer(
  ownId: NodeId,
  clusterWatch: ClusterWatchCounterpart,
  timing: ClusterTiming):

  private val suspendNesting = Atomic(0)
  private val suspendNestingLock = AsyncLock()
  private val registerClusterWatchId = SetOnce[RegisterClusterWatchId]
  private val heartbeat = Atomic(none[Heartbeat])

  // The calling ActiveClusterNode is expected to have locked clusterStateLock !!!
  def start(currentClusterState: IO[HasNodes], registerClusterWatchId: RegisterClusterWatchId)
  : IO[Checked[Completed]] =
    logger.debugIO(IO.defer:
      this.registerClusterWatchId := registerClusterWatchId
      // Due to clusterWatchIdChangeAllowed = true, the ClusterWatch should always agree.
      // This is more to teach a recently started ClusterWatch.
      currentClusterState
        .flatMap: clusterState =>
          askClusterWatch(clusterState, registerClusterWatchId)
            // The clusterState may have changed due to ClusterWatchRegistered
            .when(clusterState.setting.clusterWatchId.isDefined)
        .flatMapT: _ =>
          currentClusterState.flatMap:
            startHeartbeating(_, registerClusterWatchId)
              .map(Right.apply)
    )

  private def askClusterWatch(
    clusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId)
  : IO[Checked[Completed]] =
   logger.traceIO:
    IO.defer:
      assertThat(clusterState.activeId == ownId)
      val logAsInfo = clusterState.isInstanceOf[HasNodes]
      if logAsInfo then logger.info("Asking ClusterWatch")

      doACheckedHeartbeat(
        clusterState,
        registerClusterWatchId,
        clusterWatchIdChangeAllowed = true,
        alreadyLocked = true
      ).map(_.map:
        case None => Completed
        case Some(confirm) =>
          if logAsInfo then logger.info(
            s"${confirm.confirmer} agreed that this node is the active cluster node")
          Completed)

  def stop: IO[Unit] =
    stopHeartbeating

  def applyEvent(event: ClusterEvent, updatedClusterState: HasNodes,
    clusterWatchIdChangeAllowed: Boolean = false,
    forceWhenUntaught: Boolean = false)
  : IO[Checked[Option[ClusterWatchConfirmation]]] =
    event match
      case _: ClusterPassiveLost =>
        suspendHeartbeat(IO.pure(updatedClusterState)):
          clusterWatch.applyEvent(event, updatedClusterState,
            clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed,
            forceWhenUntaught = forceWhenUntaught)

      case _ =>
        // ClusterSwitchedOver must be emitted by the passive cluster node,
        // so we do not suspend a heartbeat (because heartbeat restart would fail).
        // A ClusterSwitchedOver event will be written to the journal after applyEvent.
        // So journal.clusterState will reflect the outdated ClusterState for a short while.
        clusterWatch
          .applyEvent(event, updatedClusterState,
            clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed)
          .flatTapT(_ => IO
            // ClusterWatchRegistered may be emitted by the background heartbeat, which
            // does not suspend heartbeat (it would suspend/kill itself).
            // So we send the updated ClusterState directly to the heartbeat (if running)
            .whenA(event.isInstanceOf[ClusterWatchRegistered])(IO:
              changeClusterState(updatedClusterState))
            .as(Checked.unit))

  def suspendHeartbeat[A](getClusterState: IO[ClusterState], forEvent: Boolean = false)
    (io: IO[Checked[A]])
    (implicit enclosing: sourcecode.Enclosing)
  : IO[Checked[A]] =
    logger.traceIO:
      startNestedSuspension *>
        io.flatTap: ioResult =>
          endNestedSuspension:
            getClusterState.flatMap:
              case clusterState: HasNodes
                if clusterState.activeId == ownId
                && ioResult != Left(ClusterPassiveLostWhileFailedOverTestingProblem)
                && ioResult != Left(ClusterModuleShuttingDownProblem) =>
                continueHeartbeating(
                  clusterState,
                  registerClusterWatchId.orThrow,
                  forEvent = forEvent
                ).tapError(t => IO:
                  logger.warn(
                    s"suspendHeartbeat called by ${enclosing.value}: ${t.toStringWithCauses}",
                    t.nullIfNoStackTrace))
              case _ => IO.unit
        .guaranteeCase:
          case Outcome.Succeeded(_) => IO.unit
          case _ => endNestedSuspension(IO.unit)

  private def startNestedSuspension(implicit enclosing: sourcecode.Enclosing): IO[Unit] =
    suspendNestingLock.lock:
      IO.defer:
        if suspendNesting.getAndIncrement() == 0 then
          stopHeartbeating
        else
          assertThat(!isHeartbeating)
          IO.unit

  private def endNestedSuspension(onZeroNesting: IO[Unit]): IO[Unit] =
    suspendNestingLock.lock:
      IO.defer:
        assertThat(!isHeartbeating)
        IO.whenA(suspendNesting.decrementAndGet() == 0):
          onZeroNesting

  private def restartHeartbeat() =
    suspendNestingLock.lock:
      IO.defer:
        if suspendNesting.getAndIncrement() == 0 then
          stopHeartbeating
        else
          assertThat(!isHeartbeating)
          IO.unit

  // forEvent = true: do not check and wait ClusterState after an event has applied.
  // We suppress this to simplify testing.
  private def continueHeartbeating(
    clusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId,
    forEvent: Boolean)
  : IO[Unit] =
    logger.traceIO:
      IO.whenA(!forEvent && clusterState.setting.clusterWatchId.isDefined):
        doACheckedHeartbeat(clusterState, registerClusterWatchId, clusterWatchIdChangeAllowed = false)
          .map:
            case Left(problem) => logger.warn(s"continueHeartbeating => $problem")
            case Right(_) =>
      .productR:
        IO.defer:
          val h = new Heartbeat(clusterState, registerClusterWatchId)
          heartbeat.getAndSet(Some(h))
            .fold(IO.unit)(_.stop)
            *> h.start.void

  private def startHeartbeating(
    clusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId)
  : IO[Completed] =
    logger.traceIO:
      IO.defer:
        val h = new Heartbeat(clusterState, registerClusterWatchId)
        heartbeat.getAndSet(Some(h))
          .fold(IO.completed)(_.stop.as(Completed)) /*just in case*/
          .*>(h.start)

  private def stopHeartbeating(implicit enclosing: sourcecode.Enclosing): IO[Unit] =
    IO.defer:
      logger.trace(s"stopHeartbeating called by ${enclosing.value}")
      heartbeat.getAndSet(None)
        .fold(IO.unit)(_.stop)

  private def changeClusterState(clusterState: HasNodes): Unit =
    @tailrec def loop(maybeHeartbeat: Option[Heartbeat]): Unit =
      maybeHeartbeat match
        case None =>
        case Some(h) =>
          h.changeClusterState(clusterState)
          val h2 = heartbeat.get()
          if h2 ne maybeHeartbeat then loop(h2)

    loop(heartbeat.get())

  def isHeartbeating =
    heartbeat.get().isDefined

  private final class Heartbeat(
    initialClusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId):

    @volatile private var clusterState = initialClusterState
    private val nr = heartbeatSessionNr.next()
    private val stopping = Deferred.unsafe[IO, Unit]
    private val heartbeat = memoize(Queue.bounded[IO, FiberIO[Unit]](1))

    def start: IO[Completed] =
      CorrelId.bindNew:
        logger.debugIO(s"Heartbeat ($nr) fiber"):
          sendHeartbeats
            .guaranteeCase:
              case Outcome.Errored(t) =>
                logger.warn(s"Sending heartbeat to ClusterWatch failed: ${t.toStringWithCauses}",
                  t.nullIfNoStackTrace)
                haltJava(
                  s"🔥 HALT after sending heartbeat to ClusterWatch failed: ${t.toStringWithCauses}",
                  restart = true)

              case Outcome.Canceled() =>
                IO.unit

              case Outcome.Succeeded(_) =>
                stopping.tryGet.map: maybe =>
                  if maybe.isEmpty then logger.error("Heartbeat stopped by itself")
        .startAndLogError
        .flatTap: fiber =>
          heartbeat
            .flatMap(_.tryOffer(fiber))
            .flatMap: ok =>
              IO.whenA(!ok):
                fiber.cancel *>
                  IO.raiseError(new ConcurrentModificationException(
                    "Tried to start Cluster heartbeating twice"))
        .as(Completed)

    def stop(implicit enclosing: sourcecode.Enclosing): IO[Unit] =
      logger.traceIO(s"Heartbeat ($nr) stop, called by ${enclosing.value}"):
        stopping.complete(())
          .flatMap(_ => heartbeat).flatMap(_.tryTake)
          .flatMap(_.fold(IO.unit)(_.joinStd))
          .logWhenMethodTakesLonger

    def changeClusterState(clusterState: HasNodes): Unit =
      this.clusterState = clusterState

    private def sendHeartbeats: IO[Unit] =
      Stream
        .fixedRate[IO](timing.clusterWatchHeartbeat)
        .prependOne(())
        // interruptWhenF before doAHeartbeat otherwise a heartbeat sticking in network congestion
        // would continue independently and arrive out of order (bad).
        .interruptWhenF(stopping.get)
        .evalMap: _ =>
          doAHeartbeat
            .handleErrorWith: t =>
              logger.warn(s"sendHeartbeats: ${t.toStringWithCauses}",
                if t.isInstanceOf[AskTimeoutException] then null else t.nullIfNoStackTrace)
              IO.raiseError(t)
        // Again interruptWhenF() to cancel a sticking doAHeartbeat
        .interruptWhenF(stopping.get)
        .compile.drain
        .as(Completed)

    private def doAHeartbeat: IO[Completed] =
      stopping.tryGet.flatMap:
        case Some(()) => IO.completed
        case None =>
          val clusterState = this.clusterState
          //logger.trace(s"Heartbeat ($nr) $clusterState")
          IO
            .race(
              stopping.get,
              doACheckedHeartbeat(
                clusterState, registerClusterWatchId, clusterWatchIdChangeAllowed = true))
            .flatMap:
              case Left(()) => IO:
                logger.trace("◼️ doACheckedHeartbeat canceled due to `stopping`")
                Completed

              case Right(Left(problem @ ClusterModuleShuttingDownProblem)) =>
                IO:
                  logger.debug(s"⚠️  doACheckedHeartbeat => $problem")
                  Completed

              case Right(Left(problem)) =>
                stopping.tryGet.map:
                  case Some(()) => Completed
                  case None =>
                    haltJava(s"🔥 HALT because ClusterWatch heartbeat failed: $problem",
                      restart = true)

              case Right(Right(_)) =>
                IO.pure(Completed)

  private def doACheckedHeartbeat(
    clusterState: HasNodes,
    registerClusterWatchId: RegisterClusterWatchId,
    clusterWatchIdChangeAllowed: Boolean,
    alreadyLocked: Boolean = false)
  : IO[Checked[Option[ClusterWatchConfirmation]]] =
    clusterWatch
      .checkClusterState(
        clusterState,
        clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed)
      .catchIntoChecked
      .flatMapT:
        case None =>
          IO.right(None)

        case Some(confirmation: ClusterWatchConfirmation) =>
          if clusterState.setting.clusterWatchId contains confirmation.clusterWatchId then
            IO.right(Some(confirmation))
          else if clusterWatchIdChangeAllowed then
            IO(logger.trace:
              s"doACheckedHeartbeat: registerClusterWatchId(alreadyLocked=$alreadyLocked)") *>
            registerClusterWatchId(confirmation, alreadyLocked)
              .rightAs(Some(confirmation))
          else
          // Not expected
            IO.left(Problem(s"New ${confirmation.clusterWatchId} cannot be registered now"))


object ClusterWatchSynchronizer:
  private val logger = Logger[this.type]
  private val heartbeatSessionNr = Iterator.from(1)

  private type RegisterClusterWatchId = (ClusterWatchConfirmation, Boolean) => IO[Checked[Unit]]
