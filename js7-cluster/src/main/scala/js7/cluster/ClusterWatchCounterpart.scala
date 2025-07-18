package js7.cluster

import cats.effect.kernel.DeferredSource
import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO, Outcome, ResourceIO}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.SyncDeadline
import js7.base.eventbus.EventPublisher
import js7.base.fs2utils.Fs2PubSub
import js7.base.log.LogLevel.Debug
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.problem.Checked
import js7.base.service.Service
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.:=
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.base.utils.{AsyncLock, Atomic}
import js7.cluster.ClusterWatchCounterpart.*
import js7.cluster.watch.api.ClusterWatchConfirmation
import js7.data.Problems.ClusterModuleShuttingDownProblem
import js7.data.cluster.ClusterEvent.{ClusterCouplingPrepared, ClusterNodesAppointed, ClusterPassiveLost}
import js7.data.cluster.ClusterState.{Coupled, FailedOver, HasNodes, PassiveLost}
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeLossNotConfirmedProblem, ClusterWatchIdDoesNotMatchProblem, ClusterWatchRequestDoesNotMatchProblem, NoClusterWatchProblem, OtherClusterWatchStillAliveProblem}
import js7.data.cluster.ClusterWatchRequest.RequestId
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterEvent, ClusterTiming, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchId, ClusterWatchRequest}
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.math.Ordering.Implicits.*
import scala.util.Random

final class ClusterWatchCounterpart private(
  pubsub: Fs2PubSub[IO, ClusterWatchRequest],
  clusterConf: ClusterConf,
  timing: ClusterTiming,
  shuttingDown: DeferredSource[IO, Unit],
  testEventPublisher: EventPublisher[Any])
  (implicit ioRuntime: IORuntime)
extends Service.StoppableByRequest:

  import clusterConf.ownId

  private val nextRequestId = Atomic(if isTest then 1 else
    Random.nextLong(Long.MaxValue - (3 * 32_000_000/*a year*/) / timing.heartbeat.toSeconds)
      / 1000_000 * 1000_000)
  private val lock = AsyncLock()
  private val _requested = Atomic(None: Option[Requested])

  private val clusterWatchUniquenessChecker = new ClusterWatchUniquenessChecker(
    clusterConf.clusterWatchUniquenessMemorySize)
  @volatile private var currentClusterWatchId: Option[CurrentClusterWatchId] = None

  protected def start =
    startService:
      untilStopRequested

  def checkClusterState(clusterState: HasNodes, clusterWatchIdChangeAllowed: Boolean)
  : IO[Checked[Option[ClusterWatchConfirmation]]] =
    if !clusterState.setting.clusterWatchId.isDefined
      && !clusterWatchIdChangeAllowed
      && !clusterState.isInstanceOf[Coupled]
      && !clusterState.isInstanceOf[PassiveLost]
      && !clusterState.isInstanceOf[FailedOver]
    then
      IO.right(None)
    else
      initializeCurrentClusterWatchId(clusterState)
        .flatMap: _ =>
          CorrelId.use: correlId =>
            check(
              clusterState.setting.clusterWatchId,
              ClusterWatchCheckState(_, correlId, ownId, clusterState),
              clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed,
              isHeartbeat = true,
            ).map(_.map(Some(_)))

  private def initializeCurrentClusterWatchId(clusterState: HasNodes): IO[Unit] =
    SyncDeadline.usingNow: now ?=>
      if currentClusterWatchId.isEmpty then
        for clusterWatchId <- clusterState.setting.clusterWatchId do
          // Set expiration time on start to inhibit change of registered ClusterWatchId when
          // another ClusterWatch tries to confirm, too.
          logger.trace(s"initializeCurrentClusterWatchId $clusterWatchId")
          currentClusterWatchId = Some(CurrentClusterWatchId(clusterWatchId, now))

  def applyEvent(event: ClusterEvent, clusterState: HasNodes,
    clusterWatchIdChangeAllowed: Boolean = false,
    forceWhenUntaught: Boolean = false)
  : IO[Checked[Option[ClusterWatchConfirmation]]] =
    CorrelId.use: correlId =>
      event match
        case _: ClusterNodesAppointed | _: ClusterCouplingPrepared
          if !clusterState.setting.clusterWatchId.isDefined =>
          IO.right(None)

        case _ =>
          check(
            clusterState.setting.clusterWatchId,
            ClusterWatchCheckEvent(_, correlId, ownId, event, clusterState,
              forceWhenUntaught = forceWhenUntaught),
            clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed
          ).map(_.map(Some(_)))

  private def check(
    clusterWatchId: Option[ClusterWatchId],
    toRequest: RequestId => ClusterWatchRequest,
    clusterWatchIdChangeAllowed: Boolean,
    isHeartbeat: Boolean = false)
  : IO[Checked[ClusterWatchConfirmation]] =
    if !clusterWatchIdChangeAllowed && !clusterWatchId.isDefined then
      logger.debug(s"Due to !clusterWatchIdChangeAllowed: $NoClusterWatchProblem")
      IO.left(NoClusterWatchProblem)
    else
      IO.defer:
        val reqId = RequestId(nextRequestId.getAndIncrement())
        val request = toRequest(reqId)
        lock.lock:
          logger.traceIOWithResult("check",
            s"$request${!clusterWatchIdChangeAllowed ?? ",clusterWatchIdChangeAllowed=false"}",
            // macOS `less` displays 🩶 with wrong width
            result = (a: Checked[ClusterWatchConfirmation]) =>
              if isHeartbeat && a.isRight then s"💚 $a" else a,
            marker = if isHeartbeat then Logger.Heartbeat else null,
            body =
              check2(
                clusterWatchId, request,
                new Requested(clusterWatchId, request,
                  clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed)))

  private def check2(
    clusterWatchId: Option[ClusterWatchId],
    request: ClusterWatchRequest,
    requested: Requested)
  : IO[Checked[ClusterWatchConfirmation]] =
    SyncDeadline.now.flatMap(since => IO.defer:
      _requested := Some(requested)
      val sym = new BlockingSymbol
      pubsub.publish(request)
        .logWhenItTakesLonger(s"ClusterWatch.send($request)")
        .*>(IO(
          testEventPublisher.publish(TestWaitingForConfirmation(request))))
        .*>(requested
          .untilConfirmed
          .timeoutAndFail(timing.clusterWatchReactionTimeout)(new RequestTimeoutException))
        .onErrorRestartLoop(()):
          case (_: RequestTimeoutException, _, retry) =>
            shuttingDown.tryGet.flatMap:
              case Some(()) => IO.left(ClusterModuleShuttingDownProblem)
              case None =>
                SyncDeadline.usingNow:
                  sym.onWarn()
                  logger.warn(s"$sym Still trying to get a confirmation from " +
                    clusterWatchId.fold("any ClusterWatch")(id =>
                      id.toString + (requested.clusterWatchIdChangeAllowed ?? " (or other)")) +
                    s" for ${request.toShortString} for ${since.elapsed.pretty}...")
                .*>(retry(()))

          case (t, _, _) => IO.raiseError(t)
        .flatTap:
          case Left(problem @ ClusterModuleShuttingDownProblem) =>
            IO(logger.log(sym.relievedLogLevel min Debug,
              s"⚠️  ${request.toShortString} => $problem · after ${since.elapsed.pretty}"))

          case Left(problem) =>
            IO(logger.warn(s"⛔ ClusterWatch rejected ${request.toShortString}: $problem"))

          case Right(confirmation) =>
            testEventPublisher.publish(TestConfirmed(request, confirmation))
            SyncDeadline.usingNow:
              logger.log(sym.relievedLogLevel,
                s"🟢 ${confirmation.clusterWatchId} finally confirmed ${
                  request.toShortString} after ${since.elapsed.pretty}")
        .guaranteeCase:
          case Outcome.Errored(t) if sym.warnLogged => SyncDeadline.usingNow:
            logger.warn:
              s"💥 ${request.toShortString} => ${t.toStringWithCauses} · after ${since.elapsed.pretty}"

          case Outcome.Canceled() if sym.warnLogged => SyncDeadline.usingNow:
            logger.info:
              s"◼️ ${request.toShortString} => Canceled after ${since.elapsed.pretty}"

          case _ => IO.unit
        .guarantee(IO:
          _requested := None))

  def executeClusterWatchConfirm(confirm: ClusterWatchConfirm): IO[Checked[Unit]] =
    IO(clusterWatchUniquenessChecker.check(confirm.clusterWatchId, confirm.clusterWatchRunId))
      .flatMapT(_ => IO(takeRequest(confirm)))
      .flatMapT: requested =>
        val confirmation = toConfirmation(confirm)
        (requested.request.maybeEvent, confirmation) match
          case (Some(_: ClusterPassiveLost), Left(problem))
            if problem is ClusterNodeLossNotConfirmedProblem =>
            // Ignore this, because ActiveClusterNode cannot handle this.
            // Continue to wait until user has confirmed the ClusterPassiveLost via ClusterWatch.

            // Possible improvement: ActiveClusterNode should continue trying to get acknowledges
            // from the lost passive node. If it succeeds, ActiveClusterNode becomes senseless.

            // Keep _requested
            _requested.compareAndSet(None, Some(requested))
            logger.warn(problem.toString)
            IO.right(())

          case _ =>
            for confirmer <- confirm.manualConfirmer do
              logger.info(s"‼️ ${requested.request.maybeEvent.fold("?")(_.getClass.simpleScalaName)
                } has MANUALLY BEEN CONFIRMED by '$confirmer' ‼️")
            requested.confirm(confirmation)
      .flatMapT: _ =>
        SyncDeadline.usingNow:
          for o <- currentClusterWatchId do o.touch(confirm.clusterWatchId)
          Checked.unit

  private def toConfirmation(confirm: ClusterWatchConfirm): Checked[ClusterWatchConfirmation] =
    confirm.problem.toLeft:
      ClusterWatchConfirmation(
        confirm.requestId,
        confirm.clusterWatchId,
        confirm.clusterWatchRunId)

  // Recursive in case of (wrong) concurrent access to this._requested
  @tailrec private def takeRequest(confirm: ClusterWatchConfirm): Checked[Requested] =
    _requested.get() match
      case None =>
        currentClusterWatchId match
          case Some(o) if o.clusterWatchId != confirm.clusterWatchId =>
            logger.warn(s"❓ ${confirm.clusterWatchId} confirms without a request · currentClusterWatchId=$o => $OtherClusterWatchStillAliveProblem")
            // Try to return the same problem when ClusterWatchId does not match,
            // whether _requested.get() contains a Requested or not.
            // So a second ClusterWatch gets always the same problem.
            Left(OtherClusterWatchStillAliveProblem/*?*/(
              rejectedClusterWatchId = confirm.clusterWatchId,
              requestedClusterWatchId = o.clusterWatchId))

          case _ =>
            logger.warn(s"❓ ${confirm.clusterWatchId} confirms without a request => $OtherClusterWatchStillAliveProblem")
            Left(ClusterWatchRequestDoesNotMatchProblem)

      case value @ Some(requested) =>
        (requested.clusterWatchId, currentClusterWatchId) match
          case (Some(o), Some(currentClusterWatchId))
            if o != confirm.clusterWatchId && currentClusterWatchId.isStillAlive =>
            val problem = OtherClusterWatchStillAliveProblem(
              rejectedClusterWatchId = confirm.clusterWatchId,
              requestedClusterWatchId = o)
            logger.warn(s"${currentClusterWatchId.timeLeft.pretty} left => $problem")
            Left(problem)

          case (Some(o), _) if o != confirm.clusterWatchId
            && !confirm.manualConfirmer.isDefined
            && !requested.clusterWatchIdChangeAllowed =>
            Left(ClusterWatchIdDoesNotMatchProblem(
              rejectedClusterWatchId = confirm.clusterWatchId,
              requestedClusterWatchId = o,
              requested.request))

          case _ =>
            if confirm.requestId != requested.id then
              val problem = ClusterWatchRequestDoesNotMatchProblem
              logger.debug(s"⛔ $problem id=${confirm.requestId} but _requested=${requested.id}")
              Left(problem)
            else if !_requested.compareAndSet(value, None) then
              takeRequest(confirm)
            else
              // Log when ActiveClusterNode will detect and register a changed ClusterWatchId.
              requested.clusterWatchId match
                case None => logger.info(s"${confirm.clusterWatchId} will be registered")
                case Some(o) if confirm.clusterWatchId != o =>
                  logger.info(s"${confirm.clusterWatchId} will replace registered $o")
                case _ =>
              Right(requested)

  def onClusterWatchRegistered(clusterWatchId: ClusterWatchId): IO[Unit] =
    SyncDeadline.usingNow: now ?=>
      logger.trace(s"onClusterWatchRegistered $clusterWatchId")
      currentClusterWatchId = Some(CurrentClusterWatchId(clusterWatchId, now))

  def newStream: fs2.Stream[IO, ClusterWatchRequest] =
    pubsub.newStream // TODO Delete all but the last request at a time. At push-side?

  override def toString = "ClusterWatchCounterpart"

  private sealed case class CurrentClusterWatchId(
    // This field is only to return a proper Problem if no Requested is pending.
    clusterWatchId: ClusterWatchId, initialNow: SyncDeadline.Now):

    private var expires: SyncDeadline =
      initialNow + timing.clusterWatchIdTimeout

    def touch(clusterWatchId: ClusterWatchId)(using now: SyncDeadline.Now): Unit =
      if clusterWatchId == this.clusterWatchId then
        expires = now + timing.clusterWatchIdTimeout

    def isStillAlive(using SyncDeadline.Now): Boolean =
      expires.hasTimeLeft

    def timeLeft(using SyncDeadline.Now): FiniteDuration =
      expires.timeLeft

    override def toString = clusterWatchId.toString


object ClusterWatchCounterpart:
  private val logger = Logger[this.type]

  def resource(
    clusterConf: ClusterConf,
    timing: ClusterTiming,
    shuttingDown: DeferredSource[IO, Unit],
    testEventPublisher: EventPublisher[Any])
    (using IORuntime)
  : ResourceIO[ClusterWatchCounterpart] =
    for
      pubsub <- Fs2PubSub.resource[IO, ClusterWatchRequest]
      service <- Service.resource:
        new ClusterWatchCounterpart(pubsub, clusterConf, timing, shuttingDown, testEventPublisher)
    yield
      service

  /** A request to ClusterWatch yet to be responded. */
  private final class Requested(
    val clusterWatchId: Option[ClusterWatchId],
    val request: ClusterWatchRequest,
    val clusterWatchIdChangeAllowed: Boolean):
    def id = request.requestId
    private val confirmation = Deferred.unsafe[IO, Checked[ClusterWatchConfirmation]]

    def untilConfirmed: IO[Checked[ClusterWatchConfirmation]] =
      confirmation.get

    def confirm(confirm: Checked[ClusterWatchConfirmation]): IO[Checked[Unit]] =
      confirmation.complete(confirm)
        .as(Checked.unit)

    override def toString =
      s"Requested($id,$clusterWatchId,clusterWatchIdChangeAllowed=$clusterWatchIdChangeAllowed)"

  private class RequestTimeoutException extends Exception

  final case class TestWaitingForConfirmation(request: ClusterWatchRequest)

  final case class TestConfirmed(
    request: ClusterWatchRequest,
    confirmation: ClusterWatchConfirmation)
