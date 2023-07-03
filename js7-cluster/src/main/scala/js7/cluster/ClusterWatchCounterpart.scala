package js7.cluster

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCase, Resource}
import cats.syntax.flatMap.*
import js7.base.eventbus.EventPublisher
import js7.base.fs2utils.Fs2PubSub
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.problem.Checked
import js7.base.service.Service
import js7.base.time.ScalaTime.RichDuration
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.cluster.ClusterWatchCounterpart.*
import js7.cluster.watch.api.ClusterWatchConfirmation
import js7.data.cluster.ClusterEvent.{ClusterCouplingPrepared, ClusterNodesAppointed, ClusterPassiveLost, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{Coupled, FailedOver, HasNodes, PassiveLost}
import js7.data.cluster.ClusterWatchProblems.{ClusterNodeLossNotConfirmedProblem, ClusterWatchIdDoesNotMatchProblem, ClusterWatchRequestDoesNotMatchProblem, NoClusterWatchProblem, OtherClusterWatchStillAliveProblem}
import js7.data.cluster.ClusterWatchRequest.RequestId
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterEvent, ClusterTiming, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchId, ClusterWatchRequest}
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import scala.annotation.tailrec
import scala.util.Random

final class ClusterWatchCounterpart private(
  clusterConf: ClusterConf,
  timing: ClusterTiming,
  testEventPublisher: EventPublisher[Any])
  (implicit scheduler: Scheduler)
extends Service.StoppableByRequest
{
  import clusterConf.ownId

  private val nextRequestId = Atomic(if (isTest) 1 else
    Random.nextLong((Long.MaxValue - (3 * 32_000_000/*a year*/) / timing.heartbeat.toSeconds))
      / 1000_000 * 1000_000)
  private val lock = AsyncLock()
  private val _requested = Atomic(None: Option[Requested])
  private val pubsub = new Fs2PubSub[Task, ClusterWatchRequest]

  private val clusterWatchUniquenessChecker = new ClusterWatchUniquenessChecker(
    clusterConf.clusterWatchUniquenessMemorySize)
  @volatile private var currentClusterWatchId: Option[CurrentClusterWatchId] = None

  protected def start =
    startService(
      untilStopRequested *> pubsub.complete)

  def checkClusterState(clusterState: HasNodes, clusterWatchIdChangeAllowed: Boolean)
  : Task[Checked[Option[ClusterWatchConfirmation]]] =
    if (!clusterState.setting.clusterWatchId.isDefined
      && !clusterWatchIdChangeAllowed
      && !clusterState.isInstanceOf[Coupled]
      && !clusterState.isInstanceOf[PassiveLost]
      && !clusterState.isInstanceOf[FailedOver])
      Task.right(None)
    else
      initializeCurrentClusterWatchId(clusterState) *>
        CorrelId.use(correlId =>
          check(
            clusterState.setting.clusterWatchId,
            ClusterWatchCheckState(_, correlId, ownId, clusterState),
            clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed
          ).map(_.map(Some(_))))

  private def initializeCurrentClusterWatchId(clusterState: HasNodes): Task[Unit] =
    Task {
      if (currentClusterWatchId.isEmpty) {
        for (clusterWatchId <- clusterState.setting.clusterWatchId) {
          // Set expiration time on start to inhibit change of registered ClusterWatchId when
          // another ClusterWatch tries to confirm, too.
          currentClusterWatchId = Some(CurrentClusterWatchId(clusterWatchId))
        }
      }
    }

  def applyEvent(event: ClusterEvent, clusterState: HasNodes)
  : Task[Checked[Option[ClusterWatchConfirmation]]] =
    CorrelId.use { correlId =>
      event match {
        case _: ClusterNodesAppointed | _: ClusterCouplingPrepared
          if !clusterState.setting.clusterWatchId.isDefined =>
          Task.right(None)

        case _ =>
          check(
            clusterState.setting.clusterWatchId,
            ClusterWatchCheckEvent(_, correlId, ownId, event, clusterState),
            clusterWatchIdChangeAllowed = event.isInstanceOf[ClusterWatchRegistered]
          ).map(_.map(Some(_)))
      }
    }

  private def check(
    clusterWatchId: Option[ClusterWatchId],
    toRequest: RequestId => ClusterWatchRequest,
    clusterWatchIdChangeAllowed: Boolean)
  : Task[Checked[ClusterWatchConfirmation]] =
    if (!clusterWatchIdChangeAllowed && !clusterWatchId.isDefined)
      Task.left(NoClusterWatchProblem)
    else
      Task.defer {
        val reqId = RequestId(nextRequestId.getAndIncrement())
        val request = toRequest(reqId)
        lock.lock(
          logger.traceTaskWithResult("check",
            s"$request${!clusterWatchIdChangeAllowed ?? ",clusterWatchIdChangeAllowed=false"}",
            task = check2(
              clusterWatchId, request,
              new Requested(clusterWatchId, request,
                clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed))))
      }

  private def check2(
    clusterWatchId: Option[ClusterWatchId],
    request: ClusterWatchRequest,
    requested: Requested)
  : Task[Checked[ClusterWatchConfirmation]] =
    Task.defer {
      _requested.set(Some(requested))
      val since = now
      val sym = new BlockingSymbol
      pubsub.publish(request)
        .logWhenItTakesLonger(s"ClusterWatch.send($request)")
        .*>(Task(
          testEventPublisher.publish(WaitingForConfirmation(request))))
        .*>(requested
          .untilConfirmed
          .timeoutTo(
            timing.clusterWatchReactionTimeout,
            Task.raiseError(RequestTimeoutException)))
        .onErrorRestartLoop(()) {
          case (RequestTimeoutException, _, retry) =>
            sym.onWarn()
            logger.warn(sym.toString +
              " Still trying to get a confirmation from " +
              clusterWatchId.fold("any ClusterWatch")(id =>
                id.toString + (requested.clusterWatchIdChangeAllowed ?? " (or other)")) +
              " for " + request.toShortString + "" +
              " for " + since.elapsed.pretty + "...")
            retry(())

          case (t, _, _) => Task.raiseError(t)
        }
        .flatTap {
          case Left(problem) =>
            Task(logger.warn(s"⛔ ClusterWatch rejected ${request.toShortString}: $problem"))

          case Right(confirmation) =>
            Task {
              if (sym.warnLogged) logger.info(
                s"🟢 ${confirmation.clusterWatchId} finally confirmed ${
                  request.toShortString} after ${since.elapsed.pretty}")
            }
        }
        .guaranteeCase {
          case ExitCase.Error(t) if sym.warnLogged => Task {
            logger.warn(
              s"💥 ${request.toShortString} => ${t.toStringWithCauses} · after ${since.elapsed.pretty}")
          }
          case ExitCase.Canceled if sym.warnLogged => Task {
            logger.info(
              s"⚫ ${request.toShortString} => Canceled after ${since.elapsed.pretty}")
          }
          case _ => Task.unit
        }
        .guarantee(Task(
          _requested.set(None)))
    }

  def executeClusterWatchConfirm(confirm: ClusterWatchConfirm): Task[Checked[Unit]] =
    Task(clusterWatchUniquenessChecker.check(confirm.clusterWatchId, confirm.clusterWatchRunId))
      .flatMapT(_ => Task(takeRequest(confirm)))
      .flatMapT { requested =>
        val confirmation = toConfirmation(confirm)
        (requested.request.maybeEvent, confirmation) match {
          case (Some(_: ClusterPassiveLost), Left(problem))
            if problem is ClusterNodeLossNotConfirmedProblem =>
            // Ignore this, because ActiveClusterNode cannot handle this.
            // Continue to wait until user has confirmed the ClusterPassiveLost via ClusterWatch.

            // Possible improvement: ActiveClusterNode should continue trying to get acknowledges
            // from the lost passive node. If it succeeds, ActiveClusterNode becomes senseless.

            // Keep _requested
            _requested.compareAndSet(None, Some(requested))
            logger.warn(problem.toString)
            Task.right(())

          case _ =>
            for (confirmer <- confirm.manualConfirmer) {
              logger.info(s"‼️ ${requested.request.maybeEvent.fold("?")(_.getClass.simpleScalaName)
                } has MANUALLY BEEN CONFIRMED by '$confirmer' ‼️")
            }
            requested.confirm(confirmation)
        }
      }
      .flatMapT(_ => Task {
        for (o <- currentClusterWatchId) o.touch(confirm.clusterWatchId)
        Checked.unit
      })

  private def toConfirmation(confirm: ClusterWatchConfirm): Checked[ClusterWatchConfirmation] =
    confirm.problem.toLeft(
      ClusterWatchConfirmation(
        confirm.requestId,
        confirm.clusterWatchId,
        confirm.clusterWatchRunId))

  // Recursive in case of (wrong) concurrent access to this._requested
  @tailrec private def takeRequest(confirm: ClusterWatchConfirm): Checked[Requested] = {
    _requested.get() match {
      case None =>
        currentClusterWatchId match {
          case Some(o) if o.clusterWatchId != confirm.clusterWatchId =>
            // Try to return the same problem when ClusterWatchId does not match,
            // whether _requested.get() contains a Requested or not.
            // So a second ClusterWatch gets always the same problem.
            Left(OtherClusterWatchStillAliveProblem/*?*/(
              rejectedClusterWatchId = confirm.clusterWatchId,
              requestedClusterWatchId = o.clusterWatchId))

          case _ =>
            logger.debug(s"❓ ${confirm.clusterWatchId} confirms, but no request is present")
            Left(ClusterWatchRequestDoesNotMatchProblem)
        }

      case value @ Some(requested) =>
        requested.clusterWatchId match {
          case Some(o) if o != confirm.clusterWatchId
            && currentClusterWatchId.exists(_.isStillAlive) =>
            Left(OtherClusterWatchStillAliveProblem(
              rejectedClusterWatchId = confirm.clusterWatchId,
              requestedClusterWatchId = o))

          case Some(o) if o != confirm.clusterWatchId
            && !confirm.manualConfirmer.isDefined
            && !requested.clusterWatchIdChangeAllowed =>
            Left(ClusterWatchIdDoesNotMatchProblem(
              rejectedClusterWatchId = confirm.clusterWatchId,
              requestedClusterWatchId = o))

          case _ =>
            if (confirm.requestId != requested.id) {
              logger.debug(s"⛔ confirm.requestId=${confirm.requestId}, but requested=${requested.id}")
              val problem = ClusterWatchRequestDoesNotMatchProblem
              logger.debug(s"$problem id=${confirm.requestId} but _requested=${requested.id}")
              Left(problem)
            } else if (!_requested.compareAndSet(value, None))
              takeRequest(confirm)
            else {
              // Log when ActiveClusterNode will detect and register a changed ClusterWatchId.
              requested.clusterWatchId match {
                case None => logger.info(s"${confirm.clusterWatchId} will be registered")
                case Some(o) if confirm.clusterWatchId != o =>
                  logger.info(s"${confirm.clusterWatchId} will replace registered $o")
                case _ =>
              }
              Right(requested)
            }
        }
    }
  }

  def onClusterWatchRegistered(clusterWatchId: ClusterWatchId): Task[Unit] =
    Task {
      currentClusterWatchId = Some(CurrentClusterWatchId(clusterWatchId))
    }

  def newStream: Task[fs2.Stream[Task, ClusterWatchRequest]] =
    pubsub.newStream // TODO Delete all but the last request at a time. At push-side?

  override def toString = "ClusterWatchCounterpart"

  private sealed case class CurrentClusterWatchId(
    // This field is only to return a proper Problem if no Requested is pending.
    clusterWatchId: ClusterWatchId)
  {
    private var expires: MonixDeadline =
      now + timing.clusterWatchIdTimeout

    def touch(clusterWatchId: ClusterWatchId): Unit =
      if (clusterWatchId == this.clusterWatchId) {
        expires = now + timing.clusterWatchIdTimeout
      }

    def isStillAlive: Boolean =
      expires.hasTimeLeft

    override def toString = s"$clusterWatchId($expires)"
  }
}

object ClusterWatchCounterpart
{
  private val logger = Logger[this.type]

  def resource(
    clusterConf: ClusterConf,
    timing: ClusterTiming,
    testEventPublisher: EventPublisher[Any])
  : Resource[Task, ClusterWatchCounterpart] =
    Service.resource(Task.deferAction(scheduler => Task(
      new ClusterWatchCounterpart(clusterConf, timing, testEventPublisher)(scheduler))))

  /** A request to ClusterWatch yet to be responded. */
  private final class Requested(
    val clusterWatchId: Option[ClusterWatchId],
    val request: ClusterWatchRequest,
    val clusterWatchIdChangeAllowed: Boolean)
  {
    def id = request.requestId
    private val confirmation = Deferred.unsafe[Task, Checked[ClusterWatchConfirmation]]

    def untilConfirmed: Task[Checked[ClusterWatchConfirmation]] =
      confirmation.get

    def confirm(confirm: Checked[ClusterWatchConfirmation]): Task[Checked[Unit]] =
      confirmation.complete(confirm)
        .materialize/*Ignore duplicate complete*/.as(Checked.unit)

    override def toString = s"Requested($id,$clusterWatchId)"
  }

  private object RequestTimeoutException extends Exception

  final case class WaitingForConfirmation(request: ClusterWatchRequest)
}
