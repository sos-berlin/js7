package js7.cluster

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCase, Resource}
import cats.syntax.flatMap.*
import cats.syntax.option.*
import js7.base.auth.UserId
import js7.base.eventbus.EventPublisher
import js7.base.fs2utils.Fs2PubSub
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.problem.Checked
import js7.base.service.Service
import js7.base.time.ScalaTime.{DurationRichInt, RichDuration}
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.cluster.ClusterWatchCounterpart.*
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterWatchIdDoesNotMatchProblem, ClusterWatchRequestDoesNotMatchProblem, ConfirmClusterNodeLossNotApplicableProblem, NoClusterWatchProblem, OtherClusterWatchStillAliveProblem}
import js7.cluster.watch.api.{ClusterWatchApi, ClusterWatchConfirmation, ConfirmedByClusterWatch, ConfirmedByUser}
import js7.data.cluster.ClusterEvent.{ClusterCouplingPrepared, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{Coupled, FailedOver, HasNodes, IsNodeLost, PassiveLost}
import js7.data.cluster.ClusterWatchRequest.RequestId
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterEvent, ClusterTiming, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchId, ClusterWatchRequest}
import js7.data.node.NodeId
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
extends Service.StoppableByRequest with ClusterWatchApi
{
  import clusterConf.ownId

  private val nextRequestId = Atomic(if (isTest) 1 else Random.nextLong(
    (Long.MaxValue - (3 * 32_000_000/*a year*/) / timing.heartbeat.toSeconds) / 1000 * 1000))
  private val lock = AsyncLock()
  private val _requested = Atomic(None: Option[Requested])
  private val pubsub = new Fs2PubSub[Task, ClusterWatchRequest]

  private val clusterWatchUniquenessChecker = new ClusterWatchUniquenessChecker(
    clusterConf.clusterWatchUniquenessMemorySize)
  @volatile private var currentClusterWatchId: Option[CurrentClusterWatchId] = None
  @volatile private var userConfirmedNodeLoss = none[UserConfirmedNodeLoss]

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
        check(
          clusterState.setting.clusterWatchId,
          ClusterWatchCheckState(_, CorrelId.current, ownId, clusterState),
          clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed
        ).map(_.map(Some(_)))

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
    Task.defer {
      userConfirmedNodeLoss = None
      event match {
        case _: ClusterNodesAppointed | _: ClusterCouplingPrepared
          if !clusterState.setting.clusterWatchId.isDefined =>
          Task.right(None)

        case _ =>
          check(
            clusterState.setting.clusterWatchId,
            ClusterWatchCheckEvent(_, CorrelId.current, ownId, event, clusterState),
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
          logger.debugTaskWithResult[Checked[ClusterWatchConfirmation]]("check",
            s"$request${!clusterWatchIdChangeAllowed ?? "clusterWatchIdChangeAllowed=false"}"
          )(Task.defer {
            userConfirmedNodeLoss
              .flatMap(_
                .nodeLostStateToConfirm(request))
              .map(Task.right(_))
              .getOrElse {
                userConfirmedNodeLoss = None

                val requested = new Requested(clusterWatchId, request,
                  clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed)
                check2(clusterWatchId, request, requested)
              }
          }))
      }

  private def check2(
    clusterWatchId: Option[ClusterWatchId],
    request: ClusterWatchRequest,
    requested: Requested)
  : Task[Checked[ClusterWatchConfirmation]] =
    Task.defer {
      _requested.set(Some(requested))
      val t = now
      var warned = false
      send(request)
        .*>(Task(
          testEventPublisher.publish(WaitingForConfirmation(request))))
        .*>(requested
          .untilConfirmed
          .timeoutTo(
            timing.clusterWatchReactionTimeout,
            Task.raiseError(RequestTimeoutException)))
        .onErrorRestartLoop(()) {
          case (RequestTimeoutException, _, retry) =>
            warned = true
            logger.warn(
              "⭕ Still trying to get a confirmation from " +
                clusterWatchId.fold("any ClusterWatch")(id =>
                  id.toString + (requested.clusterWatchIdChangeAllowed ?? " (or other)")) +
                " for " + request.toShortString + "" +
                " for " + t.elapsed.pretty + "...")
            retry(()).delayExecution(1.s)

          case (t, _, _) => Task.raiseError(t)
        }
        .flatTap {
          case Left(problem) =>
            Task(logger.warn(s"ClusterWatch rejected ${request.toShortString}: $problem"))

          case Right(confirmation) =>
            Task {
              confirmation.problem match {
                case Some(problem) =>
                  // Just in case, the caller does not warn ???
                  logger.warn(s"$clusterWatchId rejected ${request.toShortString}: $problem")
                case None =>
                  if (warned) logger.info(
                    s"🟢 $clusterWatchId finally confirmed ${
                      request.toShortString} after ${t.elapsed.pretty}")
              }
            }
        }
        .guaranteeCase(exitCase => Task {
          _requested.set(None)
          if (warned && exitCase != ExitCase.Completed) logger.warn(
            s"${request.toShortString} => $exitCase · after ${t.elapsed.pretty}")
        })
    }

  def executeClusterWatchConfirm(confirm: ClusterWatchConfirm): Task[Checked[Unit]] =
    Task(clusterWatchUniquenessChecker.check(confirm.clusterWatchId, confirm.clusterWatchRunId))
      .flatMapT(_ => Task(takeRequest(confirm)))
      .flatMapT(_.confirm(toConfirmation(confirm)))
      .flatMapT(_ => Task {
        for (o <- currentClusterWatchId) o.touched(confirm.clusterWatchId)
        Checked.unit
      })

  private def toConfirmation(confirm: ClusterWatchConfirm): ConfirmedByClusterWatch =
    ConfirmedByClusterWatch(
      confirm.requestId,
      confirm.clusterWatchId,
      confirm.clusterWatchRunId,
      confirm.problem)

  // Recursive in (wrong) case of concurrent access to this._requested
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
            Left(ClusterWatchRequestDoesNotMatchProblem)
        }

      case value @ Some(requested) =>
        requested.clusterWatchId match {
          case Some(o) if o != confirm.clusterWatchId
            && currentClusterWatchId.exists(_.isStillAlive) =>
            Left(OtherClusterWatchStillAliveProblem(
              rejectedClusterWatchId = confirm.clusterWatchId,
              requestedClusterWatchId = o))

          case Some(o) if o != confirm.clusterWatchId && !requested.clusterWatchIdChangeAllowed =>
            Left(ClusterWatchIdDoesNotMatchProblem(
              rejectedClusterWatchId = confirm.clusterWatchId,
              requestedClusterWatchId = o))

          case _ =>
            if (confirm.requestId != requested.id) {
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

  def userConfirmNodeLoss(lostNodeId: NodeId, userId: UserId): Task[Checked[Unit]] =
    logger.traceTask("userConfirmNodeLoss", lostNodeId)(
      Task.defer {
        val confirmedLoss = UserConfirmedNodeLoss(
          lostNodeId,
          expires = now + timing.clusterWatchReactionTimeout + 1.s,
          userId)

        def reject = Task.left(ConfirmClusterNodeLossNotApplicableProblem)

        @tailrec def confirmRequest(): Task[Checked[Unit]] =
          _requested.get() match {
            case None => reject
            case some @ Some(requested) =>
              confirmedLoss.nodeLostEventToConfirm(requested) match {
                case None => reject
                case Some(confirm) =>
                  if (_requested.compareAndSet(some, None)) {
                    userConfirmedNodeLoss = Some(confirmedLoss)
                    requested.confirm(confirm)
                  } else
                    confirmRequest()
              }
            case _ => reject
          }

        confirmRequest()
      })

  def onClusterWatchRegistered(clusterWatchId: ClusterWatchId): Task[Unit] =
    Task {
      currentClusterWatchId = Some(CurrentClusterWatchId(clusterWatchId))
    }

  private def send(request: ClusterWatchRequest): Task[Unit] =
    pubsub.publish(request)
      .logWhenItTakesLonger(s"ClusterWatch.send($request)")

  def newStream: Task[fs2.Stream[Task, ClusterWatchRequest]] =
    pubsub.newStream // TODO Delete all but the last request at a time. At push-side?

  override def toString = "ClusterWatchCounterpart"

  private sealed case class CurrentClusterWatchId(
    // This field is only to return a proper Problem if no Requested is pending.
    clusterWatchId: ClusterWatchId)
  {
    private var expires: MonixDeadline =
      now + timing.clusterWatchIdTimeout

    def touched(clusterWatchId: ClusterWatchId): Unit =
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

    def confirm(confirm: ClusterWatchConfirmation): Task[Checked[Unit]] =
      confirmation.complete(confirm.problem.toLeft(confirm))
        .materialize/*Ignore duplicate complete*/.as(Checked.unit)

    override def toString = s"Requested($id,$clusterWatchId)"
  }

  private case class UserConfirmedNodeLoss(
    lostNodeId: NodeId,
    expires: MonixDeadline,
    userId: UserId)
  {
    def nodeLostEventToConfirm(requested: Requested): Option[ConfirmedByUser] =
      requested.request.isNodeLostEvent(lostNodeId) ?
        toConfirmation(requested.id)

    def nodeLostStateToConfirm(request: ClusterWatchRequest): Option[ConfirmedByUser] =
      request match {
        case ClusterWatchCheckState(_, _, _, clusterState: IsNodeLost)
          if clusterState.passiveId == lostNodeId && expires.hasTimeLeft =>
          //? requested.confirm(toConfirmation(requested.id))
          Some(toConfirmation(request.requestId))
        case _ =>
          logger.trace(s"🚫 $toString does not match $request")
          None
      }

    // A fake ClusterWatchConfirm used for node loss confirmation by command
    private def toConfirmation(requestId: RequestId) =
      ConfirmedByUser(requestId, userId)

    override def toString = s"UserConfirmedNodeLoss($userId $lostNodeId expires=$expires)"
  }

  private object RequestTimeoutException extends Exception

  final case class WaitingForConfirmation(request: ClusterWatchRequest)
}
