package js7.cluster

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCase, Resource}
import cats.syntax.flatMap.*
import js7.base.fs2utils.Fs2PubSub
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked
import js7.base.service.Service
import js7.base.time.ScalaTime.{DurationRichInt, RichDeadline, RichDuration}
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.cluster.ClusterWatchCounterpart.*
import js7.cluster.watch.api.AnyClusterWatch
import js7.cluster.watch.api.ClusterWatchProblems.{ClusterWatchIdDoesNotMatchProblem, ClusterWatchRequestDoesNotMatchProblem, NoClusterWatchProblem, OtherClusterWatchStillAliveProblem}
import js7.data.cluster.ClusterEvent.{ClusterCouplingPrepared, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.cluster.ClusterState.{Coupled, FailedOver, HasNodes, PassiveLost}
import js7.data.cluster.ClusterWatchMessage.RequestId
import js7.data.cluster.ClusterWatchingCommand.ClusterWatchConfirm
import js7.data.cluster.{ClusterEvent, ClusterTiming, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchId, ClusterWatchMessage, ClusterWatchRequest}
import monix.eval.Task
import monix.execution.atomic.Atomic
import scala.annotation.tailrec
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Deadline.now
import scala.util.Random

final class ClusterWatchCounterpart private(
  clusterConf: ClusterConf,
  timing: ClusterTiming)
extends Service.StoppableByRequest with AnyClusterWatch
{
  import clusterConf.ownId

  private val nextRequestId = Atomic(if (isTest) 1 else Random.nextLong() * 1000)
  private val lock = AsyncLock()
  private val request = Atomic(None: Option[Requested])
  private val pubsub = new Fs2PubSub[Task, ClusterWatchMessage]

  private val clusterWatchUniquenessChecker = new ClusterWatchUniquenessChecker(
    clusterConf.clusterWatchUniquenessMemorySize)
  @volatile private var clusterWatchIdExpires: Option[Deadline] = None

  protected def start =
    startService(
      untilStopRequested *> pubsub.complete)

  def tryLogout = Task.pure(Completed)

  def checkClusterState(
    clusterState: HasNodes,
    clusterWatchIdChangeAllowed: Boolean)
  : Task[Checked[Option[ClusterWatchConfirm]]] =
    if (!clusterState.setting.clusterWatchId.isDefined
      && !clusterWatchIdChangeAllowed
      && !clusterState.isInstanceOf[Coupled]
      && !clusterState.isInstanceOf[PassiveLost]
      && !clusterState.isInstanceOf[FailedOver])
      Task.right(None)
    else
      check(
        clusterState.setting.clusterWatchId,
        ClusterWatchCheckState(_, CorrelId.current, ownId, clusterState),
        clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed
      ).map(_.map(Some(_)))

  def applyEvent(event: ClusterEvent, clusterState: HasNodes): Task[Checked[Completed]] =
    event match {
      case _: ClusterNodesAppointed | _: ClusterCouplingPrepared
        if !clusterState.setting.clusterWatchId.isDefined =>
        Task.right(Completed)

      case _ =>
        check(
          clusterState.setting.clusterWatchId,
          ClusterWatchCheckEvent(_, CorrelId.current, ownId, event, clusterState),
          clusterWatchIdChangeAllowed = event.isInstanceOf[ClusterWatchRegistered]
        ).rightAs(Completed)
    }

  private def check(
    clusterWatchId: Option[ClusterWatchId],
    toMessage: RequestId => ClusterWatchRequest,
    clusterWatchIdChangeAllowed: Boolean = false)
  : Task[Checked[ClusterWatchConfirm]] =
    if (!clusterWatchIdChangeAllowed && !clusterWatchId.isDefined)
      Task.left(NoClusterWatchProblem)
    else {
      Task.defer {
        val reqId = RequestId(nextRequestId.getAndIncrement())
        val msg = toMessage(reqId)
        lock.lock(
          logger.debugTask("check", msg)(Task.defer {
            val req = new Requested(clusterWatchId, reqId,
              clusterWatchIdChangeAllowed = clusterWatchIdChangeAllowed)
            request.set(Some(req))
            val t = now
            var warned = false
            send(msg)
              .*>(req.confirmation
                .get
                .timeoutTo(
                  timing.clusterWatchReactionTimeout,
                  Task.raiseError(RequestTimeoutException)))
              .onErrorRestartLoop(()) {
                case (RequestTimeoutException, _, retry) =>
                  warned = true
                  logger.warn(
                    s"⭕ Still trying to get a confirmation from ${
                      clusterWatchId getOrElse "unknown ClusterWatch"} for ${
                      msg.toShortString} for ${t.elapsed.pretty}...")
                  retry(()).delayExecution(1.s)

                case (t, _, _) => Task.raiseError(t)
              }
              .flatTap {
                case Left(problem) =>
                  Task(logger.warn(s"ClusterWatch rejected ${msg.toShortString}: $problem"))

                case Right(ClusterWatchConfirm(_, clusterWatchId, _, Some(problem))) =>
                  // Just in case, the caller does not warn ???
                  Task(logger.warn(s"$clusterWatchId rejected ${msg.toShortString}: $problem"))

                case Right(ClusterWatchConfirm(_, clusterWatchId, _, None)) =>
                  Task {
                    if (warned) logger.info(
                      s"🟢 $clusterWatchId finally confirmed ${
                        msg.toShortString} after ${t.elapsed.pretty}")
                  }
              }
              .guaranteeCase(exitCase => Task {
                request.set(None)
                if (warned && exitCase != ExitCase.Completed) logger.warn(
                  s"${msg.toShortString} => $exitCase · after ${t.elapsed.pretty}")
              })
          }))
      }
    }

  def executeClusterWatchConfirm(confirm: ClusterWatchConfirm): Task[Checked[Unit]] = {
    import confirm.{clusterWatchId, clusterWatchRunId, requestId, problem as maybeProblem}

    Task(clusterWatchUniquenessChecker.check(clusterWatchId, clusterWatchRunId))
      .flatMapT { _ =>
        @tailrec def takeRequested(): Checked[Requested] =
          request.get() match {
            case None => Left(ClusterWatchRequestDoesNotMatchProblem)
            case value @ Some(req) =>
              if (requestId != req.id) {
                logger.debug(
                  s"$ClusterWatchRequestDoesNotMatchProblem id=$requestId but requested=${req.id}")
                Left(ClusterWatchRequestDoesNotMatchProblem)
              } else
                req.clusterWatchId match {
                  case Some(o) if o != clusterWatchId && !req.clusterWatchIdChangeAllowed =>
                    Left(ClusterWatchIdDoesNotMatchProblem(
                      rejectedClusterWatchId = clusterWatchId,
                      requestedClusterWatchId = o))

                  case Some(o) if o != clusterWatchId
                    && clusterWatchIdExpires.exists(_.hasTimeLeft()) =>
                    Left(OtherClusterWatchStillAliveProblem(
                      rejectedClusterWatchId = clusterWatchId,
                      requestedClusterWatchId = o))

                  case _ =>
                    if (!request.compareAndSet(value, None))
                      takeRequested()
                    else
                      Right(req)
                }
          }

        Task.pure(takeRequested())
          .flatMapT(_
            .confirmation
            .complete(maybeProblem.toLeft(confirm))
            .materialize // Ignore duplicate complete
            .as(Checked.unit))
      }
  }

  def onClusterWatchRegistered: Task[Unit] =
    Task {
      clusterWatchIdExpires = Some(now + timing.clusterWatchIdTimeout)
    }

  private def send(msg: ClusterWatchMessage): Task[Unit] =
    logger
      .traceTask("send", msg)(
        pubsub.publish(msg))
      .logWhenItTakesLonger("ClusterWatch sender")

  def newStream: Task[fs2.Stream[Task, ClusterWatchMessage]] =
    pubsub.newStream // TODO Delete all but the last message at a time. At push-side?

  override def toString = "ClusterWatchCounterpart"
}

object ClusterWatchCounterpart
{
  private val logger = Logger[this.type]

  def resource(clusterConf: ClusterConf, timing: ClusterTiming): Resource[Task, ClusterWatchCounterpart] =
    Service.resource(Task(
      new ClusterWatchCounterpart(clusterConf, timing)))

  private final class Requested(
    val clusterWatchId: Option[ClusterWatchId],
    val id: RequestId,
    val clusterWatchIdChangeAllowed: Boolean)
  {
    val confirmation = Deferred.unsafe[Task, Checked[ClusterWatchConfirm]]
    override def toString = s"Requested($id,$clusterWatchId)"
  }

  private object RequestTimeoutException extends Exception
}
