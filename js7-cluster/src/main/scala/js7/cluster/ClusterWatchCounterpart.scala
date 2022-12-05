package js7.cluster

import js7.base.catsutils.Memoizable.syntax.*
import js7.base.fs2utils.Fs2PubSub
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.problem.{Checked, Problem}
import js7.base.service.StatefulService
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.Tests.isTest
import js7.cluster.ClusterWatchCounterpart.*
import js7.cluster.watch.api.ClusterWatchProblems.NoClusterWatchRequestMatches
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchMessage.RequestId
import js7.data.cluster.{ClusterEvent, ClusterTiming, ClusterWatchCheck, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchHeartbeat, ClusterWatchMessage}
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.atomic.Atomic
import scala.concurrent.Promise
import scala.util.Random

final class ClusterWatchCounterpart private(
  ownId: NodeId,
  timing: ClusterTiming)
extends StatefulService.StoppableByRequest
{
  private val nextRequestId = Atomic(if (isTest) 1 else Random.nextLong() * 1000)
  private val request = Atomic(None: Option[Requested])
  private val pubsub = new Fs2PubSub[Task, ClusterWatchMessage]

  def run =
    whenStopRequested *> pubsub.complete

  def checkClusterState(clusterState: HasNodes): Task[Checked[Completed]] =
    check(reqId =>
      ClusterWatchCheckState(reqId, CorrelId.current, ownId, clusterState))

  def heartbeat(clusterState: HasNodes): Task[Checked[Completed]] =
    send(ClusterWatchHeartbeat(CorrelId.current, ownId, clusterState))
      .as(Right(Completed))

  def applyEvents(event: ClusterEvent, clusterState: HasNodes): Task[Checked[Completed]] =
    check(reqId =>
      ClusterWatchCheckEvent(reqId, CorrelId.current, ownId, event, clusterState))

  private def check(toMessage: RequestId => ClusterWatchCheck): Task[Checked[Completed]] =
    Task.defer {
      val reqId = RequestId(nextRequestId.getAndIncrement())
      val msg = toMessage(reqId)
      val requestedCheck = new Requested(reqId)
      request.set(Some(requestedCheck))
      logger.debugTask("check", msg)(
        send(msg)
          .logWhenItTakesLonger("ClusterWatch sender"))
          .*>(Task
            .fromFuture(requestedCheck.promise.future)
            .timeoutTo(
              timing.clusterWatchReactionTimeout,
              Task.raiseError(RequestTimeoutException)))
          .onErrorRestartLoop(()) {
            case (RequestTimeoutException, _, retry) =>
              logger.warn("Still trying to get a response from ClusterWatch ...")
              retry(()).delayExecution(1.s/*TODO*/)

            case (t, _, _) => Task.raiseError(t)
          }
    }

  def onClusterWatchAcknowledged(id: RequestId, maybeProblem: Option[Problem]): Checked[Unit] =
    request.get() match {
      case value @ Some(requestedCheck) if requestedCheck.id == id =>
        request.compareAndSet(value, None)
        requestedCheck.promise.trySuccess(maybeProblem.toLeft(Completed))
        Checked.unit

      case _ =>
        Left(NoClusterWatchRequestMatches)
    }

  private def send(msg: ClusterWatchMessage): Task[Unit] =
    pubsub.publish(msg)

  def newStream: Task[fs2.Stream[Task, ClusterWatchMessage]] =
    pubsub.newStream

  override def toString = "ClusterWatchCounterpart"
}

object ClusterWatchCounterpart
{
  private val logger = Logger[this.type]

  def resource(ownId: NodeId, timing: ClusterTiming) =
    StatefulService.resource(Task(
      new ClusterWatchCounterpart(ownId, timing)))

  private final class Requested(val id: RequestId) {
    val promise = Promise[Checked[Completed]]()
  }

  private object RequestTimeoutException extends Exception
}
