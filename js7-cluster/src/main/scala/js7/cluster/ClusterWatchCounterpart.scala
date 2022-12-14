package js7.cluster

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import js7.base.catsutils.Memoizable.syntax.*
import js7.base.fs2utils.Fs2PubSub
import js7.base.generic.Completed
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.RichMonixTask
import js7.base.problem.{Checked, Problem}
import js7.base.service.Service
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.AsyncLock
import js7.base.utils.Tests.isTest
import js7.cluster.ClusterWatchCounterpart.*
import js7.cluster.watch.api.AnyClusterWatch
import js7.cluster.watch.api.ClusterWatchProblems.NoClusterWatchRequestMatches
import js7.data.cluster.ClusterState.HasNodes
import js7.data.cluster.ClusterWatchMessage.RequestId
import js7.data.cluster.{ClusterEvent, ClusterTiming, ClusterWatchCheck, ClusterWatchCheckEvent, ClusterWatchCheckState, ClusterWatchHeartbeat, ClusterWatchMessage}
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.atomic.Atomic
import scala.util.Random

final class ClusterWatchCounterpart private(
  ownId: NodeId,
  timing: ClusterTiming)
extends Service.StoppableByRequest with AnyClusterWatch
{
  private val nextRequestId = Atomic(if (isTest) 1 else Random.nextLong() * 1000)
  private val lock = AsyncLock()
  private val request = Atomic(None: Option[Requested])
  private val pubsub = new Fs2PubSub[Task, ClusterWatchMessage]

  protected def start =
    startService(
      whenStopRequested *> pubsub.complete)

  def tryLogout = Task.pure(Completed)

  def checkClusterState(clusterState: HasNodes): Task[Checked[Completed]] =
    check(reqId =>
      ClusterWatchCheckState(reqId, CorrelId.current, ownId, clusterState))

  def heartbeat(clusterState: HasNodes): Task[Checked[Completed]] =
    send(ClusterWatchHeartbeat(CorrelId.current, ownId, clusterState))
      .as(Right(Completed))

  def applyEvent(event: ClusterEvent, clusterState: HasNodes): Task[Checked[Completed]] =
    check(reqId =>
      ClusterWatchCheckEvent(reqId, CorrelId.current, ownId, event, clusterState))

  private def check(toMessage: RequestId => ClusterWatchCheck): Task[Checked[Completed]] =
    Task.defer {
      val reqId = RequestId(nextRequestId.getAndIncrement())
      val msg = toMessage(reqId)
      lock.lock(
        logger.debugTask("check", msg)(Task.defer {
          val requestedCheck = new Requested(reqId)
          request.set(Some(requestedCheck))
          send(msg)
            .*>(requestedCheck.promise.get.timeoutTo(
              timing.clusterWatchReactionTimeout,
              Task.raiseError(RequestTimeoutException)))
            .onErrorRestartLoop(()) {
              case (RequestTimeoutException, _, retry) =>
                logger.warn(
                  s"Still trying to get a response from ClusterWatch for ${msg.toShortString}...")
                retry(()).delayExecution(1.s /*TODO*/)

              case (t, _, _) => Task.raiseError(t)
            }
            .guarantee(Task(
              request.set(None)))
        }))
    }

  def onClusterWatchAcknowledged(id: RequestId, maybeProblem: Option[Problem])
  : Task[Checked[Unit]] =
    Task.defer {
      logger.debug(s"onClusterWatchAcknowledged $id${maybeProblem.fold("")(" 🚫 " + _)}")
      request.get() match {
        case value @ Some(requestedCheck) =>
          if (requestedCheck.id != id) {
            logger.debug(s"$NoClusterWatchRequestMatches id=$id but requested=${requestedCheck.id}")
            Task.left(NoClusterWatchRequestMatches)
          } else {
            request.compareAndSet(value, None)
            requestedCheck.promise.complete(maybeProblem.toLeft(Completed))
              .materialize  // Ignore duplicate complete
              .as(Checked.unit)
          }

        case _ =>
          Task.left(NoClusterWatchRequestMatches)
      }
    }

  private def send(msg: ClusterWatchMessage): Task[Unit] =
    logger
      .debugTask("send", msg)(
        pubsub.publish(msg))
      .logWhenItTakesLonger("ClusterWatch sender")

  // TODO Vielleicht eine Art Variable übergeben, statt Stream? Wir haben nicht mehr als ein Element!
  def newStream: Task[fs2.Stream[Task, ClusterWatchMessage]] =
    pubsub.newStream // TODO Delete all but the last message at a time. At push-side?

  override def toString = "ClusterWatchCounterpart"
}

object ClusterWatchCounterpart
{
  private val logger = Logger[this.type]

  def resource(ownId: NodeId, timing: ClusterTiming): Resource[Task, ClusterWatchCounterpart] =
    Service.resource(Task(
      new ClusterWatchCounterpart(ownId, timing)))

  private final class Requested(val id: RequestId) {
    val promise = Deferred.unsafe[Task, Checked[Completed]]
  }

  private object RequestTimeoutException extends Exception
}
