package js7.controller.agent

import cats.effect.{Deferred, IO}
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.Batch
import js7.base.catsutils.CatsEffectExtensions.startAndForget
import js7.base.catsutils.CatsExtensions.{ifTrue, tryIt}
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, CorrelIdWrapped, Logger}
import js7.base.problem.Problems.ShuttingDownProblem
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.{DurationRichInt, RichDeadline, RichFiniteDuration}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.{RichEitherF, RichJavaClass, RichThrowable}
import js7.controller.agent.AgentDriver.Queueable
import js7.controller.agent.AgentDriver.Queueable.{AttachOrder, DetachOrder, MarkOrder}
import js7.controller.agent.CommandQueue.*
import js7.controller.agent.DirectorDriver.DirectorDriverStoppedProblem
import js7.data.Problems.ClusterNodeHasBeenSwitchedOverProblem
import js7.data.agent.AgentPath
import js7.data.agent.Problems.AgentIsShuttingDown
import js7.data.order.OrderId
import js7.data.subagent.Problems.NoDirectorProblem
import scala.collection.{View, mutable}
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
private[agent] abstract class CommandQueue(
  agentPath: AgentPath,
  batchSize: Int,
  commandErrorDelay: FiniteDuration):

  protected def commandParallelism: Int
  protected def executeCommand(command: AgentCommand.Batch): IO[Checked[command.Response]]
  protected def asyncOnBatchSucceeded(queueableResponses: Seq[QueueableResponse]): IO[Unit]
  protected def asyncOnBatchFailed(queueables: Vector[Queueable], problem: Problem): IO[Unit]

  private val logger = Logger.withPrefix[this.type](agentPath.string)
  private val shortErrorDelay = 1.s min commandErrorDelay
  private val lock = AsyncLock()
  private val attachedOrderIds = mutable.Set.empty[OrderId]
  private val isExecuting = mutable.Set.empty[Queueable]
  private var delayCommandExecutionAfterErrorUntil = now
  private var isCoupled = false
  private var freshlyCoupled = false
  private var openRequestCount = 0
  private val isTerminating = false
  private val terminated = Deferred.unsafe[IO, Unit]

  private object queue:
    private val queue = mutable.Queue.empty[Queueable]
    private val queueSet = mutable.Set.empty[Queueable]
    private val detachQueue = mutable.Queue.empty[Queueable.DetachOrder]  // DetachOrder is sent to Agent before any AttachOrder, to relieve the Agent

    def enqueue(queueable: Queueable): Unit =
      queueable match
        case detachOrder: Queueable.DetachOrder =>
          removeStillQueuedCommandsWhenDetachingTheOrder(detachOrder)
          detachQueue += detachOrder

        case attach: Queueable.AttachUnsignedItem =>
          val duplicates = queue.collect:
            case o: Queueable.AttachUnsignedItem if o.item.key == attach.item.key => o
          queue --= duplicates
          queueSet --= duplicates
          queue += attach
          queueSet += attach

        case o =>
          queue += o
          queueSet += o

    private def removeStillQueuedCommandsWhenDetachingTheOrder(detachOrder: DetachOrder): Unit =
      // In case we didn't receive a proper response from the Agent
      def isCorrespondingCommand(queueable: Queueable) =
        queueable match
          case Queueable.AttachOrder(order, _) =>
            order.id == detachOrder.orderId
          case Queueable.MarkOrder(orderId, _) =>
            orderId == detachOrder.orderId
          case _ => false

      queue.removeFirst(isCorrespondingCommand).foreach: queueable =>
        logger.debug:
          s"⚠️  DetachOrder(${detachOrder.orderId}) removes corresponding ${
            queueable.getClass.simpleScalaName
          } from queue, because it obviously has been executed by the Agent: $queueable"
        queueSet -= queueable

    def dequeueAll(what: Set[Queueable]): Unit =
      queue.dequeueAll(what)
      queueSet --= what
      detachQueue.dequeueAll(what)

    def removeAlreadyAttachedOrders(): Unit =
      def isAlreadyAttached(log: Boolean = false)(queueable: Queueable): Boolean =
        queueable match
          case o: Queueable.AttachOrder if attachedOrderIds.contains(o.order.id) =>
            if log then logger.trace(s"removeAlreadyAttachedOrders: ${o.order.id}")
            true
          case _ =>
            false
      queue.removeAll(isAlreadyAttached(log = true))
      queueSet --= queueSet filter isAlreadyAttached()

    def size = queue.size + detachQueue.size

    def view: View[Queueable] =
      detachQueue.view ++ queue.view

    def contains(queueable: Queueable) =
      queueSet contains queueable

    override def toString = s"CommandQueue.queue(${queue.map(_.toShortString)})"

  final def onCoupled(attachedOrderIds: Set[OrderId]): IO[Unit] =
    lock.lock:
      IO.defer:
        this.attachedOrderIds.clear()
        this.attachedOrderIds ++= attachedOrderIds
        queue.removeAlreadyAttachedOrders()
        isCoupled = true
        freshlyCoupled = true
        maybeStartSendingLocked

  final def onDecoupled(): IO[Unit] =
    lock.lock:
      IO:
        isCoupled = false

  final def enqueue(queueable: Queueable): IO[Boolean] =
    lock.lock:
      IO.defer:
        assertThat(!isTerminating)
        queueable match
          case Queueable.AttachOrder(order, _) if attachedOrderIds contains order.id =>
            logger.debug(s"AttachOrder(${order.id} ignored because Order is already attached to Agent")
            IO.pure(false)
          case _ =>
            if queue.contains(queueable) then
              logger.trace(s"Ignore duplicate $queueable")
              IO.pure(false)
            else
              logger.trace(s"enqueue $queueable")
              queue.enqueue(queueable)
              IO
                .whenA(queue.size == batchSize || freshlyCoupled)(
                  maybeStartSendingLocked)
                .as(true)

  final def untilTerminated: IO[Unit] =
    terminated.get

  final def maybeStartSending: IO[Unit] =
    lock.lock(maybeStartSendingLocked)

  private def maybeStartSendingLocked: IO[Unit] =
   logger.traceIO("### maybeStartSendingLocked"):
    IO(isCoupled && !isTerminating).ifTrue:
      lazy val queueables = queue.view.filterNot(isExecuting)
        .take(if freshlyCoupled then 1 else batchSize)  // if freshlyCoupled, send only command to try connection
        .toVector

      val canSend = openRequestCount < commandParallelism
        && (!freshlyCoupled || openRequestCount == 0)
        && queueables.nonEmpty
      IO.whenA(canSend):
        isExecuting ++= queueables
        openRequestCount += 1
        (delayNextCommand *> sendNow(queueables))
          .handleError: t =>
            logger.error(t.toStringWithCauses, t)
          .startAndForget.void

  private def delayNextCommand: IO[Unit] =
    IO.defer:
      val delay = delayCommandExecutionAfterErrorUntil.timeLeft
      IO.whenA(delay.isPositive):
        logger.debug(s"Delay command after error for ${delay.pretty}")
        IO.sleep(delay)

  private def sendNow(queuable: Vector[Queueable]): IO[Unit] =
    IO.defer:
      val subcmds = queuable.map(o => CorrelIdWrapped(CorrelId.current, queuableToAgentCommand(o)))
      executeCommand(Batch(subcmds))
        .mapmap: response =>
          for (i, r) <- queuable zip response.responses yield QueueableResponse(i, r)
        .tryIt
        .flatMap:
          case Success(Right(queueableResponses)) =>
            logger.debug(s"✔︎ sendNow queueables=${queuable.map(_.toShortString)}")
            asyncOnBatchSucceeded(queueableResponses)

          case Success(Left(problem)) =>
            logger.debug(s"💥 sendNow: $problem", problem.throwableOption.orNull)
            logger.debug(s"💥 sendNow: queueables=${queuable.map(_.toShortString)}")
            asyncOnBatchFailed(queuable, problem)

          case Failure(t) =>
            logger.debug(s"💥 sendNow: $t", t)
            logger.debug(s"💥 sendNow: queueables=${queuable.map(_.toShortString)}")
            asyncOnBatchFailed(queuable, Problem.fromThrowable(t))

  private def queuableToAgentCommand(queuable: Queueable): AgentCommand =
    queuable match
      case Queueable.AttachOrder(order, agentPath) =>
        AgentCommand.AttachOrder(order, agentPath)

      case Queueable.DetachOrder(orderId) =>
        AgentCommand.DetachOrder(orderId)

      case Queueable.MarkOrder(orderId, mark) =>
        AgentCommand.MarkOrder(orderId, mark)

      case Queueable.AttachUnsignedItem(item) =>
        AgentCommand.AttachItem(item)

      case Queueable.AttachSignedItem(signed) =>
        AgentCommand.AttachSignedItem(signed)

      case Queueable.DetachItem(id) =>
        AgentCommand.DetachItem(id)

      case Queueable.ReleaseEventsQueueable(untilEventId) =>
        AgentCommand.ReleaseEvents(untilEventId)

      case Queueable.ResetSubagent(subagentId, force) =>
        AgentCommand.ResetSubagent(subagentId, force = force)

  final def onOrdersDetached(orderIds: View[OrderId]): IO[Unit] =
    IO.whenA(orderIds.nonEmpty):
      lock.lock:
        IO[Unit]:
          attachedOrderIds --= orderIds

  final def onOrdersAttached(orderIds: View[OrderId]): IO[Unit] =
    IO.whenA(orderIds.nonEmpty):
      lock.lock:
        IO[Unit]:
          attachedOrderIds ++= orderIds
          //logger.trace(s"onOrdersAttached attachedOrderIds=${attachedOrderIds.toSeq.sorted.mkString(" ")}")

  final def handleBatchSucceeded(responses: Seq[QueueableResponse]): IO[Seq[Queueable]] =
    lock.lock:
      IO.defer:
        freshlyCoupled = false

        // Dequeue commands including rejected ones, but not those with ServiceUnavailable response.
        // The dequeued commands will not be repeated !!!

        // Some problems don't depend on the command, but on the Agent's state, so we repeat them
        def isRepeatableProblem(problem: Problem) =
          problem.httpStatusCode == 503 /*ServiceUnavailable*/ ||
            (problem is DirectorDriverStoppedProblem) ||
            (problem is ClusterNodeHasBeenSwitchedOverProblem) ||
            (problem is NoDirectorProblem) ||
            (problem is AgentIsShuttingDown) ||
            (problem is ShuttingDownProblem)

        val (repeatables, toBeDequeued) =
          responses.partition: r =>
            r.response.left.exists(isRepeatableProblem)
          .pipe: (a, b) =>
            (a.map(_.queueable).toSet, b.map(_.queueable).toSet)

        queue.dequeueAll(toBeDequeued)

        if repeatables.nonEmpty then
          delayCommandExecutionAfterErrorUntil = now + commandErrorDelay

        onQueueableResponded(responses.view.map(_.queueable).toSet) *>
          IO:
            responses.flatMap:
              case QueueableResponse(queueable, Right(AgentCommand.Response.Accepted)) =>
                Some(queueable)

              //case QueueableResponse(MarkOrder(orderId, _: OrderMark.Go), Right(o)) =>
              //  emit something like OrderResetGoMarked ???

              case QueueableResponse(_, Right(o)) =>
                sys.error(s"Unexpected response from Agent: $o")

              //case QueueableResponse(queueable, Left(AgentIsShuttingDown)) =>
              // TODO Be sure to repeat the command after coupling

              case QueueableResponse(queueable, Left(problem)) =>
                // MarkOrder(FreshOnly) fails if order has started !!!
                if repeatables(queueable) then
                  logger.debug(s"⟲ Agent rejected ${queueable.toShortString}: $problem")
                else
                  logger.error(s"Agent rejected ${queueable.toShortString}: $problem")
                // Agent's state does not match controller's state ???
                None

  final def handleBatchFailed(queuables: Seq[Queueable], delay: Boolean): IO[Unit] =
    lock.lock:
      IO.defer:
        delayCommandExecutionAfterErrorUntil =
          now + (if delay then commandErrorDelay else shortErrorDelay)
        logger.trace:
          s"delayCommandExecutionAfterErrorUntil=${delayCommandExecutionAfterErrorUntil.toTimestamp}"
        // Don't remove from queue. Queued queueables will be processed again
        onQueueableResponded(queuables.toSet)

  private def onQueueableResponded(queueables: Set[Queueable]): IO[Unit] =
    IO.defer:
      isExecuting --= queueables
      openRequestCount -= 1
      if isTerminating && isExecuting.isEmpty then
        terminated.complete(()).void
      else
        maybeStartSendingLocked


object CommandQueue:
  private[agent] final case class QueueableResponse(
    queueable: Queueable,
    response: Checked[AgentCommand.Response])
