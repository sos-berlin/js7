package js7.controller.agent

import cats.effect.concurrent.Deferred
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.Batch
import js7.base.log.{CorrelId, CorrelIdWrapped, Logger}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.{RichDeadline, RichFiniteDuration}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichThrowable}
import js7.controller.agent.AgentDriver.Queueable
import js7.controller.agent.CommandQueue.*
import js7.controller.agent.DirectorDriver.DirectorDriverStoppedProblem
import js7.data.agent.AgentPath
import js7.data.order.OrderId
import monix.eval.Task
import scala.collection.{View, mutable}
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
private[agent] abstract class CommandQueue(
  agentPath: AgentPath,
  batchSize: Int,
  commandErrorDelay: FiniteDuration)
{
  protected def commandParallelism: Int
  protected def executeCommand(command: AgentCommand.Batch): Task[Checked[command.Response]]
  protected def asyncOnBatchSucceeded(queueableResponses: Seq[QueueableResponse]): Task[Unit]
  protected def asyncOnBatchFailed(queueables: Vector[Queueable], problem: Problem): Task[Unit]

  private val logger = Logger.withPrefix[this.type](agentPath.string)
  private val lock = AsyncLock()
  private val attachedOrderIds = mutable.Set.empty[OrderId]
  private val isExecuting = mutable.Set.empty[Queueable]
  private var delayCommandExecutionAfterErrorUntil = now
  private var isCoupled = false
  private var freshlyCoupled = false
  private var openRequestCount = 0
  private val isTerminating = false
  private val terminated = Deferred.unsafe[Task, Unit]

  private object queue {
    private val queue = mutable.Queue.empty[Queueable]
    private val queueSet = mutable.Set.empty[Queueable]
    private val detachQueue = mutable.Queue.empty[Queueable]  // DetachOrder is sent to Agent before any AttachOrder, to relieve the Agent

    def enqueue(queueable: Queueable): Unit =
      queueable match {
        case o: Queueable.DetachOrder => detachQueue += o

        case attach: Queueable.AttachUnsignedItem =>
          val duplicates = queue.collect {
            case o: Queueable.AttachUnsignedItem if o.item.key == attach.item.key => o
          }
          queue --= duplicates
          queueSet --= duplicates
          queue += attach
          queueSet += attach

        case o =>
          queue += o
          queueSet += o
      }

    def dequeueAll(what: Set[Queueable]): Unit = {
      queue.dequeueAll(what)
      queueSet --= what
      detachQueue.dequeueAll(what)
    }

    def removeAlreadyAttachedOrders(): Unit = {
      def isAlreadyAttached(log: Boolean = false)(queueable: Queueable): Boolean =
        queueable match {
          case o: Queueable.AttachOrder if attachedOrderIds.contains(o.order.id) =>
            if (log) logger.trace(s"removeAlreadyAttachedOrders: ${o.order.id}")
            true
          case _ =>
            false
        }
      queue.removeAll(isAlreadyAttached(log = true))
      queueSet --= queueSet filter isAlreadyAttached()
    }

    def size = queue.size + detachQueue.size

    def view: View[Queueable] =
      detachQueue.view ++ queue.view

    def contains(queueable: Queueable) =
      queueSet contains queueable

    override def toString = s"CommandQueue.queue(${queue.map(_.toShortString)})"
  }

  final def onCoupled(attachedOrderIds: Set[OrderId]): Task[Unit] =
    lock.lock(Task.defer {
      this.attachedOrderIds.clear()
      this.attachedOrderIds ++= attachedOrderIds
      queue.removeAlreadyAttachedOrders()
      isCoupled = true
      freshlyCoupled = true
      maybeStartSendingLocked
    })

  final def onDecoupled(): Task[Unit] =
    lock.lock(Task {
      isCoupled = false
    })

  final def enqueue(queueable: Queueable): Task[Boolean] =
    lock.lock(Task.defer {
      assertThat(!isTerminating)
      queueable match {
        case Queueable.AttachOrder(order, _) if attachedOrderIds contains order.id =>
          logger.debug(s"AttachOrder(${order.id} ignored because Order is already attached to Agent")
          Task.pure(false)
        case _ =>
          if (queue contains queueable) {
            logger.trace(s"Ignore duplicate $queueable")
            Task.pure(false)
          } else {
            logger.trace(s"enqueue $queueable")
            queue.enqueue(queueable)
            Task
              .when(queue.size == batchSize || freshlyCoupled)(
                maybeStartSendingLocked)
              .as(true)
          }
      }
    })

  final def untilTerminated: Task[Unit] =
    terminated.get

  final def maybeStartSending: Task[Unit] =
    lock.lock(maybeStartSendingLocked)

  private def maybeStartSendingLocked: Task[Unit] =
    /*logger.traceTask*/(Task.defer(Task.when(isCoupled && !isTerminating) {
      lazy val queueables = queue.view
        .filterNot(isExecuting)
        .take(if (freshlyCoupled) 1 else batchSize)  // if freshlyCoupled, send only command to try connection
        .toVector

      val canSend = openRequestCount < commandParallelism
        && (!freshlyCoupled || openRequestCount == 0)
        && queueables.nonEmpty

      Task.when(canSend)(Task.defer {
        isExecuting ++= queueables
        openRequestCount += 1
        delayNextCommand
          .*>(sendNow(queueables))
          .onErrorHandle(t => logger.error(t.toStringWithCauses, t))
          .startAndForget
      })
    }))

  private def delayNextCommand: Task[Unit] =
    Task.defer {
      val delay = delayCommandExecutionAfterErrorUntil.timeLeft
      Task.when(delay.isPositive) {
        logger.debug(s"Delay command after error for ${delay.pretty}")
        Task.sleep(delay)
      }
    }

  private def sendNow(queuable: Vector[Queueable]): Task[Unit] =
    Task.defer {
      val subcmds = queuable.map(o => CorrelIdWrapped(CorrelId.current, queuableToAgentCommand(o)))
      executeCommand(Batch(subcmds))
        .map(_.map(response =>
          for ((i, r) <- queuable zip response.responses) yield QueueableResponse(i, r)))
        .materialize
        .flatMap {
          case Success(Right(queueableResponses)) =>
            logger.debug(s"✔︎ sendNow queueables=${queuable.map(_.toShortString)}")
            asyncOnBatchSucceeded(queueableResponses)

          case Success(Left(problem)) =>
            logger.debug(s"💥 sendNow: $problem")
            logger.debug(s"💥 sendNow: queueables=${queuable.map(_.toShortString)}")
            asyncOnBatchFailed(queuable, problem)

          case Failure(t) =>
            logger.debug(s"💥 sendNow: $t")
            logger.debug(s"💥 sendNow: queueables=${queuable.map(_.toShortString)}")
            asyncOnBatchFailed(queuable, Problem.fromThrowable(t))
        }
    }

  private def queuableToAgentCommand(queuable: Queueable): AgentCommand =
    queuable match {
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
    }

  final def onOrdersDetached(orderIds: View[OrderId]): Task[Unit] =
    Task.when(orderIds.nonEmpty)(lock.lock(Task[Unit] {
      attachedOrderIds --= orderIds
    }))

  final def onOrdersAttached(orderIds: View[OrderId]): Task[Unit] =
    Task.when(orderIds.nonEmpty)(lock.lock(Task {
      attachedOrderIds ++= orderIds
      logger.trace(s"attachedOrderIds=${attachedOrderIds.toSeq.sorted.mkString(" ")}")
    }))

  final def handleBatchSucceeded(responses: Seq[QueueableResponse]): Task[Seq[Queueable]] =
    lock.lock(Task.defer {
      freshlyCoupled = false

      // Dequeue commands including rejected ones, but not those with ServiceUnavailable response.
      // The dequeued commands will not be repeated !!!

      def isRepeatable(problem: Problem) = problem match {
        case _: DirectorDriverStoppedProblem => true
        case _ => problem.httpStatusCode == 503/*ServiceUnavailable*/
      }

      queue.dequeueAll(responses.view
        .flatMap(r => r.response.left.forall(prblm => !isRepeatable(prblm)) ?
          r.queueable)
        .toSet)
      onQueueableResponded(responses.map(_.queueable).toSet)
        .*>(Task {
          responses.flatMap {
            case QueueableResponse(queueable, Right(AgentCommand.Response.Accepted)) =>
              Some(queueable)

            case QueueableResponse(_, Right(o)) =>
              sys.error(s"Unexpected response from Agent: $o")

            //case QueueableResponse(queueable, Left(AgentIsShuttingDown)) =>
            // TODO Be sure to repeat the command after coupling

            case QueueableResponse(queueable, Left(problem)) =>
              // MarkOrder(FreshOnly) fails if order has started !!!
              logger.error(s"Agent rejected ${queueable.toShortString}: $problem")
              // Agent's state does not match controller's state ???
              None
          }
        })
    })

  final def handleBatchFailed(queuables: Seq[Queueable]): Task[Unit] =
    lock.lock(Task.defer {
      delayCommandExecutionAfterErrorUntil = now + commandErrorDelay
      logger.trace(
        s"delayCommandExecutionAfterErrorUntil=${delayCommandExecutionAfterErrorUntil.toTimestamp}")
      // Don't remove from queue. Queued queueables will be processed again
      onQueueableResponded(queuables.toSet)
    })

  private def onQueueableResponded(queueables: Set[Queueable]): Task[Unit] =
    Task.defer {
      isExecuting --= queueables
      openRequestCount -= 1
      if (isTerminating && isExecuting.isEmpty)
        terminated.complete(())
      else
        maybeStartSendingLocked
    }
}

object CommandQueue
{
  private[agent] final case class QueueableResponse(
    queueable: Queueable,
    response: Checked[AgentCommand.Response])
}
