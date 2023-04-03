package js7.controller.agent

import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.Batch
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, CorrelIdWrapped, Logger}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.{RichDeadline, RichFiniteDuration}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichThrowable}
import js7.controller.agent.AgentDriver.Queueable
import js7.controller.agent.CommandQueue.*
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
  protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]): Task[Unit]
  protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem): Task[Unit]

  private val logger = Logger.withPrefix[this.type](agentPath.string)
  private val lock = AsyncLock()
  private val attachedOrderIds = mutable.Set.empty[OrderId]
  private val executingInputs = mutable.Set.empty[Queueable]
  private var delayCommandExecutionAfterErrorUntil = now
  private var isCoupled = false
  private var freshlyCoupled = false
  private var openRequestCount = 0
  private var isTerminating = false

  private object queue {
    private val queue = mutable.Queue.empty[Queueable]
    private val queueSet = mutable.Set.empty[Queueable]
    private val detachQueue = mutable.Queue.empty[Queueable]  // DetachOrder is sent to Agent before any AttachOrder, to relieve the Agent

    def enqueue(input: Queueable): Unit =
      input match {
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
      maySendLocked
    })

  final def onDecoupled(): Task[Unit] =
    lock.lock(Task {
      isCoupled = false
    })

  final def enqueue(input: Queueable): Task[Boolean] =
    lock.lock(Task.defer {
      assertThat(!isTerminating)
      input match {
        case Queueable.AttachOrder(order, _) if attachedOrderIds contains order.id =>
          logger.debug(s"AttachOrder(${order.id} ignored because Order is already attached to Agent")
          Task.pure(false)
        case _ =>
          if (queue contains input) {
            logger.trace(s"Ignore duplicate $input")
            Task.pure(false)
          } else {
            queue.enqueue(input)
            Task
              .when(queue.size == batchSize || freshlyCoupled)(
                maySendLocked)
              .as(true)
          }
      }
    })

  final def reset: Task[Unit] =
    /*FIXME lock*/(Task {
      attachedOrderIds.clear()
      isCoupled = false
      freshlyCoupled = false
    })

  final def terminate: Task[Unit] =
    lock.lock(Task {
      if (executingInputs.nonEmpty) {
        logger.info(s"🟡 Waiting for responses to AgentCommands: ${executingInputs.map(_.toShortString).mkString(", ")}")
      }
      isTerminating = true
    })

  final def isTerminated =
    isTerminating && executingInputs.isEmpty

  final def maySend: Task[Unit] =
    lock.lock(maySendLocked)

  private def maySendLocked: Task[Unit] =
    logger.traceTask(Task.defer(Task.when(isCoupled && !isTerminating) {
      lazy val inputs = queue.view
        .filterNot(executingInputs)
        .take(if (freshlyCoupled) 1 else batchSize)  // if freshlyCoupled, send only command to try connection
        .toVector

      val canSend = openRequestCount < commandParallelism
        && (!freshlyCoupled || openRequestCount == 0)
        && inputs.nonEmpty

      Task.when(canSend)(Task.defer {
        executingInputs ++= inputs
        openRequestCount += 1
        delayNextCommand
          .*>(sendNow(inputs))
          .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
          .startAndForget /*TODO*/
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
          for ((i, r) <- queuable zip response.responses) yield QueuedInputResponse(i, r)))
        .materialize
        .flatMap {
          case Success(Right(queuedInputResponses)) =>
            asyncOnBatchSucceeded(queuedInputResponses)

          case Success(Left(problem)) =>
            asyncOnBatchFailed(queuable, problem)

          case Failure(t) =>
            asyncOnBatchFailed(queuable, Problem.fromThrowable(t))
        }
        .startAndForget
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

      case Queueable.ClusterAppointNodes(idToUri, activeId) =>
        AgentCommand.ClusterAppointNodes(idToUri, activeId)

      case Queueable.ClusterSwitchOver =>
        AgentCommand.ClusterSwitchOver
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

  final def handleBatchSucceeded(responses: Seq[QueuedInputResponse]): Task[Seq[Queueable]] =
    lock.lock(Task.defer {
      freshlyCoupled = false

      // Dequeue commands including rejected ones, but not those with ServiceUnavailable response.
      // The dequeued commands will not be repeated !!!
      queue.dequeueAll(responses.view
        .flatMap(r => r.response.left.forall(_.httpStatusCode != 503/*ServiceUnavailable*/) ?
          r.input)
        .toSet)
      onQueuedInputsResponded(responses.map(_.input).toSet)
        .*>(Task {
          responses.flatMap {
            case QueuedInputResponse(input, Right(AgentCommand.Response.Accepted)) =>
              Some(input)

            case QueuedInputResponse(_, Right(o)) =>
              sys.error(s"Unexpected response from Agent: $o")

            //case QueuedInputResponse(input, Left(AgentIsShuttingDown)) =>
            // TODO Be sure to repeat the command after coupling

            case QueuedInputResponse(input, Left(problem)) =>
              // MarkOrder(FreshOnly) fails if order has started !!!
              logger.error(s"Agent rejected ${input.toShortString}: $problem")
              // Agent's state does not match controller's state ???
              None
          }
        })
    })

  final def handleBatchFailed(inputs: Seq[Queueable]): Task[Unit] =
    lock.lock(Task.defer {
      delayCommandExecutionAfterErrorUntil = now + commandErrorDelay
      logger.trace(
        s"delayCommandExecutionAfterErrorUntil=${delayCommandExecutionAfterErrorUntil.toTimestamp}")
      // Don't remove from queue. Queued inputs will be processed again
      onQueuedInputsResponded(inputs.toSet)
    })

  private def onQueuedInputsResponded(inputs: Set[Queueable]): Task[Unit] =
    Task.defer {
      executingInputs --= inputs
      openRequestCount -= 1
      maySend
        .onErrorHandle(t => Task(logger.error(t.toStringWithCauses, t)))
        .startAndForget /*TODO*/
    }
}

object CommandQueue
{
  private[agent] final case class QueuedInputResponse(
    input: Queueable,
    response: Checked[AgentCommand.Response])
}
