package js7.controller.agent

import com.typesafe.scalalogging.Logger as ScalaLogger
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.Batch
import js7.base.log.{CorrelId, CorrelIdWrapped}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.controller.agent.AgentDriver.{Input, Queueable, ReleaseEventsQueueable}
import js7.controller.agent.CommandQueue.*
import js7.data.order.OrderId
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.{View, mutable}
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
private[agent] abstract class CommandQueue(logger: ScalaLogger, batchSize: Int)(implicit s: Scheduler)
{
  protected def commandParallelism: Int
  protected def executeCommand(command: AgentCommand.Batch): Task[Checked[command.Response]]
  protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]): Unit
  protected def asyncOnBatchFailed(inputs: Vector[Queueable], problem: Problem): Unit

  private val attachedOrderIds = mutable.Set.empty[OrderId]
  private val executingInputs = mutable.Set.empty[Queueable]
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
        case o: Input.DetachOrder => detachQueue += o

        case attach: Input.AttachUnsignedItem =>
          val duplicates = queue.collect {
            case o: Input.AttachUnsignedItem if o.item.key == attach.item.key => o
          }
          queue --= duplicates
          queueSet --= duplicates
          queue += attach
          queueSet += attach

        //case attach: Input.AttachSignedItem => Same as for AttachUnsignedItem ???

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
          case o: Input.AttachOrder if attachedOrderIds.contains(o.order.id) =>
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
  }

  final def onCoupled(attachedOrderIds: Set[OrderId]) =
    synchronized/*for easier testing*/ {
      this.attachedOrderIds.clear()
      this.attachedOrderIds ++= attachedOrderIds
      queue.removeAlreadyAttachedOrders()
      isCoupled = true
      freshlyCoupled = true
      maySend()
    }

  final def onDecoupled(): Unit =
    isCoupled = false

  final def enqueue(input: Queueable): Boolean =
    synchronized {
      assertThat(!isTerminating)
      input match {
        case Input.AttachOrder(order, _) if attachedOrderIds contains order.id =>
          logger.debug(s"AttachOrder(${order.id} ignored because Order is already attached to Agent")
          false
        case _ =>
          if (queue.contains(input)) {
            logger.trace(s"Ignore duplicate $input")
            false
          } else {
            queue.enqueue(input)
            if (queue.size == batchSize || freshlyCoupled) {
              maySend()
            }
            true
          }
      }
    }

  final def reset(): Unit = {
    attachedOrderIds.clear()
    isCoupled = false
    freshlyCoupled = false
  }

  final def terminate(): Unit = {
    if (executingInputs.nonEmpty) {
      logger.info(s"Waiting for responses to AgentCommands: ${executingInputs.map(_.toShortString).mkString(", ")}")
    }
    isTerminating = true
  }

  final def isTerminated =
    isTerminating && executingInputs.isEmpty

  final def maySend(): Unit =
    synchronized {
      //logger.trace(s"maySend() isCoupled=$isCoupled freshlyCoupled=$freshlyCoupled openRequestCount=$openRequestCount" +
      //  s" commandParallelism=$commandParallelism isTerminating=$isTerminated" +
      //  s" attachedOrderIds.size=${attachedOrderIds.size} executingInput={${executingInputs.map(_.toShortString).mkString(" ")}}")
      if (isCoupled && !isTerminating) {
        lazy val inputs = queue.view
          .filterNot(executingInputs)
          .take(if (freshlyCoupled) 1 else batchSize)  // if freshlyCoupled, send only command to try connection
          .toVector
        if (openRequestCount < commandParallelism && (!freshlyCoupled || openRequestCount == 0) && inputs.nonEmpty) {
          executingInputs ++= inputs
          openRequestCount += 1
          val subcmds = inputs.map(o => CorrelIdWrapped(CorrelId.empty, inputToAgentCommand(o)))
          executeCommand(Batch(subcmds))
            .materialize foreach {
              case Success(Right(Batch.Response(responses))) =>
                asyncOnBatchSucceeded(
                  for ((i, r) <- inputs zip responses) yield
                    QueuedInputResponse(i, r))

              case Success(Left(problem)) =>
                asyncOnBatchFailed(inputs, problem)

              case Failure(t) =>
                asyncOnBatchFailed(inputs, Problem.fromThrowable(t))
            }
        }
      }
    }

  private def inputToAgentCommand(input: Queueable): AgentCommand =
    input match {
      case Input.AttachOrder(order, agentPath) =>
        AgentCommand.AttachOrder(order, agentPath)

      case Input.DetachOrder(orderId) =>
        AgentCommand.DetachOrder(orderId)

      case Input.MarkOrder(orderId, mark) =>
        AgentCommand.MarkOrder(orderId, mark)

      case Input.AttachUnsignedItem(item) =>
        AgentCommand.AttachItem(item)

      case Input.AttachSignedItem(signed) =>
        AgentCommand.AttachSignedItem(signed)

      case Input.DetachItem(id) =>
        AgentCommand.DetachItem(id)

      case ReleaseEventsQueueable(untilEventId) =>
        AgentCommand.ReleaseEvents(untilEventId)

      case Input.ResetSubagent(subagentId, force) =>
        AgentCommand.ResetSubagent(subagentId, force = force)

      case Input.ClusterAppointNodes(idToUri, activeId) =>
        AgentCommand.ClusterAppointNodes(idToUri, activeId)

      case Input.ClusterSwitchOver =>
        AgentCommand.ClusterSwitchOver
    }

  final def onOrdersDetached(orderIds: View[OrderId]): Unit = {
    attachedOrderIds --= orderIds
  }

  final def onOrdersAttached(orderIds: View[OrderId]): Unit = {
    attachedOrderIds ++= orderIds
    logger.trace(s"attachedOrderIds=${attachedOrderIds.toSeq.sorted.mkString(" ")}")
  }

  final def handleBatchSucceeded(responses: Seq[QueuedInputResponse]): Seq[Queueable] =
    synchronized {
      freshlyCoupled = false
      val inputs = responses.map(_.input).toSet

      // Dequeue commands including rejected ones, but not those with ServiceUnavailable response.
      // The dequeued commands will not be repeated !!!
      queue.dequeueAll(responses.view
        .flatMap(r => r.response.left.forall(_.httpStatusCode != 503/*ServiceUnavailable*/) ?
          r.input)
        .toSet)
      onQueuedInputsResponded(responses.map(_.input).toSet)
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
    }

  final def handleBatchFailed(inputs: Seq[Queueable]): Unit =
    synchronized {
      // Don't remove from queue. Queued inputs will be processed again
      onQueuedInputsResponded(inputs.toSet)
    }

  private def onQueuedInputsResponded(inputs: Set[Queueable]): Unit = {
    executingInputs --= inputs
    openRequestCount -= 1
    maySend()
  }
}

object CommandQueue
{
  private[agent] final case class QueuedInputResponse(
    input: Queueable,
    response: Checked[AgentCommand.Response])
}
