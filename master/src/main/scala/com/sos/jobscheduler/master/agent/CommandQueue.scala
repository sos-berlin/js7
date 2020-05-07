package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Batch
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.agent.AgentDriver.{Input, Queueable, ReleaseEventsQueueable}
import com.sos.jobscheduler.master.agent.CommandQueue._
import com.typesafe.scalalogging.{Logger => ScalaLogger}
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

  private val attachedOrderIds = mutable.Set[OrderId]()
  private val executingInputs = mutable.Set[Queueable]()
  private var isCoupled = false
  private var freshlyCoupled = false
  private var openRequestCount = 0
  private var isTerminating = false

  private object queue {
    private val queue = mutable.Queue[Queueable]()
    private val detachQueue = mutable.Queue[Queueable]()  // DetachOrder is sent to Agent before any AttachOrder, to relieve the Agent

    def enqueue(input: Queueable): Unit =
      input match {
        case o: Input.DetachOrder => detachQueue += o
        case o => queue += o
      }

    def dequeueAll(what: Set[Queueable]): Unit = {
      queue.dequeueAll(what)
      detachQueue.dequeueAll(what)
    }

    def removeAlreadyAttachedOrders(): Unit =
      queue.removeAll {
        case o: Input.AttachOrder if attachedOrderIds.contains(o.order.id) =>
          logger.trace(s"removeAlreadyAttachedOrders: ${o.order.id}")
          true
        case _ =>
          false
      }

    def size = queue.size + detachQueue.size

    def view: View[Queueable] =
      detachQueue.view ++ queue.view
  }

  final def onCoupled(attachedOrderIds: Set[OrderId]) = {
    this.attachedOrderIds.clear()
    this.attachedOrderIds ++= attachedOrderIds
    queue.removeAlreadyAttachedOrders()
    isCoupled = true
    freshlyCoupled = true
    maySend()
  }

  final def onDecoupled(): Unit =
    isCoupled = false

  final def enqueue(input: Queueable): Unit = {
    assertThat(!isTerminating)
    input match {
      case Input.AttachOrder(order, _, _) if attachedOrderIds contains order.id =>
        logger.trace(s"AttachOrder(${order.id} ignored because Order is already attached to Agent")
      case _ =>
        queue.enqueue(input)
        if (queue.size == batchSize || freshlyCoupled) {
          maySend()
        }
    }
  }

  final def terminate(): Unit = {
    if (executingInputs.nonEmpty) {
      logger.info(s"Waiting for responses to AgentCommands: ${executingInputs.map(_.toShortString).mkString(", ")}")
    }
    isTerminating = true
  }

  final def isTerminated =
    isTerminating && executingInputs.isEmpty

  final def maySend(): Unit = {
    logger.trace(s"maySend() isCoupled=$isCoupled freshlyCoupled=$freshlyCoupled openRequestCount=$openRequestCount" +
      s" commandParallelism=$commandParallelism isTerminating=$isTerminated" +
      s" attachedOrderIds.size=${attachedOrderIds.size} executingInput={${executingInputs.map(_.toShortString).mkString(" ")}}")
    if (isCoupled && !isTerminating) {
      lazy val inputs = queue.view
        .filterNot(executingInputs)
        .take(if (freshlyCoupled) 1 else batchSize)  // if freshlyCoupled, send only command to try connection
        .toVector
      if (openRequestCount < commandParallelism && (!freshlyCoupled || openRequestCount == 0) && inputs.nonEmpty) {
        executingInputs ++= inputs
        openRequestCount += 1
        executeCommand(Batch(inputs map inputToAgentCommand))
          .materialize foreach {
            case Success(Right(Batch.Response(responses))) =>
              asyncOnBatchSucceeded(for ((i, r) <- inputs zip responses) yield QueuedInputResponse(i, r))

            case Success(Left(problem)) =>
              asyncOnBatchFailed(inputs, problem)

            case Failure(t) =>
              asyncOnBatchFailed(inputs, Problem.pure(t))
          }
      }
    }
  }

  private def inputToAgentCommand(input: Queueable): AgentCommand =
    input match {
      case Input.AttachOrder(order, agentRefPath, signedWorkflow) =>
        AgentCommand.AttachOrder(order, agentRefPath, signedWorkflow.signedString)
      case Input.DetachOrder(orderId) =>
        AgentCommand.DetachOrder(orderId)
      case Input.CancelOrder(orderId, mode) =>
        AgentCommand.CancelOrder(orderId, mode)
      case ReleaseEventsQueueable(untilEventId) =>
        AgentCommand.ReleaseEvents(untilEventId)
    }

  final def handleBatchSucceeded(responses: Seq[QueuedInputResponse]): Seq[Queueable] = {
    freshlyCoupled = false
    val inputs = responses.map(_.input).toSet
    attachedOrderIds --= inputs collect { case Input.DetachOrder(orderId) => orderId }
    attachedOrderIds ++= inputs collect { case Input.AttachOrder(order, _, _) => order.id }
    logger.trace(s"attachedOrderIds=${attachedOrderIds.toSeq.sorted.mkString(" ")}")
    queue.dequeueAll(inputs)  // Including rejected commands. The corresponding orders are ignored henceforth.
    onQueuedInputsResponded(inputs)
    responses.flatMap {
      case QueuedInputResponse(input, Right(AgentCommand.Response.Accepted)) =>
        Some(input)
      case QueuedInputResponse(_, Right(o)) =>
        sys.error(s"Unexpected response from Agent: $o")
      case QueuedInputResponse(input, Left(problem)) =>
        // CancelOrder(NotStarted) fails if order has started !!!
        logger.error(s"Agent has rejected ${input.toShortString}: $problem")
        // Agent's state does not match master's state ???
        // TODO: But "Agent is shutting down" is okay
        None
    }
  }

  final def handleBatchFailed(inputs: Seq[Queueable]): Unit = {
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
  private[agent] final case class QueuedInputResponse(input: Queueable, response: Checked[AgentCommand.Response])
}
