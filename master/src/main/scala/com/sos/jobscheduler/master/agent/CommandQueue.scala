package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Batch
import com.sos.jobscheduler.master.agent.AgentDriver.Input
import com.sos.jobscheduler.master.agent.CommandQueue._
import com.typesafe.scalalogging.{Logger ⇒ ScalaLogger}
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
private[agent] abstract class CommandQueue(logger: ScalaLogger, batchSize: Int)(implicit s: Scheduler) {

  protected def executeCommand(command: AgentCommand.Batch): Task[command.Response]
  protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]): Unit
  protected def asyncOnBatchFailed(inputs: Vector[Input.QueueableInput], throwable: Throwable): Unit

  private val executingInputs = mutable.Set[Input.QueueableInput]()
  private var freshReconnected = true  // After connect, send a single command first. Start queueing first after one successful response.
  private var openRequestCount = 0

  private object queue {
    private val attachOrCancelQueue = mutable.Queue[Input.QueueableInput]()
    private val detachQueue = mutable.Queue[Input.QueueableInput]()  // DetachOrder is sent to Agent prior any AttachOrder, to relieve the Agent

    def enqueue(input: Input.QueueableInput): Unit =
      input match {
        case o: Input.AttachOrder ⇒ attachOrCancelQueue += o
        case o: Input.DetachOrder ⇒ detachQueue += o
        case o: Input.CancelOrder ⇒ attachOrCancelQueue += o
      }

    def dequeueAll(what: Set[Input.QueueableInput]): Unit = {
      attachOrCancelQueue.dequeueAll(what)
      detachQueue.dequeueAll(what)
    }

    def size = attachOrCancelQueue.size + detachQueue.size

    def iterator = detachQueue.iterator ++ attachOrCancelQueue.iterator
  }

  final def onRecoupled() =
    freshReconnected = true

  final def enqueue(input: Input.QueueableInput): Unit = {
    queue.enqueue(input)
    if (queue.size == batchSize || freshReconnected) {
      maySend()
    }
  }

  final def maySend(): Unit = {
    lazy val inputs = queue.iterator.filterNot(executingInputs).take(batchSize).toVector
    if (openRequestCount < OpenRequestsMaximum && (!freshReconnected || openRequestCount == 0) && inputs.nonEmpty) {
      executingInputs ++= inputs
      openRequestCount += 1
      executeCommand(Batch(inputs map inputToAgentCommand))
        .runAsync
        .andThen {
          case Success(Batch.Response(responses)) ⇒
            asyncOnBatchSucceeded(for ((i, r) ← inputs zip responses) yield QueuedInputResponse(i, r))

          case Failure(t) ⇒
            asyncOnBatchFailed(inputs, t)
        }
    }
  }

  private def inputToAgentCommand(input: Input.QueueableInput): AgentCommand =
    input match {
      case Input.AttachOrder(order, agentId, workflow) ⇒
        AgentCommand.AttachOrder(order, agentId, workflow)
      case Input.DetachOrder(orderId) ⇒
        AgentCommand.DetachOrder(orderId)
      case Input.CancelOrder(orderId, mode) ⇒
        AgentCommand.CancelOrder(orderId, mode)
    }

  final def handleBatchSucceeded(responses: Seq[QueuedInputResponse]): Seq[Input.QueueableInput] = {
    freshReconnected = false
    val inputs = responses.map(_.input).toSet
    queue.dequeueAll(inputs)  // Including rejected commands. The corresponding orders are ignored henceforth.
    onQueuedInputsResponded(inputs)
    responses.flatMap {
      case QueuedInputResponse(input, Batch.Succeeded(AgentCommand.Response.Accepted)) ⇒
        Some(input)
      case QueuedInputResponse(_, Batch.Succeeded(o)) ⇒
        sys.error(s"Unexpected response from agent: $o")
      case QueuedInputResponse(input, Batch.Failed(message)) ⇒
        // CancelOrder(NotStarted) fails if order has started !!!
        logger.error(s"Agent has rejected the command ${input.toShortString}: $message")
        // Agent's state does not match master's state ???
        // TODO: But "Agent is shutting down" is okay
        None
    }
  }

  final def handleBatchFailed(inputs: Seq[Input.QueueableInput]): Unit = {
    // Don't remove from queue. Queued inputs will be processed again
    onQueuedInputsResponded(inputs.toSet)
  }

  private def onQueuedInputsResponded(inputs: Set[Input.QueueableInput]): Unit = {
    executingInputs --= inputs
    openRequestCount -= 1
    maySend()
  }
}

object CommandQueue {
  private val OpenRequestsMaximum = 2

  private[agent] final case class QueuedInputResponse(input: Input.QueueableInput, response: Batch.SingleResponse)
}
