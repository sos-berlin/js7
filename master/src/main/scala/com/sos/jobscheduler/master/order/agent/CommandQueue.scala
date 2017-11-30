package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Accepted, Batch}
import com.sos.jobscheduler.master.order.agent.AgentDriver.Input
import com.sos.jobscheduler.master.order.agent.CommandQueue._
import com.typesafe.scalalogging.{Logger ⇒ ScalaLogger}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
private[agent] abstract class CommandQueue(logger: ScalaLogger, batchSize: Int)(implicit ec: ExecutionContext) {

  protected def executeCommand(command: AgentCommand.Batch): Future[command.Response]
  protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]): Unit
  protected def asyncOnBatchFailed(inputs: Vector[Input.QueueableInput], throwable: Throwable): Unit

  private val executingInputs = mutable.Set[Input.QueueableInput]()
  private var freshReconnected = true  // After connect, send a single command first. Start queueing first after one successful response.
  private var openRequestCount = 0

  private object queue {
    private val attachQueue = mutable.Queue[Input.QueueableInput]()
    private val detachQueue = mutable.Queue[Input.QueueableInput]()  // DetachOrder is sent to Agent prior any AttachOrder, to relieve the Agent

    def enqueue(input: Input.QueueableInput): Unit =
      input match {
        case o: Input.AttachOrder ⇒ attachQueue += o
        case o: Input.DetachOrder ⇒ detachQueue += o
      }

    def dequeueAll(what: Set[Input.QueueableInput]): Unit = {
      attachQueue.dequeueAll(what)
      detachQueue.dequeueAll(what)
    }

    def size = attachQueue.size + detachQueue.size

    def iterator = detachQueue.iterator ++ attachQueue.iterator
  }

  final def onReconnected() =
    freshReconnected = true

  final def enqueue(input: Input.QueueableInput): Unit = {
    queue.enqueue(input)
    if (queue.size == batchSize || freshReconnected) {
      maySend()
    }
  }

  final def maySend(): Unit =
    if (openRequestCount < OpenRequestsMaximum && (!freshReconnected || openRequestCount == 0)) {
      val inputs = queue.iterator.filterNot(executingInputs).take(batchSize).toVector
      if (inputs.nonEmpty) {
        executingInputs ++= inputs
        openRequestCount += 1
        executeCommand(Batch(inputs map inputToAgentCommand)) onComplete {
          case Success(Batch.Response(responses)) ⇒
            asyncOnBatchSucceeded(for ((i, r) ← inputs zip responses) yield QueuedInputResponse(i, r))

          case Failure(t) ⇒
            asyncOnBatchFailed(inputs, t)
        }
      }
    }

  private def inputToAgentCommand(input: Input.QueueableInput): AgentCommand =
    input match {
      case Input.AttachOrder(order, agentPath, workflow) ⇒
        AgentCommand.AttachOrder(order, agentPath, workflow)
      case Input.DetachOrder(orderId) ⇒
        AgentCommand.DetachOrder(orderId)
    }

  final def handleBatchSucceeded(responses: Seq[QueuedInputResponse]): Seq[Input.QueueableInput] = {
    freshReconnected = false
    val inputs = responses.map(_.input).toSet
    queue.dequeueAll(inputs)  // Including rejected commands. The corresponding orders are ignored henceforth.
    onQueuedInputsResponded(inputs)
    responses.flatMap {
      case QueuedInputResponse(input, Batch.Succeeded(Accepted)) ⇒
        Some(input)
      case QueuedInputResponse(input, Batch.Failed(message)) ⇒
        logger.error(s"Agent has rejected the command ${input.toShortString}: $message")
        // Agent's state does not match master's state ???
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
