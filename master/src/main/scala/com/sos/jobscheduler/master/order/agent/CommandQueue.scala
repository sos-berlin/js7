package com.sos.jobscheduler.master.order.agent

import akka.actor.ActorRef
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Accepted, Batch}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.master.order.agent.AgentDriver.Input
import com.sos.jobscheduler.master.order.agent.CommandQueue._
import com.typesafe.scalalogging.{Logger ⇒ ScalaLogger}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
private class CommandQueue(client: AgentClient, self: ActorRef, logger: ScalaLogger, batchSize: Int)(implicit ec: ExecutionContext) {

  private val executingInputs = mutable.Set[Input.QueueableInput]()
  private var freshReconnected = true  // After connect, send a single command first. Start queueing first after one successful response.
  private var openRequestCount = 0

  private object queue {
    private val attachQueue = mutable.Queue[Input.QueueableInput]()
    private val detachQueue = mutable.Queue[Input.QueueableInput]()  // DetachOrder is sent to Agent prior any AttachOrder, to relieve the Agent

    def enqueue(sender: ActorRef, input: Input.QueueableInput): Unit = {
      input match {
        case o: Input.AttachOrder ⇒ attachQueue += o
        case o: Input.DetachOrder ⇒ detachQueue += o
      }
    }

    def dequeueAll(what: Set[Input.QueueableInput]): Unit = {
      attachQueue.dequeueAll(what)
      detachQueue.dequeueAll(what)
    }

    def size = attachQueue.size + detachQueue.size

    def iterator = detachQueue.iterator ++ attachQueue.iterator
  }

  def onReconnected() =
    freshReconnected = true

  def enqueue(sender: ActorRef, input: Input.QueueableInput): Unit = {
    queue.enqueue(sender, input)
    if (queue.size  == batchSize || freshReconnected) {
      maySend()
    }
  }

  def maySend(): Unit =
    if (openRequestCount < OpenRequestsMaximum && (!freshReconnected || openRequestCount == 0)) {
      val inputs = queue.iterator.filterNot(executingInputs).take(batchSize).toVector
      if (inputs.nonEmpty) {
        executingInputs ++= inputs
        openRequestCount += 1
        client.executeCommand(Batch(inputs map inputToAgentCommand)) onComplete {
          case Success(Batch.Response(responses)) ⇒
            self ! Message.BatchResponse(for ((input, o) ← inputs zip responses) yield Message.QueuedInputResponse(input, o))
          case Failure(t) ⇒
            self ! Message.BatchFailed(inputs, t)
        }
      }
    }

  private def inputToAgentCommand(input: Input.QueueableInput): AgentCommand =
    input match {
      case Input.AttachOrder(order, jobnet) ⇒
        AgentCommand.AttachOrder(order, jobnet)
      case Input.DetachOrder(orderId) ⇒
        AgentCommand.DetachOrder(orderId)
    }

  def handleBatchResponse(batchResponse: Message.BatchResponse): Set[OrderId] = {
    freshReconnected = false
    val inputs = batchResponse.responses.map(_.input).toSet
    queue.dequeueAll(inputs)  // Including rejected commands. The corresponding orders are ignored henceforth.
    onQueuedInputsResponded(inputs)
    batchResponse.responses.flatMap {
      case Message.QueuedInputResponse(input, Batch.Succeeded(Accepted)) ⇒
        Some(input.orderId)
      case Message.QueuedInputResponse(input, Batch.Failed(message)) ⇒
        logger.error(s"Agent has rejected the command ${input.toShortString}: $message")
        // Agent's state does not match master's state ???
        None
    }.toSet
  }

  def handleBatchFailed(failed: Message.BatchFailed): Unit = {
    // Don't remove from inputQueue. Queued inputs will be processed again
    onQueuedInputsResponded(failed.inputs.toSet)
  }

  private def onQueuedInputsResponded(inputs: Set[Input.QueueableInput]): Unit = {
    executingInputs --= inputs
    openRequestCount -= 1
    maySend()
  }
}

object CommandQueue {
  private val OpenRequestsMaximum = 2

  private[agent] final case class QueuedInput(sender: ActorRef, input: Input.QueueableInput)

  private[agent] object Message {
    final case class BatchResponse(responses: Seq[QueuedInputResponse])
    final case class QueuedInputResponse(input: Input.QueueableInput, response: Batch.SingleResponse)
    final case class BatchFailed(inputs: Seq[Input.QueueableInput], throwable: Throwable)
  }
}
