package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{Accepted, Batch}
import com.sos.jobscheduler.common.scalautil.Futures.SynchronousExecutionContext
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.agent.AgentDriver.Input
import com.sos.jobscheduler.master.agent.CommandQueue.QueuedInputResponse
import com.sos.jobscheduler.master.agent.CommandQueueTest._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.language.reflectiveCalls

/**
  * @author Joacim Zschimmer
  */
final class CommandQueueTest extends FreeSpec {

  "test" in {
    implicit def ec = SynchronousExecutionContext
    val commandQueue = new CommandQueue(logger, batchSize = 3) {
      val succeeded = mutable.Buffer[Seq[QueuedInputResponse]]()
      val failed = mutable.Buffer[(Vector[Input.QueueableInput], Throwable)]()

      protected def executeCommand(command: AgentCommand.Batch) =
        Task.pure(Batch.Response(Vector.fill(command.commands.size)(Batch.Succeeded(Accepted))))

      protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
        succeeded += queuedInputResponses

      protected def asyncOnBatchFailed(inputs: Vector[Input.QueueableInput], throwable: Throwable) =
        failed += ((inputs, throwable))
    }

    val expected = mutable.Buffer[Seq[QueuedInputResponse]]()

    commandQueue.onReconnected()

    // The first Input is sent alone to the Agent regardless of batchSize.
    val aOrder = toOrder("A")
    commandQueue.enqueue(AgentDriver.Input.AttachOrder(aOrder, TestAgentPath % "(initial)", TestWorkflow))
    expected += toQueuedInputResponse(aOrder) :: Nil
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    val twoOrders = toOrder("B") :: toOrder("C") :: Nil
    for (o ← twoOrders) commandQueue.enqueue(AgentDriver.Input.AttachOrder(o, TestAgentPath % "(initial)", TestWorkflow))
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    // After the Agent has processed the Input, the two queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueue.succeeded.last) shouldEqual List(Input.AttachOrder(aOrder, TestAgentPath % "(initial)", TestWorkflow))
    expected += twoOrders map toQueuedInputResponse
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    val fiveOrders = toOrder("D") :: toOrder("E") :: toOrder("F") :: toOrder("G") :: toOrder("H") :: Nil
    for (o ← fiveOrders) commandQueue.enqueue(AgentDriver.Input.AttachOrder(o, TestAgentPath % "(initial)", TestWorkflow))
    expected += fiveOrders take 1 map toQueuedInputResponse
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    // After the Agent has processed the Input, three of the queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueue.succeeded.last) shouldEqual fiveOrders.take(1).map(o ⇒ Input.AttachOrder(o, TestAgentPath % "(initial)", TestWorkflow))
    expected += fiveOrders drop 1 take 3 map toQueuedInputResponse
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    // Finally, the last queued Input is processed
    commandQueue.handleBatchSucceeded(commandQueue.succeeded.last)
    expected += fiveOrders drop 4 map toQueuedInputResponse
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)
    assert(commandQueue.failed.isEmpty)
  }
}

object CommandQueueTest {
  private val logger = Logger(getClass)
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestWorkflow = Workflow.of(WorkflowPath("/A") % "VERSION",
    Execute(WorkflowJob(TestAgentPath, ExecutablePath("/EXECUTABLE"))))

  private def toQueuedInputResponse(order: Order[Order.Idle]) =
    QueuedInputResponse(AgentDriver.Input.AttachOrder(order, TestAgentPath % "(initial)", TestWorkflow), Batch.Succeeded(Accepted))

  private def toOrder(name: String) = Order(OrderId(name), TestWorkflow.id, Order.Fresh.StartImmediately)
}
