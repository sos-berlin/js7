package com.sos.jobscheduler.master.agent

import cats.data.Validated.Valid
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Batch
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.core.crypt.silly.{SillySignature, SillySigner}
import com.sos.jobscheduler.core.filebased.FileBasedSigner
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.crypt.Signed
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.agent.AgentDriver.{Input, Queueable}
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
    val commandQueue = new CommandQueue(logger, batchSize = 3) {
      val succeeded = mutable.Buffer[Seq[QueuedInputResponse]]()
      val failed = mutable.Buffer[(Vector[Queueable], Throwable)]()

      protected def executeCommand(command: AgentCommand.Batch) =
        Task(Valid(Batch.Response(Vector.fill(command.commands.size)(Valid(AgentCommand.Response.Accepted)))))

      protected def asyncOnBatchSucceeded(queuedInputResponses: Seq[QueuedInputResponse]) =
        succeeded += queuedInputResponses

      protected def asyncOnBatchFailed(inputs: Vector[Queueable], throwable: Throwable) =
        failed += ((inputs, throwable))
    }

    val expected = mutable.Buffer[Seq[QueuedInputResponse]]()

    commandQueue.onRecoupled()

    // The first Input is sent alone to the Agent regardless of batchSize.
    val aOrder = toOrder("A")
    commandQueue.enqueue(AgentDriver.Input.AttachOrder(aOrder, TestAgentRefPath % "(initial)", signedWorkflow))
    expected += toQueuedInputResponse(aOrder) :: Nil
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    val twoOrders = toOrder("B") :: toOrder("C") :: Nil
    for (o ← twoOrders) commandQueue.enqueue(AgentDriver.Input.AttachOrder(o, TestAgentRefPath % "(initial)", signedWorkflow))
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    // After the Agent has processed the Input, the two queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueue.succeeded.last) shouldEqual List(Input.AttachOrder(aOrder, TestAgentRefPath % "(initial)", signedWorkflow))
    expected += twoOrders map toQueuedInputResponse
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    val fiveOrders = toOrder("D") :: toOrder("E") :: toOrder("F") :: toOrder("G") :: toOrder("H") :: Nil
    for (o ← fiveOrders) commandQueue.enqueue(AgentDriver.Input.AttachOrder(o, TestAgentRefPath % "(initial)", signedWorkflow))
    expected += fiveOrders take 1 map toQueuedInputResponse
    waitForCondition(9.s, 10.ms) { commandQueue.succeeded == expected }
    assert(commandQueue.succeeded == expected)

    // After the Agent has processed the Input, three of the queued commands are sent as a Batch to the Agent
    commandQueue.handleBatchSucceeded(commandQueue.succeeded.last) shouldEqual fiveOrders.take(1).map(o ⇒ Input.AttachOrder(o, TestAgentRefPath % "(initial)", signedWorkflow))
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
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val TestWorkflow = Workflow.of(WorkflowPath("/A") % "VERSION",
    Execute(WorkflowJob(TestAgentRefPath, ExecutablePath("/EXECUTABLE"))))
  private val fileBasedSigner = new FileBasedSigner(new SillySigner(SillySignature("MY-SILLY-SIGNATURE")), Workflow.jsonEncoder)
  private val signedWorkflow: Signed[Workflow] = fileBasedSigner.toSigned(TestWorkflow)

  private def toQueuedInputResponse(order: Order[Order.FreshOrReady]) =
    QueuedInputResponse(AgentDriver.Input.AttachOrder(order, TestAgentRefPath % "(initial)", signedWorkflow), Valid(AgentCommand.Response.Accepted))

  private def toOrder(name: String) = Order(OrderId(name), TestWorkflow.id, Order.Fresh.StartImmediately)
}
