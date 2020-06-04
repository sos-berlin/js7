package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.data.Problems.{CancelStartedOrderProblem, UnknownOrderProblem}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCancellationMarked, OrderCancelled, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.TestExecutablePath
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.MasterCommand.{Batch, CancelOrder, Response}
import com.sos.jobscheduler.tests.CancelOrderTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class CancelOrderTest extends AnyFreeSpec with MasterAgentForScalaTest
{
  protected val agentRefPaths = TestAgentRefPath :: Nil
  protected val fileBased = SingleJobWorkflow :: TwoJobsWorkflow :: Nil

  override def beforeAll() = {
    for (a <- directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(2.s))
    super.beforeAll()
  }

  "Cancel a fresh order" in {
    val order = FreshOrder(OrderId("🔹"), SingleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderTransferredToAgent](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.NotStarted)).await(99.seconds).orThrow
    master.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(SingleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderCancellationMarked(CancelMode.NotStarted),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderCancelled))
  }

  "Cancel a finishing order" in {
    val order = FreshOrder(OrderId("🔺"), SingleJobWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted)).await(99.seconds).orThrow
    master.eventWatch.await[OrderFinished](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(SingleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancellationMarked(CancelMode.FreshOrStarted),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished))
  }

  "Cancelling (mode=NotStarted) a started order is not possible" in {
    val order = FreshOrder(OrderId("❌"), TwoJobsWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    sleep(100.ms)  // MasterOrderKeeper may take some time to update its state
    // Master knows, the order has started
    assert(master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.NotStarted)).await(99.seconds) ==
      Left(CancelStartedOrderProblem(OrderId("❌"))))
  }

  "Cancel a started order between two jobs" in {
    val order = FreshOrder(OrderId("🔴"), TwoJobsWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted)).await(99.seconds).orThrow
    master.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(TwoJobsWorkflow.id, order.scheduledFor),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancellationMarked(CancelMode.FreshOrStarted),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderCancelled))
  }

  "Cancel unknown order" in {
    assert(master.executeCommandAsSystemUser(CancelOrder(OrderId("UNKNOWN"), CancelMode.NotStarted)).await(99.seconds) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))
  }

  "Cancel multiple orders with Batch" in {
    val orders = for (i <- 1 to 3) yield FreshOrder(OrderId(i.toString), SingleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    for (o <- orders) master.addOrderBlocking(o)
    for (o <- orders) master.eventWatch.await[OrderTransferredToAgent](_.key == o.id)
    val response = master.executeCommandAsSystemUser(Batch(for (o <- orders) yield CancelOrder(o.id, CancelMode.NotStarted))).await(99.seconds).orThrow
    assert(response == Batch.Response(Vector.fill(orders.length)(Right(Response.Accepted))))
    for (o <- orders) master.eventWatch.await[OrderCancelled](_.key == o.id)
  }
}

object CancelOrderTest {
  private val TestExecutablePath = ExecutablePath("/executable.cmd")
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val SingleJobWorkflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ "INITIAL",
    Execute(WorkflowJob(TestAgentRefPath, TestExecutablePath)))
  private val TwoJobsWorkflow = Workflow.of(
    WorkflowPath("/TWO") ~ "INITIAL",
    Execute(WorkflowJob(TestAgentRefPath, TestExecutablePath)),
    Execute(WorkflowJob(TestAgentRefPath, TestExecutablePath)))
}
