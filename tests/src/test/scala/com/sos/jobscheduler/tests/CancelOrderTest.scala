package com.sos.jobscheduler.tests

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.message.ProblemCodeMessages.problemCodeToString
import com.sos.jobscheduler.core.message.ProblemCodes
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCancelationMarked, OrderCanceled, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.TestExecutablePath
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.MasterCommand.{Batch, CancelOrder, Response}
import com.sos.jobscheduler.tests.CancelOrderTest._
import com.sos.jobscheduler.tests.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class CancelOrderTest extends FreeSpec with DirectoryProvider.ForScalaTest
{
  protected val agentPaths = TestAgentPath :: Nil

  override def beforeAll() = {
    directoryProvider.master.writeJson(SingleJobWorkflow.withoutVersion)
    directoryProvider.master.writeJson(TwoJobsWorkflow.withoutVersion)
    for (a ← directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(1.s))
    super.beforeAll()
  }

  "Cancel a fresh order" in {
    val order = FreshOrder(OrderId("🔹"), SingleJobWorkflow.id.path, scheduledAt = Some(now + 99.seconds))
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderTransferredToAgent](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.NotStarted)).await(99.seconds).orThrow
    master.eventWatch.await[OrderCanceled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(SingleJobWorkflow.id, order.scheduledAt),
      OrderAttachable(TestAgentPath),
      OrderTransferredToAgent(TestAgentPath % "(initial)"),
      OrderCancelationMarked(CancelMode.NotStarted),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderCanceled))
  }

  "Cancel a finishing order" in {
    val order = FreshOrder(OrderId("🔺"), SingleJobWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted)).await(99.seconds).orThrow
    master.eventWatch.await[OrderFinished](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(SingleJobWorkflow.id, order.scheduledAt),
      OrderAttachable(TestAgentPath),
      OrderTransferredToAgent(TestAgentPath % "(initial)"),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancelationMarked(CancelMode.FreshOrStarted),
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished))
  }

  "Canceling (mode=NotStarted) a started order is not possible" in {
    val order = FreshOrder(OrderId("❌"), TwoJobsWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    // Master knows the order has started
    assert(master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.NotStarted)).await(99.seconds) ==
      Invalid(Problem(ProblemCodes.CancelStartedOrder, "❌")))
  }

  "Cancel a started order between two jobs" in {
    val order = FreshOrder(OrderId("🔴"), TwoJobsWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted)).await(99.seconds).orThrow
    master.eventWatch.await[OrderCanceled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(TwoJobsWorkflow.id, order.scheduledAt),
      OrderAttachable(TestAgentPath),
      OrderTransferredToAgent(TestAgentPath % "(initial)"),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancelationMarked(CancelMode.FreshOrStarted),
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderCanceled))
  }

  "Cancel unknown order" in {
    assert(master.executeCommandAsSystemUser(CancelOrder(OrderId("UNKNOWN"), CancelMode.NotStarted)).await(99.seconds) ==
      Invalid(Problem(ProblemCodes.UnknownOrder, "UNKNOWN")))
  }

  "Cancel multiple orders with Batch" in {
    val orders = for (i ← 1 to 3) yield FreshOrder(OrderId(i.toString), SingleJobWorkflow.id.path, scheduledAt = Some(now + 99.seconds))
    for (o ← orders) master.addOrderBlocking(o)
    for (o ← orders) master.eventWatch.await[OrderTransferredToAgent](_.key == o.id)
    val response = master.executeCommandAsSystemUser(Batch(for (o ← orders) yield CancelOrder(o.id, CancelMode.NotStarted))).await(99.seconds).orThrow
    assert(response == Batch.Response(Vector.fill(orders.length)(Valid(Response.Accepted))))
    for (o ← orders) master.eventWatch.await[OrderCanceled](_.key == o.id)
  }
}

object CancelOrderTest {
  private val TestExecutablePath = ExecutablePath("/executable.cmd")
  private val TestAgentPath = AgentPath("/AGENT")
  private val SingleJobWorkflow = Workflow.of(
    WorkflowPath("/SINGLE") % "(initial)",
    Execute(WorkflowJob(TestAgentPath, TestExecutablePath)))
  private val TwoJobsWorkflow = Workflow.of(
    WorkflowPath("/TWO") % "(initial)",
    Execute(WorkflowJob(TestAgentPath, TestExecutablePath)),
    Execute(WorkflowJob(TestAgentPath, TestExecutablePath)))
}
