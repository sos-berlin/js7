package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCancelationMarked, OrderCanceled, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.TestExecutablePath
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.MasterCommand.CancelOrder
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

  "Cancel fresh order" in {
    val order = FreshOrder(OrderId("🔹"), SingleJobWorkflow.id.path, scheduledAt = Some(now + 99.seconds))
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderTransferredToAgent](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id)).await(99.seconds).orThrow
    master.eventWatch.await[OrderCanceled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(SingleJobWorkflow.id, order.scheduledAt),
      OrderAttachable(TestAgentPath),
      OrderTransferredToAgent(TestAgentPath % "(initial)"),
      OrderCancelationMarked,
      OrderDetachable,
      OrderTransferredToMaster,
      OrderCanceled))
  }

  "Canceling finishing order" in {
    val order = FreshOrder(OrderId("🔺"), SingleJobWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id)).await(99.seconds).orThrow
    master.eventWatch.await[OrderFinished](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(SingleJobWorkflow.id, order.scheduledAt),
      OrderAttachable(TestAgentPath),
      OrderTransferredToAgent(TestAgentPath % "(initial)"),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancelationMarked,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished))
  }

  "Canceling started order between two jobs" in {
    val order = FreshOrder(OrderId("🔴"), TwoJobsWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id)).await(99.seconds).orThrow
    master.eventWatch.await[OrderCanceled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(TwoJobsWorkflow.id, order.scheduledAt),
      OrderAttachable(TestAgentPath),
      OrderTransferredToAgent(TestAgentPath % "(initial)"),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancelationMarked,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderCanceled))
  }
}

object CancelOrderTest {
  private val TestExecutablePath = ExecutablePath("/executable")
  private val TestAgentPath = AgentPath("/AGENT")
  private val SingleJobWorkflow = Workflow.of(
    WorkflowPath("/SINGLE") % "(initial)",
    Execute(WorkflowJob(TestAgentPath, TestExecutablePath)))
  private val TwoJobsWorkflow = Workflow.of(
    WorkflowPath("/TWO") % "(initial)",
    Execute(WorkflowJob(TestAgentPath, TestExecutablePath)),
    Execute(WorkflowJob(TestAgentPath, TestExecutablePath)))
}
