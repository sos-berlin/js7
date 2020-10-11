package js7.tests.order

import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand.RemoveOrdersWhenTerminated
import js7.data.agent.AgentName
import js7.data.item.VersionId
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderRemoveMarked, OrderRemoved, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.order.RemoveOrderWhenTerminatedTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

final class RemoveOrderWhenTerminatedTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentNames = agentName :: Nil
  protected val inventoryItems = quickWorkflow:: Nil

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(quickExecutablePath, script(1.s))
      a.writeExecutable(slowExecutablePath, script(1.s))
    }
    super.beforeAll()
  }

  "Remove a fresh order" in {
    val order = FreshOrder(OrderId("🔵"), quickWorkflow.id.path, scheduledFor = Some(Timestamp.now + 1.seconds))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(RemoveOrdersWhenTerminated(Seq(order.id))).await(99.seconds).orThrow
    controller.eventWatch.await[OrderRemoved](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(quickWorkflow.id, order.scheduledFor),
      OrderAttachable(agentName),
      OrderAttached(agentName),
      OrderStarted,
      OrderProcessingStarted,
      OrderRemoveMarked,
      OrderStdoutWritten("TEST ☘\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished,
      OrderRemoved))
  }

  "Remove a finished order" in {
    val order = FreshOrder(OrderId("🔴"), slowWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(RemoveOrdersWhenTerminated(Seq(order.id))).await(99.seconds).orThrow
    controller.eventWatch.await[OrderRemoved](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(slowWorkflow.id, order.scheduledFor),
      OrderAttachable(agentName),
      OrderAttached(agentName),
      OrderStarted,
      OrderProcessingStarted,
      OrderRemoveMarked,
      OrderStdoutWritten("TEST ☘\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished,
      OrderRemoved))
  }
}

object RemoveOrderWhenTerminatedTest
{
  private val quickExecutablePath = ExecutablePath("/quick.cmd")
  private val slowExecutablePath = ExecutablePath("/slow.cmd")
  private val agentName = AgentName("AGENT")
  private val versionId = VersionId("INITIAL")

  private val quickWorkflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ versionId,
    Execute(WorkflowJob(agentName, quickExecutablePath)))

  private val slowWorkflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ versionId,
    Execute(WorkflowJob(agentName, slowExecutablePath)))
}
