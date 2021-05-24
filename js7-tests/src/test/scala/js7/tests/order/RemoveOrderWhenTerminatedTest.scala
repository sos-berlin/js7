package js7.tests.order

import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.RemoveOrdersWhenTerminated
import js7.data.item.VersionId
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderRemoveMarked, OrderRemoved, OrderStarted, OrderStdWritten}
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

final class RemoveOrderWhenTerminatedTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(quickWorkflow)

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(quickPathExecutable, script(1.s))
      a.writeExecutable(slowPathExecutable, script(1.s))
    }
    super.beforeAll()
  }

  "Remove a fresh order" in {
    val order = FreshOrder(OrderId("🔵"), quickWorkflow.id.path, scheduledFor = Some(Timestamp.now + 1.s))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(RemoveOrdersWhenTerminated(Seq(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderRemoved](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(quickWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderRemoveMarked,
      OrderProcessed(Outcome.succeededRC0),
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
    controller.executeCommandAsSystemUser(RemoveOrdersWhenTerminated(Seq(order.id))).await(99.s).orThrow
    controller.eventWatch.await[OrderRemoved](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(slowWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderRemoveMarked,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished,
      OrderRemoved))
  }
}

object RemoveOrderWhenTerminatedTest
{
  private val quickPathExecutable = RelativePathExecutable("quick.cmd")
  private val slowPathExecutable = RelativePathExecutable("slow.cmd")
  private val agentPath = AgentPath("AGENT")
  private val versionId = VersionId("INITIAL")

  private val quickWorkflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    Execute(WorkflowJob(agentPath, quickPathExecutable)))

  private val slowWorkflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    Execute(WorkflowJob(agentPath, slowPathExecutable)))
}
