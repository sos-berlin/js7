package js7.tests.order

import cats.effect.unsafe.IORuntime
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.DeleteOrdersWhenTerminated
import js7.data.item.VersionId
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDeletionMarked, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.order.DeleteOrderWhenTerminatedTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.{script, toLocalSubagentId}

final class DeleteOrderWhenTerminatedTest extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(quickWorkflow)

  override def beforeAll() =
    for a <- directoryProvider.agentEnvs do
      a.writeExecutable(quickPathExecutable, script(1.s))
      a.writeExecutable(slowPathExecutable, script(1.s))
    super.beforeAll()

  "Delete a fresh order" in:
    val order = FreshOrder(OrderId("🟦"), quickWorkflow.id.path, scheduledFor = Some(Timestamp.now + 1.s))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    execCmd(DeleteOrdersWhenTerminated(Seq(order.id)))
    eventWatch.await[OrderDeleted](_.key == order.id)
    assert(eventWatch.eventsByKey[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(quickWorkflow.id, order.arguments, scheduledFor = order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderDeletionMarked,
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished(),
      OrderDeleted))

  "Delete a finished order" in:
    val order = FreshOrder(OrderId("🟧"), slowWorkflow.id.path)
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    execCmd(DeleteOrdersWhenTerminated(Seq(order.id)))
    eventWatch.await[OrderDeleted](_.key == order.id)
    assert(controller.eventWatch.eventsByKey[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(slowWorkflow.id, order.arguments, scheduledFor = order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderDeletionMarked,
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished(),
      OrderDeleted))


object DeleteOrderWhenTerminatedTest:
  private val quickPathExecutable = RelativePathExecutable("quick.cmd")
  private val slowPathExecutable = RelativePathExecutable("slow.cmd")
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val versionId = VersionId("INITIAL")

  private val quickWorkflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    Execute(WorkflowJob(agentPath, quickPathExecutable)))

  private val slowWorkflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    Execute(WorkflowJob(agentPath, slowPathExecutable)))
