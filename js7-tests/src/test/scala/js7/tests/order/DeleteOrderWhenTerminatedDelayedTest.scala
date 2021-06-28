package js7.tests.order

import js7.base.configutils.Configs._
import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.DeleteOrdersWhenTerminated
import js7.data.item.VersionId
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderDeleted, OrderFinished, OrderStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.order.DeleteOrderWhenTerminatedDelayedTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class DeleteOrderWhenTerminatedDelayedTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = agentPath :: Nil
  protected val items = workflow:: Nil
  override protected def controllerConfig = config"""js7.order.delete-delay = 1s"""
    .withFallback(super.controllerConfig)

  override def beforeAll() = {
    for (a <- directoryProvider.agents) a.writeExecutable(pathExecutable, script(1.s))
    super.beforeAll()
  }

  "OrderDeleted is delayed" in {
    val order = FreshOrder(OrderId("🔴"), workflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(DeleteOrdersWhenTerminated(Seq(order.id))).await(99.s).orThrow
    val finished = controller.eventWatch.await[OrderFinished](_.key == order.id).head
    val removed = controller.eventWatch.await[OrderDeleted](_.key == order.id).head
    assert(removed.timestamp - finished.timestamp > 500.ms)
  }
}

object DeleteOrderWhenTerminatedDelayedTest
{
  private val pathExecutable = RelativePathExecutable("quick.cmd")
  private val agentPath = AgentPath("AGENT")
  private val versionId = VersionId("INITIAL")

  private val workflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    Execute(WorkflowJob(agentPath, pathExecutable)))
}