package js7.tests.order

import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.common.configutils.Configs._
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand.RemoveOrdersWhenTerminated
import js7.data.agent.AgentId
import js7.data.item.VersionId
import js7.data.job.RelativeExecutablePath
import js7.data.order.OrderEvent.{OrderFinished, OrderRemoved, OrderStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.order.RemoveOrderWhenTerminatedDelayedTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class RemoveOrderWhenTerminatedDelayedTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = agentId :: Nil
  protected val versionedItems = workflow:: Nil
  override protected def controllerConfig = config"""js7.order.remove-delay = 1s"""
    .withFallback(super.controllerConfig)

  override def beforeAll() = {
    for (a <- directoryProvider.agents) a.writeExecutable(executablePath, script(1.s))
    super.beforeAll()
  }

  "OrderRemoved is delayed" in {
    val order = FreshOrder(OrderId("🔴"), workflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(RemoveOrdersWhenTerminated(Seq(order.id))).await(99.s).orThrow
    val finished = controller.eventWatch.await[OrderFinished](_.key == order.id).head
    val removed = controller.eventWatch.await[OrderRemoved](_.key == order.id).head
    assert(removed.timestamp - finished.timestamp > 500.ms)
  }
}

object RemoveOrderWhenTerminatedDelayedTest
{
  private val executablePath = RelativeExecutablePath("quick.cmd")
  private val agentId = AgentId("AGENT")
  private val versionId = VersionId("INITIAL")

  private val workflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ versionId,
    Execute(WorkflowJob(agentId, executablePath)))
}
