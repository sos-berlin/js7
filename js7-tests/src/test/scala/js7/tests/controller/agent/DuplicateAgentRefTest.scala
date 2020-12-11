package js7.tests.controller.agent

import com.typesafe.config.ConfigUtil.quoteString
import js7.agent.data.Problems.DuplicateAgentRef
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand.UpdateAgentRefs
import js7.controller.data.events.AgentRefStateEvent.{AgentCouplingFailed, AgentReady}
import js7.data.agent.{AgentId, AgentRef}
import js7.data.job.RelativeExecutablePath
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.test.ForkTestSetting.TestExecutablePath
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.agent.DuplicateAgentRefTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import org.scalatest.freespec.AnyFreeSpec

final class DuplicateAgentRefTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = aAgentId :: Nil
  protected val versionedItems = workflow :: Nil

  import controller.scheduler

  override def beforeAll() = {
    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
      "js7.auth.agents." + quoteString(bAgentId.string) + " = " +
        quoteString(directoryProvider.agentToTree(aAgentId).password.string) + "\n"
    for (a <- directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(0.s))
    super.beforeAll()
  }

  "test" in {
    controller.eventWatch.await[AgentReady](_.key == aAgentId)
    controller.executeCommandAsSystemUser(UpdateAgentRefs(Seq(AgentRef(bAgentId, agent.localUri))))
      .await(99.s).orThrow

    val orderId = OrderId("ORDER")
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    val a = controller.eventWatch.await[AgentCouplingFailed]().head.value.event
    val b = AgentCouplingFailed(DuplicateAgentRef(first = aAgentId, second = bAgentId))
    assert(a == b)
  }
}

object DuplicateAgentRefTest
{
  private val aAgentId = AgentId("A-AGENT")
  private val bAgentId = AgentId("B-AGENT")
  private val workflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ "INITIAL",
    Execute(WorkflowJob(aAgentId, RelativeExecutablePath("executable.cmd"))),
    Execute(WorkflowJob(bAgentId, RelativeExecutablePath("executable.cmd"))))
}
