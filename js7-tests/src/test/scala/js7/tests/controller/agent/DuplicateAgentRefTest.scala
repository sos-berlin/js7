package js7.tests.controller.agent

import com.typesafe.config.ConfigUtil.quoteString
import js7.agent.data.Problems.DuplicateAgentRef
import js7.base.time.ScalaTime._
import js7.common.scalautil.FileUtils.syntax._
import js7.controller.data.events.ControllerAgentEvent.{AgentCouplingFailed, AgentReady}
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.job.ExecutablePath
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
  protected val agentRefPaths = aAgentRefPath :: Nil
  protected val fileBased = workflow :: Nil

  import controller.scheduler

  override def beforeAll() = {
    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
      "js7.auth.agents." + quoteString(bAgentRefPath.string) + " = " +
        quoteString(directoryProvider.agentToTree(aAgentRefPath).password.string) + "\n"
    for (a <- directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(0.s))
    super.beforeAll()
  }

  "test" in {
    controller.eventWatch.await[AgentReady](_.key == aAgentRefPath)
    updateRepo(AgentRef(bAgentRefPath, agent.localUri) :: Nil)

    val orderId = OrderId("ORDER")
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    val a = controller.eventWatch.await[AgentCouplingFailed]().head.value.event
    val b = AgentCouplingFailed(DuplicateAgentRef(first = aAgentRefPath, second = bAgentRefPath))
    assert(a == b)
  }
}

object DuplicateAgentRefTest
{
  private val aAgentRefPath = AgentRefPath("/A-AGENT")
  private val bAgentRefPath = AgentRefPath("/B-AGENT")
  private val workflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ "INITIAL",
    Execute(WorkflowJob(aAgentRefPath, ExecutablePath("/executable.cmd"))),
    Execute(WorkflowJob(bAgentRefPath, ExecutablePath("/executable.cmd"))))
}
