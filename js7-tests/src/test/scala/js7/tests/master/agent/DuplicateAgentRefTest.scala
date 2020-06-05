package js7.tests.master.agent

import com.typesafe.config.ConfigUtil.quoteString
import js7.agent.data.Problems.DuplicateAgentRef
import js7.base.time.ScalaTime._
import js7.common.scalautil.FileUtils.syntax._
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.job.ExecutablePath
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.test.ForkTestSetting.TestExecutablePath
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.master.data.events.MasterAgentEvent.{AgentCouplingFailed, AgentReady}
import js7.tests.master.agent.DuplicateAgentRefTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.MasterAgentForScalaTest
import org.scalatest.freespec.AnyFreeSpec

final class DuplicateAgentRefTest extends AnyFreeSpec with MasterAgentForScalaTest
{
  protected val agentRefPaths = aAgentRefPath :: Nil
  protected val fileBased = workflow :: Nil

  import master.scheduler

  override def beforeAll() = {
    (directoryProvider.master.configDir / "private" / "private.conf") ++=
      "js7.auth.agents." + quoteString(bAgentRefPath.string) + " = " +
        quoteString(directoryProvider.agentToTree(aAgentRefPath).password.string) + "\n"
    for (a <- directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(0.s))
    super.beforeAll()
  }

  "test" in {
    master.eventWatch.await[AgentReady](_.key == aAgentRefPath)
    updateRepo(AgentRef(bAgentRefPath, agent.localUri) :: Nil)

    val orderId = OrderId("ORDER")
    master.addOrderBlocking(FreshOrder(orderId, workflow.path))
    val a = master.eventWatch.await[AgentCouplingFailed]().head.value.event
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
