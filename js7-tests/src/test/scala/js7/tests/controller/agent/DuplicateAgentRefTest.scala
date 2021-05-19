package js7.tests.controller.agent

import com.typesafe.config.ConfigUtil.quoteString
import js7.agent.data.Problems.AgentPathMismatchProblem
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentReady}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.job.RelativePathExecutable
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.test.ForkTestSetting.TestPathExecutable
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.agent.DuplicateAgentRefTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import org.scalatest.freespec.AnyFreeSpec

final class DuplicateAgentRefTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  protected val agentPaths = aAgentPath :: Nil
  protected val versionedItems = workflow :: Nil

  import controller.scheduler

  override def beforeAll() = {
    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
      "js7.auth.agents." + quoteString(bAgentPath.string) + " = " +
        quoteString(directoryProvider.agentToTree(aAgentPath).password.string) + "\n"
    for (a <- directoryProvider.agents) a.writeExecutable(TestPathExecutable, script(0.s))
    super.beforeAll()
  }

  "test" in {
    controller.eventWatch.await[AgentReady](_.key == aAgentPath)
    controllerApi.updateUnsignedSimpleItems(Seq(AgentRef(bAgentPath, agent.localUri))).await(99.s).orThrow

    val orderId = OrderId("ORDER")
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    val a = controller.eventWatch.await[AgentCouplingFailed]().head.value.event
    val b = AgentCouplingFailed(AgentPathMismatchProblem(bAgentPath, aAgentPath))
    assert(a == b)
  }
}

object DuplicateAgentRefTest
{
  private val aAgentPath = AgentPath("A-AGENT")
  private val bAgentPath = AgentPath("B-AGENT")
  private val workflow = Workflow.of(
    WorkflowPath("SINGLE") ~ "INITIAL",
    Execute(WorkflowJob(aAgentPath, RelativePathExecutable("executable.cmd"))),
    Execute(WorkflowJob(bAgentPath, RelativePathExecutable("executable.cmd"))))
}
