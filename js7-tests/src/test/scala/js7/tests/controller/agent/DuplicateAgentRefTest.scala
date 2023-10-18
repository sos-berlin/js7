package js7.tests.controller.agent

import com.typesafe.config.ConfigUtil.quoteString
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentReady}
import js7.data.agent.Problems.AgentPathMismatchProblem
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.tests.controller.agent.DuplicateAgentRefTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.traced

final class DuplicateAgentRefTest extends OurTestSuite with ControllerAgentForScalaTest:
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  protected val agentPaths = aAgentPath :: Nil
  protected val items = Nil

  override def beforeAll() =
    (directoryProvider.controllerEnv.privateConf) ++=
      "js7.auth.agents." + quoteString(bAgentPath.string) + " = " +
        quoteString(directoryProvider.agentToEnv(aAgentPath).controllerPassword.string) + "\n"
    super.beforeAll()

  "Add a second Subagent with same URI" in:
    pending // TODO

  "Add a second AgentRef/SubagentItem with same URI" in:
    controller.eventWatch.await[AgentReady](_.key == aAgentPath)
    val subagentId = SubagentId(bAgentPath.string + "-0")
    controller.api.updateUnsignedSimpleItems(Seq(
      AgentRef(bAgentPath, Seq(subagentId)),
      SubagentItem(subagentId, bAgentPath, agent.localUri)
    )).await(99.s).orThrow

    val a = controller.eventWatch.await[AgentCouplingFailed]().head.value.event
    assert(a == AgentCouplingFailed(AgentPathMismatchProblem(bAgentPath, aAgentPath)))


object DuplicateAgentRefTest:
  private val aAgentPath = AgentPath("A-AGENT")
  private val bAgentPath = AgentPath("B-AGENT")
