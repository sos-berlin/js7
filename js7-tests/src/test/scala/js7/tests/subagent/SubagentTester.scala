package js7.tests.subagent

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.tests.subagent.SubagentMultipleOrdersTest.agentPath
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler
import org.scalatest.Suite

trait SubagentTester extends ControllerAgentForScalaTest
{
  this: Suite =>

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms
    """.withFallback(super.controllerConfig)

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    js7.auth.subagents.BARE-SUBAGENT = "${toLocalSubagentId(agentPath)}'s PASSWORD"
    """.withFallback(super.agentConfig)

  protected val bareSubagentId = SubagentId("BARE-SUBAGENT")
  protected lazy val bareSubagentItem = SubagentItem(
    bareSubagentId,
    agentPath,
    findFreeLocalUri())

  protected val scheduler: Scheduler
}
