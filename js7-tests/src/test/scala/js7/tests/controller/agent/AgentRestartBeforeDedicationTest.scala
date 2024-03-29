package js7.tests.controller.agent

import cats.effect.unsafe.IORuntime
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentDedicated
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.agent.AgentRestartBeforeDedicationTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProviderForScalaTest

final class AgentRestartBeforeDedicationTest extends OurTestSuite, DirectoryProviderForScalaTest:

  private given IORuntime = ioRuntime

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(workflow)

  "Restart Agent before Controller initializes it" in:
    directoryProvider.runAgents() { _ => }
    directoryProvider.runAgents() { _ => }
    directoryProvider.runAgents() { _ =>
      sleep(100.ms)
      directoryProvider.runController() { controller =>
        controller.eventWatch.await[AgentDedicated](_.key == agentPath,
          after = controller.eventWatch.tornEventId)
        val events = controller.runOrder(FreshOrder(OrderId("ORDER"), workflow.path))
        assert(events.last.value == OrderFinished())
      }
    }


object AgentRestartBeforeDedicationTest:
  private val agentPath = AgentPath("AGENT")
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath)))
