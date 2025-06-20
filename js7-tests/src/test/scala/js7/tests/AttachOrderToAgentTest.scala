package js7.tests

import js7.agent.scheduler.order.AgentOrderKeeper
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.AttachOrderToAgentTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest

final class AttachOrderToAgentTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    pekko.http.client.idle-timeout =
      ${AgentOrderKeeper.TestAttachOrderToAgentResponseTimeout.toMillis}ms
    pekko.scheduler.tick-duration =
      ${AgentOrderKeeper.TestAttachOrderToAgentResponseTimeout.toMillis / 2}ms # Pekko's timer accuracy
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected def agentPaths = Seq(agentPath)
  protected def items = Nil

  "JS-2207 AttachOrder without response" in:
    // With our OrderId the Agent delays the AttachOrder command,
    // elapsing pekko.http.client.idle-timeout.
    // This lets the DirectorDriver repeat the AttachOrder command.
    // But because the Agent has executed the AttachOrder command (but not responded),
    // the Controller will get an OrderDetachable event, and give an DetachOrder command.
    // This DetachOrderCommand removes the still queued AttachOrder command.
    // See AgentOrderKeeper, look for "AttachOrderToAgent"

    val workflow = Workflow.of:
      EmptyJob.execute(agentPath)

    withItem(workflow): workflow =>
      controller.runOrder:
        FreshOrder(OrderId("AttachOrderToAgentTest"), workflow.path, deleteWhenTerminated = true)


object AttachOrderToAgentTest:
  private val logger = Logger[this.type]

  private val agentPath = AgentPath("AGENT")
