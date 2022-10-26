package js7.tests

import js7.agent.RunningAgent
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ShutdownAgentWithProcessTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.traced

final class ShutdownAgentWithProcessTest extends OurTestSuite with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  private implicit def actorSystem = controller.actorSystem

  "JS-2025 AgentCommand.Shutdown with SIGKILL job process, then recovery" in {
    val orderId = OrderId("🔹")
    controller.addOrderBlocking(FreshOrder(OrderId("🔹"), workflow.path))
    eventWatch.await[OrderProcessingStarted](_.key == orderId)

    val agentTree = directoryProvider.agents.head
    val agentClient = AgentClient(agentUri = agent.localUri, agentTree.userAndPassword)
    agentClient.login() await 99.s

    agentClient
      .commandExecute(AgentCommand.ShutDown(processSignal = Some(SIGKILL)))
      .await(99.s)
    agent.terminated.await(99.s)

    val restartedAgent = RunningAgent.startForTest(agentTree.agentConfiguration)
      .await(10.s)
    eventWatch.await[OrderFailed](_.key == orderId)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.Killed(Outcome.Failed(Some("Canceled")))),
      OrderProcessingKilled,
      OrderDetachable,
      OrderDetached,
      OrderFailed(Position(0))))

    restartedAgent.terminate().await(99.s)
  }
}

object ShutdownAgentWithProcessTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val versionId = VersionId("INITIAL")

  final class TestJob extends SemaphoreJob(TestJob)
  object TestJob extends SemaphoreJob.Companion[TestJob]

  private val workflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    TestJob.execute(agentPath))
}
