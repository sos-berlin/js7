package js7.tests

import js7.agent.RunningAgent
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentShutDown}
import js7.data.event.{Event, KeyedEvent}
import js7.data.item.VersionId
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NumberValue
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
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
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  private implicit def actorSystem = controller.actorSystem

  "JS-2025 AgentCommand.Shutdown with SIGKILL job process, then recovery" in {
    eventWatch.await[AgentCoupled]()
    val eventId = eventWatch.lastAddedEventId

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

    assert(eventWatch.keyedEvents[Event](after = eventId)
      .filter {
        case KeyedEvent(_, _: OrderEvent) => true
        case KeyedEvent(_, _: AgentShutDown) => true
        case KeyedEvent(_, _: AgentCoupled) => true
        case _ => false
      } == Seq(
      orderId <-: OrderAdded(workflow.id),
      orderId <-: OrderAttachable(agentPath),
      orderId <-: OrderAttached(agentPath),
      orderId <-: OrderStarted,
      orderId <-: OrderProcessingStarted(subagentId),
      orderId <-: OrderStdoutWritten("TestJob\n"),
      orderId <-: OrderProcessed(Outcome.Killed(Outcome.Failed(namedValues = Map(
        "returnCode" -> NumberValue(137))))),
      agentPath <-: AgentShutDown,
      agentPath <-: AgentCoupled,
      orderId <-: OrderProcessingKilled,
      orderId <-: OrderDetachable,
      orderId <-: OrderDetached,
      orderId <-: OrderFailed(Position(0))))

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
    Execute(WorkflowJob.apply(
      agentPath,
      ShellScriptExecutable(
        (isWindows ?? "@echo off\n") +
          "echo TestJob\n" +
          "sleep 99\n"))))
}
