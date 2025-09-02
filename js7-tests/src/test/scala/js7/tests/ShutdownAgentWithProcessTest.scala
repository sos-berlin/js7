package js7.tests

import java.lang.System.lineSeparator as nl
import js7.agent.TestAgent
import js7.agent.client.AgentClient
import js7.agent.data.commands.AgentCommand
import js7.base.auth.Admission
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.useSync
import js7.base.utils.Base64UUID
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.{AgentReady, AgentShutDown}
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.event.{AnyKeyedEvent, Event, KeyedEvent}
import js7.data.item.VersionId
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderCancelled, OrderDetachable, OrderDetached, OrderMoved, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentDedicated, SubagentRestarted, SubagentShutdown, SubagentShutdownStarted}
import js7.data.subagent.{SubagentId, SubagentItemStateEvent, SubagentRunId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ShutdownAgentWithProcessTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import org.apache.pekko.actor.ActorSystem

final class ShutdownAgentWithProcessTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(simpleWorkflow)

  private given ActorSystem = controller.actorSystem

  override def beforeAll() =
    super.beforeAll()
    controller.awaitNext[AgentReady]()
    controller.awaitNextKey[SubagentDedicated](subagentId)

  "JS-2025 AgentCommand.Shutdown with SIGKILL job process, then recovery" in:
    val addOrderEventId = controller.lastAddedEventId

    val simpleOrderId = OrderId("🔹")
    controller.addOrderBlocking(FreshOrder(OrderId("🔹"), simpleWorkflow.path))
    controller.awaitNextKey[OrderProcessingStarted](simpleOrderId).head.eventId
    controller.awaitNextKey[OrderStdoutWritten](simpleOrderId)

    val agentEnv = directoryProvider.agentEnvs.head
    locally:
      val agentClient = AgentClient(Admission(agent.localUri, agentEnv.controllerUserAndPassword))
      agentClient.login().await(99.s)
      agentClient.commandExecute:
        AgentCommand.ShutDown(Some(SIGKILL))
      .await(99.s)
      agent.untilTerminated.await(99.s)

    controller.awaitNextKey[SubagentShutdownStarted](subagentId)
    val orderProcessed = controller.awaitNextKey[OrderProcessed](simpleOrderId)
    assert(orderProcessed.map(_.value) == Vector(OrderProcessed(OrderOutcome.processKilledRestartable(SIGKILL))))
    controller.awaitNextKey[SubagentShutdown](subagentId)
    controller.awaitNextKey[AgentShutDown](agentPath)

    agentEnv.testAgentResource.useSync(99.s): restartedAgent =>
      controller.awaitNextKey[OrderProcessingStarted](simpleOrderId)
      controller.awaitNextKey[OrderStdoutWritten](simpleOrderId)
      execCmd:
        CancelOrders(simpleOrderId :: Nil, CancellationMode.kill(immediately = true))
      controller.awaitNextKey[OrderTerminated](simpleOrderId)

      assert(controller.keyedEvents[Event](after = addOrderEventId)
        .flatMap(manipulateEvent(_, simpleOrderId)) == Seq(
        simpleOrderId <-: OrderAdded(simpleWorkflow.id),
        simpleOrderId <-: OrderAttachable(agentPath),
        simpleOrderId <-: OrderAttached(agentPath),

        simpleOrderId <-: OrderStarted,
        simpleOrderId <-: OrderProcessingStarted(subagentId),
        simpleOrderId <-: OrderStdoutWritten(s"TestJob$nl"),

        subagentId <-: SubagentShutdownStarted,
        simpleOrderId <-: OrderProcessed(OrderOutcome.processKilledRestartable(SIGKILL)),
        simpleOrderId <-: OrderMoved(Position(0)),
        subagentId <-: SubagentShutdown,
        agentPath <-: AgentShutDown,

        subagentId <-: SubagentCoupled,
        subagentId <-: SubagentRestarted,
        subagentId <-: SubagentDedicated(SubagentRunId(Base64UUID.zero), None),

        agentPath <-: AgentReady("UTC", None),
        simpleOrderId <-: OrderProcessingStarted(subagentId),
        simpleOrderId <-: OrderStdoutWritten(s"TestJob$nl"),
        simpleOrderId <-: OrderCancellationMarked(CancellationMode.kill(immediately = true)),
        simpleOrderId <-: OrderCancellationMarkedOnAgent,
        simpleOrderId <-: OrderProcessed(OrderOutcome.killed(SIGKILL)),
        simpleOrderId <-: OrderProcessingKilled,
        simpleOrderId <-: OrderDetachable,
        simpleOrderId <-: OrderDetached,
        simpleOrderId <-: OrderCancelled))

  private def manipulateEvent(keyedEvent: AnyKeyedEvent, orderId: OrderId): Option[AnyKeyedEvent] =
    keyedEvent match
      case KeyedEvent(`orderId`, _) => Some(keyedEvent)
      case KeyedEvent(_, _: AgentShutDown) => Some(keyedEvent)
      case KeyedEvent(agentPath: AgentPath, _: AgentReady) =>
        Some(agentPath <-: AgentReady("UTC", None))
      case KeyedEvent(subagentId: SubagentId, _: SubagentDedicated) =>
        Some(subagentId <-: SubagentDedicated(SubagentRunId(Base64UUID.zero), None))
      case KeyedEvent(_, _: SubagentItemStateEvent) => Some(keyedEvent)
      case _ => None


object ShutdownAgentWithProcessTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val versionId = VersionId("INITIAL")

  private val simpleWorkflow = Workflow.of(
    WorkflowPath("SIMPLE") ~ versionId,
    Execute(WorkflowJob(
      agentPath,
      ShellScriptExecutable:
        (isWindows ?? "@echo off\n") +
          "echo TestJob\n" +
          "sleep 99\n")))
