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

  private implicit def actorSystem: ActorSystem =
    controller.actorSystem

  override def beforeAll() =
    super.beforeAll()
    eventWatch.awaitNext[AgentReady]()

  "JS-2025 AgentCommand.Shutdown with SIGKILL job process, then recovery" in:
    val addOrderEventId = eventWatch.lastAddedEventId

    val simpleOrderId = OrderId("🔹")
    controller.addOrderBlocking(FreshOrder(OrderId("🔹"), simpleWorkflow.path))
    eventWatch.awaitNextKey[OrderProcessingStarted](simpleOrderId).head.eventId
    eventWatch.awaitNextKey[OrderStdoutWritten](simpleOrderId)

    val agentEnv = directoryProvider.agentEnvs.head
    locally:
      val agentClient = AgentClient(Admission(agent.localUri, agentEnv.controllerUserAndPassword))
      agentClient.login().await(99.s)
      agentClient
        .commandExecute(AgentCommand.ShutDown(processSignal = Some(SIGKILL)))
        .await(99.s)
      agent.untilTerminated.await(99.s)

    agentEnv.testAgentResource.useSync(99.s): restartedAgent =>
      val stampedEvents = eventWatch.awaitNextKey[OrderProcessed](simpleOrderId)
      assert(stampedEvents.map(_.value) == Vector(OrderProcessed(OrderOutcome.processKilledRestartable(SIGKILL))))

      eventWatch.awaitNextKey[OrderStdoutWritten](simpleOrderId)
      execCmd:
        CancelOrders(simpleOrderId :: Nil, CancellationMode.kill(immediately = true))
      eventWatch.awaitNextKey[OrderTerminated](simpleOrderId)

      assert(eventWatch.keyedEvents[Event](after = addOrderEventId)
        .flatMap(manipulateEvent(_, simpleOrderId)) == Seq(
        simpleOrderId <-: OrderAdded(simpleWorkflow.id),
        simpleOrderId <-: OrderAttachable(agentPath),
        simpleOrderId <-: OrderAttached(agentPath),

        simpleOrderId <-: OrderStarted,
        simpleOrderId <-: OrderProcessingStarted(subagentId),
        simpleOrderId <-: OrderStdoutWritten(s"TestJob$nl"),
        simpleOrderId <-: OrderProcessed(OrderOutcome.processKilledRestartable(SIGKILL)),
        agentPath <-: AgentShutDown,

        agentPath <-: AgentReady("UTC", None),
        simpleOrderId <-: OrderMoved(Position(0)),
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
      case o @ KeyedEvent(`orderId`, _) => Some(o)
      case o @ KeyedEvent(_, _: AgentShutDown) => Some(o)
      case KeyedEvent(agentPath: AgentPath, _: AgentReady) =>
        Some(agentPath <-: AgentReady("UTC", None))
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
