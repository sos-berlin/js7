package js7.tests.agent

import cats.effect.unsafe.IORuntime
import js7.agent.data.commands.AgentCommand
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.deleteDirectoryContentRecursively
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.AgentResetProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentDedicated}
import js7.data.agent.Problems.AgentNotDedicatedProblem
import js7.data.controller.ControllerCommand.{CancelOrders, ResetAgent}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancelled, OrderDetached, OrderFailed, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.workflow.position.{InstructionNr, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.agent.ResetAgentWhenCancelingTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class ResetAgentWhenCancelingTest
  extends OurTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig =
    config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig =
    config"""
    js7.job.execution.signed-script-injection-allowed = on
    """.withFallback(super.agentConfig)

  protected def agentPaths = Seq(agentPath)
  protected def items = Nil

  "ResetAgent while an Order is being canceled and the Agent restarts" in:
    val workflow = updateItem(Workflow(WorkflowPath("WORKFLOW"), Seq(
      TestJob.execute(agentPath),
      TestJob.execute(agentPath))))
    val orderId = OrderId("CANCELING")
    controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    eventWatch.awaitNextKey[OrderStdoutWritten](orderId)

    val agentTerminated = agent.terminate(clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Failover)).unsafeToFuture()
    sleep(500.ms) // Give terminate some time to start !!!
    TestJob.continue()
    // May wait forever and fail when second job has been started while terminating !!!
    agentTerminated.await(99.s)

    execCmd:
      CancelOrders(Seq(orderId))

    // Delete Agent's journal
    deleteDirectoryContentRecursively(directoryProvider.agentEnvs.head.stateDir)

    val freshAgent = directoryProvider.startAgent(agentPath).await(99.s)
    eventWatch.awaitNext[AgentCouplingFailed](_.event.problem is AgentNotDedicatedProblem)

    execCmd:
      ResetAgent(agentPath)
    eventWatch.awaitNextKey[OrderTerminated](orderId)
    eventWatch.awaitNext[AgentDedicated]()

    // TODO 💥While resetting, the order may start the second job, but the test does not expect this
    assert(eventWatch.eventsByKey[OrderEvent](orderId)
      .filter(e => !e.isInstanceOf[OrderCancellationMarked]/*unreliable ordering*/)
      .filter(e => !e.isInstanceOf[OrderMoved]/*may occur after OrderProcessed*/)
      .map {
        case OrderFailed(Position(Nil, InstructionNr(1)), None) => OrderFailed(Position(0))
        case e => e
      } ==
      Seq(
        OrderAdded(workflow.id),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("TestJob\n"),
        OrderProcessed(OrderOutcome.Disrupted(AgentResetProblem(agentPath))),
        //OrderProcessed(OrderOutcome.succeeded), // Until v2.6 (Monix), only when non-parallel tested
        OrderDetached,
        OrderOutcomeAdded(OrderOutcome.Disrupted(AgentResetProblem(agentPath))),
        OrderFailed(Position(0)),
        OrderCancelled))

    freshAgent.terminate().await(99.s)


object ResetAgentWhenCancelingTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  final class TestJob extends SemaphoreJob(TestJob)
  object TestJob extends SemaphoreJob.Companion[TestJob]
