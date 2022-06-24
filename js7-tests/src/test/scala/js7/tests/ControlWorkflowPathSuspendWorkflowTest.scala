package js7.tests

import cats.syntax.parallel._
import java.util.concurrent.TimeoutException
import js7.agent.RunningAgent
import js7.agent.data.event.AgentEvent.AgentReady
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand
import js7.data.event.EventId
import js7.data.item.BasicItemEvent.ItemDetached
import js7.data.item.ItemOperation.{AddVersion, RemoveVersioned}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.OrderEvent.{OrderAttached, OrderFinished, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.WorkflowPathControlEvent.{WorkflowPathControlAttached, WorkflowPathControlUpdated}
import js7.data.workflow.instructions.Prompt
import js7.data.workflow.{Workflow, WorkflowPath, WorkflowPathControl, WorkflowPathControlState}
import js7.proxy.ControllerApi
import js7.tests.ControlWorkflowPathSuspendWorkflowTest._
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class ControlWorkflowPathSuspendWorkflowTest
extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(aAgentPath, bAgentPath)
  protected val items = Seq(aWorkflow, bWorkflow)

  private var controller: RunningController = null
  private var aAgent: RunningAgent = null
  private var bAgent: RunningAgent = null

  private def eventWatch = controller.eventWatch

  override def afterAll() = {
    Seq(aAgent, bAgent)
      .flatMap(Option(_)).parTraverse(_.terminate()).await(99.s)
    controller.terminate(suppressSnapshot = true).await(99.s)
    super.afterAll()
  }

  "ControlWorkflowPath suspend=true" in {
    aAgent = directoryProvider.startAgent(aAgentPath).await(99.s)
    controller = directoryProvider.startController().await(99.s)
    implicit val controllerApi = directoryProvider.newControllerApi(controller)

    var eventId = suspendWorkflow(aWorkflow.path, true, ItemRevision(1))

    val aOrderId = OrderId("A")
    controllerApi.addOrder(FreshOrder(aOrderId, aWorkflow.path, deleteWhenTerminated = true))
      .await(99.s)
    intercept[TimeoutException](
      eventWatch.await[OrderStarted](_.key == aOrderId, after = eventId, timeout = 500.ms))

    eventId = suspendWorkflow(aWorkflow.path, false, ItemRevision(2))
    eventId = eventWatch.await[OrderPrompted](_.key == aOrderId, after = eventId).head.eventId

    def orderObstacles: Checked[Seq[(OrderId, Set[OrderObstacle])]] =
      new OrderObstacleCalculator(controller.controllerState.await(99.s))
        .ordersToObstacles(Seq(aOrderId), Timestamp.now)
        .map(_.toSeq)

    assert(orderObstacles == Right(Seq(
      aOrderId -> Set[OrderObstacle](OrderObstacle.WaitingForCommand))))

    eventId = suspendWorkflow(aWorkflow.path, true, ItemRevision(3))

    controllerApi.executeCommand(ControllerCommand.AnswerOrderPrompt(aOrderId)).await(99.s)
    // OrderPromptAnswered happens despite suspended Workflow
    eventId = eventWatch.await[OrderPromptAnswered](_.key == aOrderId, after = eventId)
      .head.eventId

    assert(orderObstacles == Right(Seq(
      aOrderId -> Set[OrderObstacle](OrderObstacle.WorkflowSuspended))))

    intercept[TimeoutException] {
      eventWatch.await[OrderAttached](_.key == aOrderId, after = eventId, timeout = 500.ms)
    }

    eventId = suspendWorkflow(aWorkflow.path, false, ItemRevision(4))

    eventId = eventWatch
      .await[OrderStdoutWritten](_ == aOrderId <-: OrderStdoutWritten("ASemaphoreJob\n"), after = eventId)
      .head.eventId

    eventId = suspendWorkflow(aWorkflow.path, true, ItemRevision(5))
    assert(eventWatch.await[WorkflowPathControlAttached](after = eventId).map(_.value) == Seq(
      aWorkflow.path <-: WorkflowPathControlAttached(aAgentPath, ItemRevision(5))))

    ASemaphoreJob.continue()
    intercept[TimeoutException] {
      eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId, timeout = 500.ms)
    }

    eventId = suspendWorkflow(aWorkflow.path, false, ItemRevision(6))
    eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)

    controller.controllerState.await(99.s).pathToWorkflowPathControlState_(aWorkflow.path) ==
      WorkflowPathControlState(
        WorkflowPathControl(
          aWorkflow.path,
          suspended = false,
          revision = ItemRevision(6)),
        attachedToAgents = Set(aAgentPath))

    suspendWorkflow(aWorkflow.path, true, ItemRevision(7))
    controllerApi.stop.await(99.s)
  }

  "After Controller recovery, the WorkflowPathControl is attached to the remaining Agents" in {
    val bOrderId = OrderId("B")
    bAgent = directoryProvider.startAgent(bAgentPath).await(99.s)
    var agentEventId = bAgent.eventWatch.await[AgentReady]().last.eventId

    locally {
      val eventWatch = controller.eventWatch
      val eventId = eventWatch.lastAddedEventId
      implicit val controllerApi = directoryProvider.newControllerApi(controller)
      controllerApi.addOrder(FreshOrder(bOrderId, bWorkflow.path, deleteWhenTerminated = true))
        .await(99.s)
      eventWatch
        .await[OrderStdoutWritten](_ == bOrderId <-: OrderStdoutWritten("B1SemaphoreJob\n"), after = eventId)

      val terminated = bAgent.terminate().runToFuture
      B1SemaphoreJob.continue()
      terminated.await(99.s)

      suspendWorkflow(bWorkflow.path, false, ItemRevision(1))
    }

    controller.terminate().await(99.s)

    bAgent = directoryProvider.startAgent(bAgentPath).await(99.s)
    bAgent.eventWatch.await[AgentReady](after = agentEventId)
    agentEventId = bAgent.eventWatch
      .await[OrderStdoutWritten](_ == bOrderId <-: OrderStdoutWritten("B2SemaphoreJob\n"), after = agentEventId)
      .head.eventId

    controller = directoryProvider.startController().await(99.s)
    implicit val controllerApi = directoryProvider.newControllerApi(controller)
    val eventWatch = controller.eventWatch
    val eventId = eventWatch.lastAddedEventId

    assert(
      eventWatch.await[WorkflowPathControlAttached](_.key == bWorkflow.path, after = eventId)
        .map(_.value) ==
        Seq(bWorkflow.path <-: WorkflowPathControlAttached(bAgentPath, ItemRevision(1))))

    suspendWorkflow(bWorkflow.path, false, ItemRevision(2))
    B2SemaphoreJob.continue()
    eventWatch.await[OrderFinished](_.key == bOrderId)

    controller.controllerState.await(99.s).pathToWorkflowPathControlState_(bWorkflow.path) ==
      WorkflowPathControlState(
        WorkflowPathControl(bWorkflow.path, suspended = true, revision = ItemRevision(1)),
        attachedToAgents = Set(bAgentPath))
  }

  "WorkflowPathControl disappears with the last Workflow version" in {
    val controllerApi = directoryProvider.newControllerApi(controller)
    val eventId = eventWatch.lastAddedEventId

    controllerApi
      .updateItems(Observable(
        AddVersion(VersionId("DELETE")),
        RemoveVersioned(aWorkflow.path),
        RemoveVersioned(bWorkflow.path)))
      .await(99.s)
    assert(eventWatch.await[ItemDetached](_.event.key == aWorkflow.id, after = eventId)
      .head.value.event
      == ItemDetached(aWorkflow.id, aAgentPath))
    assert(eventWatch.await[ItemDetached](_.event.key == bWorkflow.id, after = eventId)
      .head.value.event
      == ItemDetached(bWorkflow.id, bAgentPath))

    assert(aAgent.currentAgentState().pathToWorkflowPathControlState.isEmpty)
    assert(controller.controllerState.await(99.s).pathToWorkflowPathControlState.isEmpty)
  }

  private def suspendWorkflow(workflowPath: WorkflowPath, suspend: Boolean, revision: ItemRevision)
    (implicit controllerApi: ControllerApi)
  : EventId = {
    val eventId = eventWatch.lastAddedEventId
    controllerApi
      .executeCommand(ControllerCommand.ControlWorkflowPath(workflowPath, suspend = Some(suspend)))
      .await(99.s).orThrow
    val keyedEvents = eventWatch.await[WorkflowPathControlUpdated](after = eventId)
    assert(keyedEvents.map(_.value) == Seq(
      workflowPath <-: WorkflowPathControlUpdated(suspend, Set.empty, revision)))
    keyedEvents.last.eventId
  }
}

object ControlWorkflowPathSuspendWorkflowTest
{
  private val aAgentPath = AgentPath("A-AGENT")
  private val bAgentPath = AgentPath("B-AGENT")

  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ VersionId("INITIAL"), Seq(
    Prompt(expr("'PROMPT'")),
    ASemaphoreJob.execute(aAgentPath)))

  private val bWorkflow = Workflow(WorkflowPath("B-WORKFLOW") ~ VersionId("INITIAL"), Seq(
    B1SemaphoreJob.execute(bAgentPath),
    B2SemaphoreJob.execute(bAgentPath)))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]

  final class B1SemaphoreJob extends SemaphoreJob(B1SemaphoreJob)
  object B1SemaphoreJob extends SemaphoreJob.Companion[B1SemaphoreJob]

  final class B2SemaphoreJob extends SemaphoreJob(B2SemaphoreJob)
  object B2SemaphoreJob extends SemaphoreJob.Companion[B2SemaphoreJob]
}
