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
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, ControlWorkflowPath}
import js7.data.event.EventId
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemDetached}
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.ItemOperation.{AddVersion, RemoveVersioned}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemAddedOrChanged, UnsignedSimpleItemChanged}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.OrderEvent.{OrderAttached, OrderFinished, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Prompt
import js7.data.workflow.{Workflow, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
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
    js7.controller.agent-driver.command-error-delay = 1s
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
    controller = directoryProvider.startController().await(99.s)
    def controllerState = controller.controllerState.await(99.s)
    implicit val controllerApi = directoryProvider.newControllerApi(controller)

    var eventId = suspendWorkflow(aWorkflow.path, true, ItemRevision(1),
      UnsignedSimpleItemAdded.apply)

    assert(controllerState.workflowPathControlToIgnorantAgents.isEmpty)
    assert(controllerState
      .itemToIgnorantAgents(WorkflowPathControl).get(WorkflowPathControlPath(aWorkflow.path)) == None)

    val aOrderId = OrderId("A")
    controllerApi.addOrder(FreshOrder(aOrderId, aWorkflow.path, deleteWhenTerminated = true))
      .await(99.s)
    intercept[TimeoutException](
      eventWatch.await[OrderStarted](_.key == aOrderId, after = eventId, timeout = 500.ms))

    eventId = suspendWorkflow(aWorkflow.path, false, ItemRevision(2),
      UnsignedSimpleItemChanged.apply)
    eventId = eventWatch.await[OrderPrompted](_.key == aOrderId, after = eventId).head.eventId

    def orderObstacles: Checked[Seq[(OrderId, Set[OrderObstacle])]] =
      new OrderObstacleCalculator(controllerState)
        .ordersToObstacles(Seq(aOrderId), Timestamp.now)
        .map(_.toSeq)

    waitForCondition(10.s, 10.ms)(orderObstacles.exists(_.nonEmpty))
    assert(orderObstacles == Right(Seq(
      aOrderId -> Set[OrderObstacle](OrderObstacle.WaitingForCommand))))

    eventId = suspendWorkflow(aWorkflow.path, true, ItemRevision(3),
      UnsignedSimpleItemChanged.apply)

    controllerApi.executeCommand(AnswerOrderPrompt(aOrderId)).await(99.s)
    // OrderPromptAnswered happens despite suspended Workflow
    eventId = eventWatch.await[OrderPromptAnswered](_.key == aOrderId, after = eventId)
      .head.eventId

    assert(orderObstacles == Right(Seq(
      aOrderId -> Set[OrderObstacle](OrderObstacle.WorkflowSuspended))))

    aAgent = directoryProvider.startAgent(aAgentPath).await(99.s)
    intercept[TimeoutException] {
      eventWatch.await[OrderAttached](_.key == aOrderId, after = eventId, timeout = 500.ms)
    }

    eventId = suspendWorkflow(aWorkflow.path, false, ItemRevision(4),
      UnsignedSimpleItemChanged.apply)

    eventId = eventWatch
      .await[OrderStdoutWritten](_ == aOrderId <-: OrderStdoutWritten("ASemaphoreJob\n"), after = eventId)
      .head.eventId

    eventId = suspendWorkflow(aWorkflow.path, true, ItemRevision(5),
      UnsignedSimpleItemChanged.apply)
    assert(eventWatch.await[ItemAttached](after = eventId).map(_.value) == Seq(
      NoKey <-:
        ItemAttached(WorkflowPathControlPath(aWorkflow.path), Some(ItemRevision(5)), aAgentPath)))

    ASemaphoreJob.continue()
    intercept[TimeoutException] {
      eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId, timeout = 500.ms)
    }

    eventId = suspendWorkflow(aWorkflow.path, false, ItemRevision(6),
      UnsignedSimpleItemChanged.apply)
    eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)

    assert(controller.controllerState.await(99.s)
      .keyTo(WorkflowPathControl)(WorkflowPathControlPath(aWorkflow.path)) ==
      WorkflowPathControl(
        WorkflowPathControlPath(aWorkflow.path),
        suspended = false,
        itemRevision = Some(ItemRevision(6))))

    assert(controller.controllerState.await(99.s)
      .itemToAgentToAttachedState(WorkflowPathControlPath(aWorkflow.path)) ==
      Map(aAgentPath -> Attached(Some(ItemRevision(6)))))

    aAgent.terminate().await(99.s)

    eventId = suspendWorkflow(aWorkflow.path, true, ItemRevision(7),
      UnsignedSimpleItemChanged.apply)
    eventWatch.await[ItemAttachable](
      _.event.key == WorkflowPathControlPath(aWorkflow.path),
      after = eventId)

    assert(controllerState.workflowPathControlToIgnorantAgents.toMap == Map(
      WorkflowPathControlPath(aWorkflow.path) -> Set(aAgentPath)))
    assert(controllerState
      .itemToIgnorantAgents(WorkflowPathControl)(WorkflowPathControlPath(aWorkflow.path)) ==
      Set(aAgentPath))

    controllerApi.stop.await(99.s)
  }

  "After Agent recouples, the attachable WorkflowPathControl is attached" in {
    def controllerState = controller.controllerState.await(99.s)
    val eventId = eventWatch.lastAddedEventId
    aAgent = directoryProvider.startAgent(aAgentPath).await(99.s)

    eventWatch.await[ItemAttached](
      _.event.key == WorkflowPathControlPath(aWorkflow.path),
      after = eventId)

    assert(controllerState.workflowPathControlToIgnorantAgents.toMap.isEmpty)
    assert(controllerState
      .itemToIgnorantAgents(WorkflowPathControl).get(WorkflowPathControlPath(aWorkflow.path)) == None)
  }

  "After Controller recovery, the WorkflowPathControl is attached to the remaining Agents" in {
    bAgent = directoryProvider.startAgent(bAgentPath).await(99.s)
    var agentEventId = bAgent.eventWatch.await[AgentReady]().last.eventId
    var eventId = eventWatch.lastAddedEventId
    val bOrderId = OrderId("B")

    locally {
      implicit val controllerApi = directoryProvider.newControllerApi(controller)
      controllerApi.addOrder(FreshOrder(bOrderId, bWorkflow.path, deleteWhenTerminated = true))
        .await(99.s)
      eventWatch.await[OrderStdoutWritten](
        _ == bOrderId <-: OrderStdoutWritten("B1SemaphoreJob\n"),
        after = eventId)

      val terminated = bAgent.terminate().runToFuture
      sleep(500.ms)  // Wait until AgentCommand.ShutDown takes effect
      B1SemaphoreJob.continue()
      terminated.await(99.s)

      suspendWorkflow(bWorkflow.path, true, ItemRevision(1),
        UnsignedSimpleItemAdded.apply)
      eventWatch.await[ItemAttachable](
        _.event.key == WorkflowPathControlPath(bWorkflow.path),
        after = eventId)

      controllerApi.stop.await(99.s)
    }

    controller.terminate().await(99.s)

    bAgent = directoryProvider.startAgent(bAgentPath).await(99.s)
    agentEventId = bAgent.eventWatch.await[AgentReady](after = agentEventId).head.eventId
    agentEventId = bAgent.eventWatch
      .await[OrderStdoutWritten](
        _ == bOrderId <-: OrderStdoutWritten("B2SemaphoreJob\n"),
        after = agentEventId)
      .head.eventId

    controller = directoryProvider.startController().await(99.s)
    implicit val controllerApi = directoryProvider.newControllerApi(controller)
    eventId = eventWatch.lastFileEventId

    // Events at recovery
    assert(
      eventWatch
        .await[ItemAttached](_.event.key == WorkflowPathControlPath(bWorkflow.path), after = eventId)
        .map(_.value) ==
        Seq(NoKey <-: ItemAttached(WorkflowPathControlPath(bWorkflow.path), Some(ItemRevision(1)), bAgentPath)))

    suspendWorkflow(bWorkflow.path, false, ItemRevision(2), UnsignedSimpleItemChanged.apply)
    B2SemaphoreJob.continue()
    eventWatch.await[OrderFinished](_.key == bOrderId)

    controller.controllerState.await(99.s)
      .keyTo(WorkflowPathControl)(WorkflowPathControlPath(bWorkflow.path)) ==
        WorkflowPathControl(
          WorkflowPathControlPath(bWorkflow.path),
          suspended = true,
          itemRevision = Some(ItemRevision(1)))
    controller.controllerState.await(99.s)
      .itemToAgentToAttachedState(WorkflowPathControlPath(bWorkflow.path)) ==
      Map(bAgentPath -> Attached(Some(ItemRevision(1))))
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
      .head.value.event == ItemDetached(aWorkflow.id, aAgentPath))

    assert(eventWatch.await[ItemDetached](
      _.event.key == WorkflowPathControlPath(aWorkflow.path),
      after = eventId
    ).head.value.event == ItemDetached(WorkflowPathControlPath(aWorkflow.path), aAgentPath))

    assert(eventWatch.await[ItemDetached](_.event.key == bWorkflow.id, after = eventId)
      .head.value.event == ItemDetached(bWorkflow.id, bAgentPath))

    assert(eventWatch.await[ItemDetached](
      _.event.key == WorkflowPathControlPath(bWorkflow.path),
      after = eventId
    ).head.value.event == ItemDetached(WorkflowPathControlPath(bWorkflow.path), bAgentPath))

    assert(bAgent.currentAgentState().keyTo(WorkflowPathControl).isEmpty)
    // Controller has implicitly deleted WorkflowPathControl
    assert(controller.controllerState.await(99.s).keyTo(WorkflowPathControl).isEmpty)
  }

  private def suspendWorkflow(workflowPath: WorkflowPath, suspend: Boolean, revision: ItemRevision,
    workflowPathControlToEvent: WorkflowPathControl => UnsignedSimpleItemAddedOrChanged)
    (implicit controllerApi: ControllerApi)
  : EventId = {
    val eventId = eventWatch.lastAddedEventId
    controllerApi
      .executeCommand(ControlWorkflowPath(workflowPath, suspend = Some(suspend)))
      .await(99.s).orThrow
    val keyedEvents = eventWatch.await[UnsignedSimpleItemAddedOrChanged](after = eventId)
    assert(keyedEvents.map(_.value) == Seq(
      NoKey <-: workflowPathControlToEvent(
        WorkflowPathControl(WorkflowPathControlPath(workflowPath),
          suspended = suspend,
          itemRevision = Some(revision)))))
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
