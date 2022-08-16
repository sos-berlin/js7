package js7.tests

import cats.syntax.parallel.*
import js7.agent.RunningAgent
import js7.agent.data.event.AgentEvent.AgentReady
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.ResumeOrder
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemDetached}
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.ItemOperation.{AddVersion, RemoveVersioned}
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemChanged}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.OrderEvent.{OrderFinished, OrderStdoutWritten, OrderSuspended}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowPath}
import js7.journal.watch.StrictEventWatch
import js7.tests.ControlWorkflowBreakpointTest.setBreakpoints
import js7.tests.ControlWorkflowRecoveryTest.*
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable

final class ControlWorkflowRecoveryTest
extends OurTestSuite with DirectoryProviderForScalaTest
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

  private implicit def eventWatch: StrictEventWatch =
    controller.eventWatch

  override def afterAll() = {
    Seq(aAgent, bAgent)
      .flatMap(Option(_))
      .parTraverse(_.terminate())
      .await(99.s)
    controller.terminate(suppressSnapshot = true).await(99.s)
    super.afterAll()
  }

  "ControlWorkflow sets some breakpoints" in {
    controller = directoryProvider.startController().await(99.s)
    implicit val controllerApi = directoryProvider.newControllerApi(controller)

    aAgent = directoryProvider.startAgent(aAgentPath).await(99.s)

    val eventId = setBreakpoints(
      aWorkflow.id,
      Map(
        Position(3) -> true),
      resultBreakpoints = Set(
        Position(3)),
      revision = ItemRevision(0),
      UnsignedItemAdded.apply)

    val aOrderId = OrderId("A")
    controllerApi.addOrder(FreshOrder(aOrderId, aWorkflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    // Breakpoint at Position(3) reached
    eventWatch.await[OrderSuspended](_.key == aOrderId, after = eventId)

    controllerApi.executeCommand(ResumeOrder(aOrderId)).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)

    aAgent.terminate().await(99.s)

    setBreakpoints(
      aWorkflow.id,
      Map(
        Position(1) -> true,
        Position(3) -> true),
      Set(Position(1), Position(3)),
      ItemRevision(1),
      UnsignedItemChanged.apply)

    controllerApi.stop.await(99.s)
  }

  "After Agent recouples, the attachable WorkflowControl is attached" in {
    def controllerState = controller.controllerState.await(99.s)
    val eventId = eventWatch.lastAddedEventId
    aAgent = directoryProvider.startAgent(aAgentPath).await(99.s)

    eventWatch.await[ItemAttached](
      _.event.key == aWorkflowControlId,
      after = eventId)

    assert(controllerState.itemToIgnorantAgents(WorkflowControl).toMap.isEmpty)
  }

  "After Controller recovery, the WorkflowControl is attached to the remaining Agents" in {
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

      setBreakpoints(
        bWorkflow.id,
        Map(Position(1) -> true),
        Set(Position(1)),
        ItemRevision(0),
        UnsignedItemAdded.apply)
      eventWatch.await[ItemAttachable](
        _.event.key == bWorkflowControlId,
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
        .await[ItemAttached](_.event.key == bWorkflowControlId, after = eventId)
        .map(_.value) ==
        Seq(NoKey <-: ItemAttached(bWorkflowControlId, Some(ItemRevision(0)), bAgentPath)))

    setBreakpoints(
      bWorkflow.id,
      Map(Position(1) -> false),
      Set(),
      ItemRevision(1),
      UnsignedItemChanged.apply)
    B2SemaphoreJob.continue()
    eventWatch.await[OrderFinished](_.key == bOrderId)

    controller.controllerState.await(99.s)
      .keyTo(WorkflowControl)(bWorkflowControlId) ==
        WorkflowControl(
          bWorkflowControlId,
          breakpoints = Set(Position(1)),
          itemRevision = Some(ItemRevision(1)))
    controller.controllerState.await(99.s)
      .itemToAgentToAttachedState(bWorkflowControlId) ==
      Map(bAgentPath -> Attached(Some(ItemRevision(1))))
  }

  "WorkflowControl disappears with the Workflow" in {
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
      _.event.key == aWorkflowControlId,
      after = eventId
    ).head.value.event == ItemDetached(aWorkflowControlId, aAgentPath))

    assert(eventWatch.await[ItemDetached](_.event.key == bWorkflow.id, after = eventId)
      .head.value.event == ItemDetached(bWorkflow.id, bAgentPath))

    assert(eventWatch.await[ItemDetached](
      _.event.key == bWorkflowControlId,
      after = eventId
    ).head.value.event == ItemDetached(bWorkflowControlId, bAgentPath))

    assert(bAgent.currentAgentState().keyTo(WorkflowControl).isEmpty)
    // Controller has implicitly deleted WorkflowControl
    assert(controller.controllerState.await(99.s).keyTo(WorkflowControl).isEmpty)
  }
}

object ControlWorkflowRecoveryTest
{
  private val aAgentPath = AgentPath("A-AGENT")
  private val bAgentPath = AgentPath("B-AGENT")

  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ "INITIAL", Seq(
    EmptyJob.execute(aAgentPath),
    EmptyJob.execute(aAgentPath),
    EmptyJob.execute(aAgentPath),
    EmptyJob.execute(aAgentPath)))

  private val bWorkflow = Workflow(WorkflowPath("B-WORKFLOW") ~ "INITIAL", Seq(
    B1SemaphoreJob.execute(bAgentPath),
    B2SemaphoreJob.execute(bAgentPath)))

  private val aWorkflowControlId = WorkflowControlId(aWorkflow.id)
  private val bWorkflowControlId = WorkflowControlId(bWorkflow.id)

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]

  final class B1SemaphoreJob extends SemaphoreJob(B1SemaphoreJob)
  object B1SemaphoreJob extends SemaphoreJob.Companion[B1SemaphoreJob]

  final class B2SemaphoreJob extends SemaphoreJob(B2SemaphoreJob)
  object B2SemaphoreJob extends SemaphoreJob.Companion[B2SemaphoreJob]
}
