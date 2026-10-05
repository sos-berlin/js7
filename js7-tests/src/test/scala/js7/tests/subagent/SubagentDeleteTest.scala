package js7.tests.subagent

import fs2.Stream
import java.util.concurrent.TimeoutException
import js7.agent.TestAgent
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentRefStateEvent.AgentReady
import js7.data.controller.ControllerCommand.DeleteOrdersWhenTerminated
import js7.data.item.BasicItemEvent.{ItemDeleted, ItemDetachable, ItemDetached}
import js7.data.item.ItemOperation.{AddOrChangeSimple, DeleteSimple}
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.OrderOutcome.Disrupted
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.subagent.Problems.ProcessLostDueSubagentDeletedProblem
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.SubagentDeleteTest.*
import js7.tests.subagent.SubagentTester.agentPath

final class SubagentDeleteTest extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val primarySubagentsDisabled = true

  private var myAgent: TestAgent = null.asInstanceOf[TestAgent]

  override def beforeAll() =
    super.beforeAll()
    myAgent = agent

  override def afterAll() =
    try
      myAgent.terminate().await(99.s)
    finally
      super.afterAll()

  "Delete bare Subagent" in:
    runSubagent(bareSubagentItem) { _ =>
      val eventId = controller.resetLastWatchedEventId()
      controller.api
        .updateItems(Stream(DeleteSimple(bareSubagentItem.id)))
        .await(99.s)
        .orThrow
      controller.awaitNext[ItemDetachable](_.event.key == bareSubagentItem.id)
      controller.awaitNext[ItemDeleted](_.event.key == bareSubagentItem.id)
    }

  "Delete Subagent while an Order is processed; Deletion does not allow more starts" in:
    val aOrderId = OrderId("REMOVE-SUBAGENT-A")
    val bOrderId = OrderId("REMOVE-SUBAGENT-B")
    var detachableEventId = controller.resetLastWatchedEventId()

    controller.api
      .updateItems(Stream(AddOrChangeSimple(bareSubagentItem)))
      .await(99.s).orThrow
    runSubagent(bareSubagentItem) { _ =>
      locally:
        controller.addOrderBlocking(FreshOrder(aOrderId, workflow.path))
        val started = controller.awaitNextKey[OrderProcessingStarted](aOrderId)
          .head.value
        assert(started == OrderProcessingStarted(bareSubagentItem.path))
        controller.awaitNextKey[OrderStdoutWritten](aOrderId)
        // aOrderId waits for semaphore

      detachableEventId = controller.lastAddedEventId
      controller.api
        .updateItems(Stream(DeleteSimple(bareSubagentItem.path)))
        .await(99.s).orThrow
      detachableEventId =
        controller.awaitNext[ItemDetachable](_.event.key == bareSubagentItem.id).head.eventId

      // ItemDetached is delayed until no Order is being processed
      intercept[TimeoutException]:
        controller.awaitNext[ItemDetached](_.event.key == bareSubagentItem.id, timeout = 1.s)

      // Don't allow orders to start while SubagentItem is deleted
      val eventId = controller.lastAddedEventId
      controller.addOrderBlocking(FreshOrder(bOrderId, workflow.path))
      intercept[TimeoutException]:
        controller.awaitKey[OrderProcessingStarted](bOrderId, timeout = 1.s)

      // Continue aOrderId
      TestSemaphoreJob.continue()
      controller.awaitNextKey[OrderProcessed](aOrderId, after = eventId)
      controller.awaitNextKey[OrderFinished](aOrderId)
    }

    // Subagent is being removed
    controller.awaitNext[ItemDetached](_.event.key == bareSubagentItem.id, after = detachableEventId)
    controller.awaitNext[ItemDeleted](_.event.key == bareSubagentItem.id)

    // START SUBAGENT AGAIN

    controller.api
      .updateItems(Stream(AddOrChangeSimple(bareSubagentItem)))
      .await(99.s).orThrow

    runSubagent(bareSubagentItem) { _ =>
      // Continue bOrderId
      TestSemaphoreJob.continue()
      val started = controller.awaitNextKey[OrderProcessingStarted](bOrderId)
        .head.value
      assert(started == OrderProcessingStarted(bareSubagentItem.path))

      controller.awaitNextKey[OrderProcessed](bOrderId)
      controller.awaitNextKey[OrderFinished](bOrderId)
    }

  "Delete Subagent and continue deletion after Director's restart" in:
    runSubagent(bareSubagentItem) { _ =>
      val orderId = OrderId("REMOVE-SUBAGENT-WHILE-RESTARTING-DIRECTOR")

      locally:
        controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
        val started = controller.awaitNextKey[OrderProcessingStarted](orderId).head.value
        assert(started == OrderProcessingStarted(bareSubagentItem.path))
        controller.awaitNextKey[OrderStdoutWritten](orderId)
        // orderId waits for semaphore //

      controller.api
        .updateItems(Stream(DeleteSimple(bareSubagentItem.path)))
        .await(99.s).orThrow
      controller.awaitNext[ItemDetachable](_.event.key == bareSubagentItem.id)

      // ItemDetached is delayed until no Order is being processed //
      intercept[TimeoutException]:
        controller.awaitNext[ItemDetached](_.event.key == bareSubagentItem.id, timeout = 1.s)

      // Changing a Subagent is rejected while it is being deleted //
      locally:
        val checked = controller.api
          .updateItems(Stream(AddOrChangeSimple(bareSubagentItem.copy(disabled = true))))
          .await(99.s)
        assert(checked == Left(Problem(
          "Subagent:BARE-SUBAGENT is marked as deleted and cannot be changed")))

      // Deleting a Subagent is ignored while it is being deleted //
      controller.api
        .updateItems(Stream(DeleteSimple(bareSubagentId)))
        .await(99.s)
        .orThrow

      // RESTART DIRECTOR //
      myAgent.terminate().await(99.s)

      val eventId = controller.lastAddedEventId
      myAgent = directoryProvider.startAgent(agentPath).await(99.s)
      controller.awaitNext[AgentReady](after = eventId)

      // Continue orderId //
      TestSemaphoreJob.continue()
      // FIXME OrderProcessed may not occur when recovering Agent stops SubagentDriver
      //  due to deletion while still recovering processing Orders.
      //  See fixme in SubagentKeeper
      controller.awaitNextKey[OrderProcessed](orderId, after = eventId)
      controller.awaitNextKey[OrderTerminated](orderId)
      assert(controllerState.idToOrder(orderId).lastOutcome ==
        OrderOutcome.Disrupted(ProcessLostDueSubagentDeletedProblem(bareSubagentId)))
      execCmd:
        DeleteOrdersWhenTerminated(orderId :: Nil)

      // Subagent is being removed
      controller.await[ItemDetached](_.event.key == bareSubagentItem.id)
      controller.await[ItemDeleted](_.event.key == bareSubagentItem.id)
    }

  "The Director Subagent cannot be deleted" in:
    val checked = controller.api
      .updateItems(Stream(
        DeleteSimple(directoryProvider.subagentId)))
      .await(99.s)
    assert(checked == Left(
      ItemIsStillReferencedProblem(directoryProvider.subagentId, agentPath,
        moreInfo = ", attached to Agent:AGENT")))


object SubagentDeleteTest:
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath, processLimit = 99)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
