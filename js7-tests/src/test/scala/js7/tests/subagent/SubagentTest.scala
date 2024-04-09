package js7.tests.subagent

import java.util.concurrent.TimeoutException
import js7.base.Js7Version
import js7.base.Problems.MessageSignedByUnknownProblem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.delegate.DelegateCouplingState.Coupled
import js7.data.order.OrderEvent.{OrderAttached, OrderCancelled, OrderFinished, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentDedicated}
import js7.data.subagent.{SubagentId, SubagentItemState}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.SubagentTest.*
import js7.tests.subagent.SubagentTester.agentPath

final class SubagentTest extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val primarySubagentsDisabled = true

  "Local Subagent couplingState is Coupled" in:
    val localSubagentId = SubagentId("AGENT-0")
    eventWatch.await[SubagentCoupled](_.key == localSubagentId)
    awaitAndAssert(
      controllerState.keyTo(SubagentItemState)(localSubagentId).couplingState == Coupled)
    awaitAndAssert(controllerState.keyTo(SubagentItemState)(localSubagentId)
      .platformInfo.map(_.js7Version) contains Js7Version)

  "Reject items if no signature keys are installed" in:
    val eventId = eventWatch.lastAddedEventId

    runSubagent(bareSubagentItem, suppressSignatureKeys = true) { _ =>
      val orderId = OrderId("ITEM-SIGNATURE")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))

      val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(bareSubagentItem.id))

      val processed = eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
        .head.value.event
      assert(processed == OrderProcessed(OrderOutcome.Disrupted(MessageSignedByUnknownProblem)))
    }

  "CancelOrder" in:
    // Local Subagent must be disabled (see test above)

    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("CANCEL-ORDER")

    TestSemaphoreJob.reset()

    runSubagent(bareSubagentItem) { _ =>
      val subagentDedicated = eventWatch.await[SubagentDedicated](_.key == bareSubagentId, after = eventId)
        .head.value.event
      assert(subagentDedicated.platformInfo.map(_.js7Version) contains Js7Version)
      assert(controllerState.keyTo(SubagentItemState)(bareSubagentId)
        .platformInfo.map(_.js7Version) contains Js7Version)

      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))

      val processingStarted = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
      assert(processingStarted == OrderProcessingStarted(bareSubagentItem.id))

      val started = eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)
        .head.value.event
      assert(started == OrderStdoutWritten("TestSemaphoreJob\n"))

      controller.api.executeCommand(CancelOrders(Seq(orderId), CancellationMode.kill()))
        .await(99.s).orThrow

      val processed = eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
        .head.value.event
      assert(processed == OrderProcessed(OrderOutcome.killedInternal))
      eventWatch.await[OrderCancelled](_.key == orderId, after = eventId)
    }

  "Order waits when no Subagent is available" in:
    var eventId = eventWatch.lastAddedEventId
    enableSubagents(directoryProvider.subagentId -> false)

    // Be sure that BareSubagent's shutdown has been detected
    awaitAndAssert(10.s, 10.ms)(
      controllerState.keyTo(SubagentItemState)(bareSubagentId).problem.isDefined)

    val orderId = OrderId("WAIT-FOR-SUBAGENT")
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)

    intercept[TimeoutException]:
      eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId, timeout = 200.ms)

    eventId = eventWatch.lastAddedEventId
    runSubagent(bareSubagentItem) { _ =>
      TestSemaphoreJob.continue()
      val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
      assert(started.subagentId contains bareSubagentId)
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
    }

  //"Change URI of Director" --> See UpdateAgentRefsTest
  //"Change JobResource" in --> See JobResourceAtBareSubagentTest


object SubagentTest:
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
