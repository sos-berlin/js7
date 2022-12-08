package js7.tests.subagent

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.ResetSubagent
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation
import js7.data.item.ItemOperation.DeleteSimple
import js7.data.order.OrderEvent.{OrderAttached, OrderFinished, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentReset, SubagentResetStarted}
import js7.data.subagent.{SubagentId, SubagentRunId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.StealAndResetSubagentTest.*
import js7.tests.subagent.SubagentMultipleOrdersTest.agentPath
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.TimeoutException

final class StealAndResetSubagentTest extends OurTestSuite with SubagentTester
{
  override protected def agentConfig = config"""
    js7.auth.subagents.STOLEN-SUBAGENT = "THIEVE-AGENT-PASSWORD"
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath, thieveAgentPath)
  protected lazy val items = Seq(aWorkflow, thieveWorkflow, bareSubagentItem)
  override protected val subagentsDisabled = true

  protected implicit val scheduler = Scheduler.traced

  "ResetSubagent(force) steals a dedicated alien Subagent" in {
    // Both SubagentItems denote the same Subagent (same URI)
    val stolenSubagentItem = bareSubagentItem.copy(
      id = SubagentId("STOLEN-SUBAGENT"),
      agentPath = thieveAgentPath)

    val aOrderId = OrderId("A-ORDER")
    val thieveOrderId = OrderId("THIEVE-ORDER")
    var firstSubagentRunId: SubagentRunId = null

    val subagentConfig = config"""
      js7.auth.users.THIEVE-AGENT {
        permissions: [ AgentDirector ]
        password: "plain:THIEVE-AGENT-PASSWORD"  # Subagent must allow to be stolen!?
      }"""

    runSubagent(bareSubagentItem, subagentConfig) { subagent =>
      eventWatch.await[SubagentCoupled](_.key == bareSubagentId)
      firstSubagentRunId = subagent.subagentRunId

      // Start an Order
      locally {
        controllerApi.addOrder(FreshOrder(aOrderId, aWorkflow.path)).await(99.s).orThrow
        val processingStarted = eventWatch.await[OrderProcessingStarted](_.key == aOrderId)
          .head.value.event
        assert(processingStarted == OrderProcessingStarted(bareSubagentId))
        eventWatch.await[OrderProcessingStarted](_.key == aOrderId)
        eventWatch.await[OrderStdoutWritten](_.key == aOrderId)
        // Order waits for semaphore
      }

      // THE THIEVE COMES
      controllerApi.updateItems(Observable(ItemOperation.AddOrChangeSimple(stolenSubagentItem)))
        .await(99.s).orThrow

      // THIEVE STARTS AN ORDER
      controllerApi.addOrder(FreshOrder(thieveOrderId, thieveWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == thieveOrderId)

      // STEAL
      controllerApi.executeCommand(ResetSubagent(stolenSubagentItem.id, force = true))
        .await(99.s).orThrow
      eventWatch.await[SubagentResetStarted](_.key == stolenSubagentItem.id)

      // The stolen orders are killed
      val processed = eventWatch.await[OrderProcessed](_.key == aOrderId).head
      assert(processed.value.event ==
        OrderProcessed(Outcome.Killed(Outcome.Failed(Some("Canceled")))))

      intercept[TimeoutException] {
        // Times out because Subagent must be restarted
        eventWatch.await[OrderProcessingStarted](_.key == thieveOrderId, timeout = 1.s)
      }

      eventWatch.await[SubagentReset](_.key == stolenSubagentItem.id)
      subagent.untilTerminated.await(99.s)
    }

    // Reset and delete bareSubagentId to give the thieve a change to dedicate the Subagent
    controllerApi.executeCommand(ResetSubagent(bareSubagentId, force = true))
      .await(99.s).orThrow
    eventWatch.await[SubagentResetStarted](_.key == bareSubagentId)
    controllerApi.updateItems(Observable(DeleteSimple(bareSubagentId))).await(99.s).orThrow
    eventWatch.await[ItemDeleted](_.event.key == bareSubagentId)

    // START SUBAGENT AGAIN AND THIEVE WILL TAKE IT OVER
    val eventId = eventWatch.lastAddedEventId
    runSubagent(stolenSubagentItem, subagentConfig) { _ =>
      TestSemaphoreJob.continue(2)
      //eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
      eventWatch.await[OrderProcessingStarted](_.key == thieveOrderId, after = eventId)
      eventWatch.await[OrderFinished](_.key == thieveOrderId, after = eventId)
    }
  }
}

object StealAndResetSubagentTest
{
  val agentPath = AgentPath("AGENT")
  val thieveAgentPath = AgentPath("THIEVE-AGENT")

  private val aWorkflow = Workflow(
    WorkflowPath("A-WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  private val thieveWorkflow = Workflow(
    WorkflowPath("THIEVE-WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(thieveAgentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
}
