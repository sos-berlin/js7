package js7.tests.subagent

import fs2.Stream
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.ResetSubagent
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation
import js7.data.item.ItemOperation.DeleteSimple
import js7.data.order.OrderEvent.{OrderAttached, OrderFinished, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.subagent.Problems.{ProcessKilledDueToSubagentShutdownProblem, ProcessLostDueToShutdownProblem}
import js7.data.subagent.SubagentId
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentReset, SubagentResetStarted}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.StealAndResetSubagentTest.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.concurrent.TimeoutException

final class StealAndResetSubagentTest extends OurTestSuite, SubagentTester:

  override protected def agentConfig = config"""
    js7.auth.subagents.STOLEN-SUBAGENT = "THIEVE-AGENT-PASSWORD"
    """.withFallback(super.agentConfig)

  protected val agentPaths = Seq(agentPath, thieveAgentPath)
  protected lazy val items = Seq(aWorkflow, thieveWorkflow, bareSubagentItem)
  override protected val primarySubagentsDisabled = true

  "ResetSubagent(force) steals a dedicated alien Subagent" in:
    // Both SubagentItems denote the same Subagent (same URI)
    val stolenSubagentItem = bareSubagentItem.copy(
      id = SubagentId("STOLEN-SUBAGENT"),
      agentPath = thieveAgentPath)

    val aOrderId = OrderId("A-ORDER")
    val thieveOrderId = OrderId("THIEVE-ORDER")

    val bareSubagentConfig = config"""
      js7.auth.users.${toLocalSubagentId(agentPath)} {
        permissions: [ AgentDirector ]
        password: "plain:${toLocalSubagentId(agentPath)}'s PASSWORD"
      }
      js7.auth.users.${toLocalSubagentId(thieveAgentPath)} {
        permissions: [ AgentDirector ]
        password: "plain:THIEVE-AGENT-PASSWORD"  # Subagent must allow to be stolen!?
      }"""

    runSubagent(bareSubagentItem, config = bareSubagentConfig) { subagent =>
      eventWatch.await[SubagentCoupled](_.key == bareSubagentId)

      // Start an Order
      locally:
        controller.api.addOrder(FreshOrder(aOrderId, aWorkflow.path)).await(99.s).orThrow
        val processingStarted = eventWatch.await[OrderProcessingStarted](_.key == aOrderId)
          .head.value.event
        assert(processingStarted == OrderProcessingStarted(bareSubagentId))
        eventWatch.await[OrderProcessingStarted](_.key == aOrderId)
        eventWatch.await[OrderStdoutWritten](_.key == aOrderId)
        // Order waits for semaphore

      // THE THIEVE COMES
      controller.api.updateItems(Stream(ItemOperation.AddOrChangeSimple(stolenSubagentItem)))
        .await(99.s).orThrow

      // THIEVE STARTS AN ORDER
      controller.api.addOrder(FreshOrder(thieveOrderId, thieveWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == thieveOrderId)

      // STEAL THE SUBAGENT
      execCmd(ResetSubagent(stolenSubagentItem.id, force = true))
      eventWatch.await[SubagentResetStarted](_.key == stolenSubagentItem.id)

      // The stolen orders are canceled (and its processes have been killed)
      val processed = eventWatch.await[OrderProcessed](_.key == aOrderId).head.value.event
      assert:
        val killed = OrderOutcome.Killed(OrderOutcome.Failed(Some("Canceled")))
        processed == OrderProcessed(OrderOutcome.processLost(ProcessLostDueToShutdownProblem)) ||
        processed == OrderProcessed(OrderOutcome.processLost(ProcessKilledDueToSubagentShutdownProblem(killed))) ||
        processed == OrderProcessed(killed)

      intercept[TimeoutException]:
        // Times out because Subagent must be restarted
        eventWatch.await[OrderProcessingStarted](_.key == thieveOrderId, timeout = 1.s)

      eventWatch.await[SubagentReset](_.key == stolenSubagentItem.id)
      subagent.untilTerminated.await(99.s)
    }

    // Reset and delete bareSubagentId to give the thieve a change to dedicate the Subagent
    execCmd(ResetSubagent(bareSubagentId, force = true))
    eventWatch.await[SubagentResetStarted](_.key == bareSubagentId)
    controller.api.updateItems(Stream(DeleteSimple(bareSubagentId))).await(99.s).orThrow
    eventWatch.await[ItemDeleted](_.event.key == bareSubagentId)

    // START SUBAGENT AGAIN AND THIEVE WILL TAKE IT OVER
    val eventId = eventWatch.lastAddedEventId
    runSubagent(stolenSubagentItem, config = bareSubagentConfig) { _ =>
      TestSemaphoreJob.continue(2)
      //eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
      eventWatch.await[OrderProcessingStarted](_.key == thieveOrderId, after = eventId)
      eventWatch.await[OrderFinished](_.key == thieveOrderId, after = eventId)
    }


object StealAndResetSubagentTest:
  private val thieveAgentPath = AgentPath("THIEVE-AGENT")

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
