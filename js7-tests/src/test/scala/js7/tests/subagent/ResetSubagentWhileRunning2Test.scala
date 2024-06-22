package js7.tests.subagent

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.controller.ControllerCommand.ResetSubagent
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.platform.PlatformInfo
import js7.data.subagent.Problems.SubagentAlreadyDedicatedProblem
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentDedicated, SubagentReset, SubagentResetStarted, SubagentResetStartedByController}
import js7.data.subagent.{SubagentItemStateEvent, SubagentRunId}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.ResetSubagentWhileRunning2Test.*
import js7.tests.subagent.SubagentTester.agentPath
import scala.concurrent.TimeoutException

final class ResetSubagentWhileRunning2Test extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  override protected val primarySubagentsDisabled = true
  protected lazy val items = Seq(workflow, bareSubagentItem)

  "ResetSubagent while Subagent is coupled and Subagent does not shut down" in:
    enableSubagents(directoryProvider.subagentId -> false)

    val aOrderId = OrderId("A-ORDER")
    val bOrderId = OrderId("B-ORDER")
    var firstSubagentRunId: SubagentRunId = null

    runSubagent(bareSubagentItem,
      config = config"""js7.tests.SubagentDriver.suppressResetShutdown = true"""
    ) { subagent =>
      eventWatch.await[SubagentCoupled](_.key == bareSubagentId)
      firstSubagentRunId = subagent.subagentRunId

      controller.api.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderProcessingStarted](_.key == aOrderId)
      eventWatch.await[OrderStdoutWritten](_.key == aOrderId)

      // RESET
      controller.api.executeCommand(ResetSubagent(bareSubagentItem.id)).await(99.s).orThrow
      eventWatch.await[SubagentResetStarted](_.key == bareSubagentId)

      val processed = eventWatch.await[OrderProcessed](_.key == aOrderId).head
      assert(processed.value.event == OrderProcessed.processLostDueToReset)
      eventWatch.await[OrderMoved](_.key == aOrderId, after = processed.eventId)

      // Subagent continue to run because we have suppressed SubagentCommand.ShutDown

      // Add bOrderId which should wait until Subagent has been reset and restarted
      controller.api.addOrder(FreshOrder(bOrderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == bOrderId)
      intercept[TimeoutException]:
        // Times out because Subagent is being reset
        eventWatch.await[OrderProcessingStarted](_.key == bOrderId, timeout = 1.s)

      val eventId = eventWatch.await[SubagentReset](_.key == bareSubagentId).head.eventId
      val failed1 = eventWatch.await[SubagentCouplingFailed](_.key == bareSubagentId,
        after = eventId).head
      assert(failed1.value.event ==
        SubagentCouplingFailed(SubagentAlreadyDedicatedProblem("Subagent is already in use")))
      //subagent.shutdown(Some(SIGKILL))
    }
    // SubagentShutdown is suppressed like any other event from Subagent after Reset
    //eventWatch.await[SubagentShutdown](_.key == bareSubagentId)

    assert(eventWatch.allKeyedEvents[SubagentItemStateEvent]
      .collect {
        case KeyedEvent(`bareSubagentId`, event @ SubagentCouplingFailed(SubagentAlreadyDedicatedProblem("Subagent is already in use"))) =>
          Some(event)

        case ke @ KeyedEvent(`bareSubagentId`, event @ SubagentCouplingFailed(problem)) =>
          if problem.toString.contains("Connection refused")
            || problem.toString.contains("504 Service Unavailable")
          then
            logger.warn(ke.toString)
            None
          else
            Some(event)

        case KeyedEvent(`bareSubagentId`, SubagentDedicated(runId, _)) =>
          Some(SubagentDedicated(runId, Some(PlatformInfo.test)))

        case KeyedEvent(`bareSubagentId`, event) => Some(event)
      }.flatten ==
      Seq(
        SubagentDedicated(firstSubagentRunId, Some(PlatformInfo.test)),
        SubagentCoupled,
        SubagentResetStartedByController(false),
        SubagentResetStarted(false),
        SubagentCouplingFailed(Problem("decoupled")),
        SubagentReset,
        SubagentCouplingFailed(SubagentAlreadyDedicatedProblem("Subagent is already in use"))/*time dependent?*/))

    assert(eventWatch.allKeyedEvents[OrderEvent] == Seq(
      aOrderId <-: OrderAdded(workflow.id),
      aOrderId <-: OrderAttachable(agentPath),
      aOrderId <-: OrderAttached(agentPath),
      aOrderId <-: OrderStarted,
      aOrderId <-: OrderProcessingStarted(bareSubagentId),
      aOrderId <-: OrderStdoutWritten("TestSemaphoreJob\n"),
      aOrderId <-: OrderProcessed.processLostDueToReset,
      aOrderId <-: OrderMoved(Position(0)),

      bOrderId <-: OrderAdded(workflow.id),
      bOrderId <-: OrderAttachable(agentPath),
      bOrderId <-: OrderAttached(agentPath)))

    // START SUBAGENT AGAIN
    val eventId = eventWatch.lastAddedEventId
    runSubagent(bareSubagentItem) { _ =>
      TestSemaphoreJob.continue(2)
      for orderId <- Seq(aOrderId, bOrderId) do
        eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
    }


object ResetSubagentWhileRunning2Test:
  private val logger = Logger[this.type]

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
