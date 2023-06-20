package js7.tests.subagent

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEither}
import js7.data.agent.AgentPath
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
import js7.tests.subagent.ResetSubagentWhileRunningTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler
import scala.concurrent.TimeoutException

final class ResetSubagentWhileRunningTest extends OurTestSuite with SubagentTester
{
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val primarySubagentsDisabled = true

  protected implicit val scheduler = Scheduler.traced

  "ResetSubagent to a Director is prohibited" in {
    val checked = controller.api.executeCommand(ResetSubagent(localSubagentId)).await(99.s)
    assert(checked == Left(Problem("Subagent:AGENT-0 as a Agent Director cannot be reset")))
  }

  "ResetSubagent while Subagent is coupled" in {
    enableSubagents(directoryProvider.subagentId -> false)

    val aOrderId = OrderId("A-ORDER")
    val bOrderId = OrderId("B-ORDER")
    var firstSubagentRunId: SubagentRunId = null

    runSubagent(bareSubagentItem) { subagent =>
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

      // Add bOrderId which should wait until Subagent has been reset and restarted
      controller.api.addOrder(FreshOrder(bOrderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == bOrderId)
      intercept[TimeoutException] {
        // Times out because Subagent is being reset
        eventWatch.await[OrderProcessingStarted](_.key == bOrderId, timeout = 1.s)
      }

      eventWatch.await[SubagentReset](_.key == bareSubagentId)

      // Due to reset(force=false) the Subagent is shut down without killing processes ???
      // Continue aOrderId, will emit OrderProcessed.succeeded, which must be suppressed
      // because reset emits OrderProcessed.processLost.
      //TestSemaphoreJob.continue()

      // SubagentShutdown is suppressed like any other event from Subagent after Reset
      //eventWatch.await[SubagentShutdown](_.key == bareSubagentId)
      subagent.untilTerminated.await(99.s)
    }

    assert(eventWatch.allKeyedEvents[SubagentItemStateEvent]
      .collect {
        case KeyedEvent(`bareSubagentId`, event @ SubagentCouplingFailed(SubagentAlreadyDedicatedProblem)) =>
          Some(event)

        case KeyedEvent(`bareSubagentId`, SubagentDedicated(runId, _)) =>
          Some(SubagentDedicated(runId, Some(PlatformInfo.test)))

        case KeyedEvent(`bareSubagentId`, event @ SubagentCouplingFailed(problem)) =>
          !problem.toString.contains("Connection refused") ? event

        case KeyedEvent(`bareSubagentId`, event) => Some(event)
      }.flatten ==
      Seq(
        SubagentDedicated(firstSubagentRunId, Some(PlatformInfo.test)),
        SubagentCoupled,
        SubagentResetStartedByController(false),
        SubagentResetStarted(false),
        SubagentCouplingFailed(Problem("decoupled")),
        SubagentReset,
        SubagentCouplingFailed(SubagentAlreadyDedicatedProblem)/*time dependent?*/))

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
      eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
      eventWatch.await[OrderProcessingStarted](_.key == bOrderId, after = eventId)
      eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)
      eventWatch.await[OrderFinished](_.key == bOrderId, after = eventId)
    }
  }
}

object ResetSubagentWhileRunningTest
{
  private val agentPath = AgentPath("AGENT")
  private val localSubagentId = toLocalSubagentId(agentPath)

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
}
