package js7.tests.subagent

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.ResetSubagent
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.subagent.Problems.SubagentAlreadyDedicatedProblem
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentDedicated, SubagentReset, SubagentResetStarted, SubagentResetStartedByController}
import js7.data.subagent.{SubagentItemStateEvent, SubagentRunId}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.ResetSubagentWhileRunning2Test._
import js7.tests.subagent.ResetSubagentWhileRunningTest.agentPath
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.TimeoutException

final class ResetSubagentWhileRunning2Test extends AnyFreeSpec with SubagentTester
{
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val subagentsDisabled = true

  protected implicit val scheduler = Scheduler.global

  "ResetSubagent while Subagent is coupled and Subagent does not shut down" in {
    enableSubagents(directoryProvider.subagentItems.head -> false)

    val aOrderId = OrderId("A-ORDER")
    val bOrderId = OrderId("B-ORDER")
    var firstSubagentRunId: SubagentRunId = null

    runSubagent(bareSubagentItem,
      config"""js7.tests.RemoteSubagentDriver.suppressResetShutdown = true""")
    { subagent =>
      eventWatch.await[SubagentCoupled](_.key == bareSubagentId)
      firstSubagentRunId = subagent.subagentRunId

      controllerApi.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderProcessingStarted](_.key == aOrderId)
      eventWatch.await[OrderStdoutWritten](_.key == aOrderId)

      // RESET
      controllerApi.executeCommand(ResetSubagent(bareSubagentItem.id)).await(99.s).orThrow
      eventWatch.await[SubagentResetStarted](_.key == bareSubagentId)

      val processed = eventWatch.await[OrderProcessed](_.key == aOrderId).head
      assert(processed.value.event == OrderProcessed.processLost)
      eventWatch.await[OrderMoved](_.key == aOrderId, after = processed.eventId)

      // Subagent continue to run because we have suppressed SubagentCommand.ShutDown

      // Add bOrderId which should wait until Subagent has been reset and restarted
      controllerApi.addOrder(FreshOrder(bOrderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == bOrderId)
      intercept[TimeoutException] {
        // Times out because Subagent is being reset
        eventWatch.await[OrderProcessingStarted](_.key == bOrderId, timeout = 1.s)
      }

      val eventId = eventWatch.await[SubagentReset](_.key == bareSubagentId).head.eventId
      val failed1 = eventWatch.await[SubagentCouplingFailed](_.key == bareSubagentId,
        after = eventId).head
      assert(failed1.value.event == SubagentCouplingFailed(SubagentAlreadyDedicatedProblem))
      //subagent.shutdown(Some(SIGKILL))
    }
    // SubagentShutdown is suppressed like any other event from Subagent after Reset
    //eventWatch.await[SubagentShutdown](_.key == bareSubagentId)

    assert(eventWatch.allKeyedEvents[SubagentItemStateEvent]
      .collect {
        case KeyedEvent(`bareSubagentId`, event @ SubagentCouplingFailed(SubagentAlreadyDedicatedProblem)) =>
          Some(event)

        case KeyedEvent(`bareSubagentId`, event @ SubagentCouplingFailed(problem)) =>
          !problem.toString.contains("Connection refused") ? event

        case KeyedEvent(`bareSubagentId`, event) => Some(event)
      }.flatten ==
      Seq(
        SubagentDedicated(firstSubagentRunId),
        SubagentCoupled,
        SubagentResetStartedByController(false),
        SubagentResetStarted(false),
        SubagentCouplingFailed(Problem("SubagentEventListener stopped")),
        SubagentReset,
        SubagentCouplingFailed(SubagentAlreadyDedicatedProblem)/*time dependent?*/))

    assert(eventWatch.allKeyedEvents[OrderEvent] == Seq(
      aOrderId <-: OrderAdded(workflow.id),
      aOrderId <-: OrderAttachable(agentPath),
      aOrderId <-: OrderAttached(agentPath),
      aOrderId <-: OrderStarted,
      aOrderId <-: OrderProcessingStarted(bareSubagentId),
      aOrderId <-: OrderStdoutWritten("STARTED\n"),
      aOrderId <-: OrderProcessed.processLost,
      aOrderId <-: OrderMoved(Position(0)),

      bOrderId <-: OrderAdded(workflow.id),
      bOrderId <-: OrderAttachable(agentPath),
      bOrderId <-: OrderAttached(agentPath)))

    // START SUBAGENT AGAIN
    val eventId = eventWatch.lastAddedEventId
    runSubagent(bareSubagentItem) { _ =>
      TestSemaphoreJob.continue(2)
      for (orderId <- Seq(aOrderId, bOrderId)) {
        eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
      }
    }
  }
}

object ResetSubagentWhileRunning2Test
{
  val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
}
