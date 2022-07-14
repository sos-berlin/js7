package js7.tests.subagent

import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation.{AddOrChangeSimple, DeleteSimple}
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentItemStateEvent.SubagentCouplingFailed
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentDeleteWhileMovedTest.*
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class SubagentDeleteWhileMovedTest extends AnyFreeSpec with SubagentTester
{
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val subagentsDisabled = true

  protected implicit val scheduler = Scheduler.traced

  "Delete SubagentItem while being changed" in {
    var eventId = eventWatch.lastAddedEventId
    runSubagent(bareSubagentItem) { _ =>
      val aOrderId = OrderId("A-CHANGE-URI-TWICE")
      locally {
        controllerApi.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow
        val processingStarted = eventWatch
          .await[OrderProcessingStarted](_.key == aOrderId, after = eventId).head.value.event
        assert(processingStarted == OrderProcessingStarted(bareSubagentId))
        eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)
        // aOrderId is waiting for semaphore
      }
    }

    // Change URI, but do not start a corresponding Subagent
    eventId = eventWatch.lastAddedEventId
    locally {
      val bare1SubagentItem = bareSubagentItem.copy(uri = Uri("http://localhost:" + findFreeTcpPort()))
      val agentEventId = agent.eventWatch.lastAddedEventId
      controllerApi.updateItems(Observable(AddOrChangeSimple(bare1SubagentItem)))
        .await(99.s).orThrow
      agent.eventWatch.await[SubagentCouplingFailed](_.key == bareSubagentId, after = agentEventId)
    }

    // Delete SubagentItem
    locally {
      eventId = eventWatch.lastAddedEventId
      controllerApi.updateItems(Observable(DeleteSimple(bareSubagentId)))
        .await(99.s).orThrow
      eventWatch.await[ItemDeleted](_.event.key == bareSubagentId, after = eventId)
    }
  }
}

object SubagentDeleteWhileMovedTest
{
  val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      EmptyJob.execute(agentPath)))
}
