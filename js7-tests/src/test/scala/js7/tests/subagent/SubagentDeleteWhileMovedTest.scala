package js7.tests.subagent

import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation.{AddOrChangeSimple, DeleteSimple}
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.SubagentItemStateEvent.SubagentCouplingFailed
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.subagent.SubagentDeleteWhileMovedTest.*
import js7.tests.subagent.SubagentTester.agentPath
import monix.execution.Scheduler
import monix.reactive.Observable

final class SubagentDeleteWhileMovedTest extends OurTestSuite, SubagentTester:
  
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val primarySubagentsDisabled = true

  protected implicit val scheduler = Scheduler.traced

  "Delete SubagentItem while being changed" in:
    var eventId = eventWatch.lastAddedEventId
    runSubagent(bareSubagentItem) { _ =>
      val aOrderId = OrderId("A-CHANGE-URI-TWICE")
      locally:
        controller.api.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow
        val processingStarted = eventWatch
          .await[OrderProcessingStarted](_.key == aOrderId, after = eventId).head.value.event
        assert(processingStarted == OrderProcessingStarted(bareSubagentId))
        eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)
        // aOrderId is waiting for semaphore
    }

    // Change URI, but do not start a corresponding Subagent
    eventId = eventWatch.lastAddedEventId
    locally:
      val bare1SubagentItem = bareSubagentItem.copy(uri = findFreeLocalUri())
      val agentEventId = agent.eventWatch.lastAddedEventId
      controller.api.updateItems(Observable(AddOrChangeSimple(bare1SubagentItem)))
        .await(99.s).orThrow
      agent.eventWatch.await[SubagentCouplingFailed](_.key == bareSubagentId, after = agentEventId)

    // Delete SubagentItem
    locally:
      eventId = eventWatch.lastAddedEventId
      controller.api.updateItems(Observable(DeleteSimple(bareSubagentId)))
        .await(99.s).orThrow
      eventWatch.await[ItemDeleted](_.event.key == bareSubagentId, after = eventId)


object SubagentDeleteWhileMovedTest:
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      EmptyJob.execute(agentPath)))
