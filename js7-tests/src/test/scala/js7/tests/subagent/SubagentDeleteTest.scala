package js7.tests.subagent

import java.util.concurrent.TimeoutException
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.{ItemDeleted, ItemDetachable, ItemDetached}
import js7.data.item.ItemOperation.{AddOrChangeSimple, DeleteSimple}
import js7.data.order.OrderEvent.{OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.SubagentDeleteTest._
import js7.tests.subagent.SubagentMoveTwiceTest.TestSemaphoreJob
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class SubagentDeleteTest extends AnyFreeSpec with SubagentTester
{
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val subagentsDisabled = true

  protected implicit val scheduler = Scheduler.global

  "Delete the Director Subagent ❓" in pending // TODO

  "Delete bare Subagent" in {
    runSubagent(bareSubagentItem) { _ =>
      val eventId = eventWatch.lastAddedEventId
      controllerApi
        .updateItems(Observable(
          DeleteSimple(bareSubagentItem.path)))
        .await(99.s)
        .orThrow
      eventWatch.await[ItemDetachable](_.event.key == bareSubagentItem.id, after = eventId)
      eventWatch.await[ItemDeleted](_.event.key == bareSubagentItem.id, after = eventId)
    }
  }

  "Delete Subagent while an Order is processed" in {
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("REMOVE-SUBAGENT")
    controllerApi
      .updateItems(Observable(
        AddOrChangeSimple(bareSubagentItem)))
      .await(99.s)
      .orThrow
    runSubagent(bareSubagentItem) { subagent =>
      controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(bareSubagentItem.path))
      eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

      controllerApi
        .updateItems(Observable(
          DeleteSimple(bareSubagentItem.path)))
        .await(99.s)
        .orThrow
      eventWatch.await[ItemDetachable](_.event.key == bareSubagentItem.id, after = eventId)

      // ItemDetached is delayed until no Order is being processed
      intercept[TimeoutException](
        eventWatch.await[ItemDetached](_.event.key == bareSubagentItem.id, after = eventId, timeout = 1.s))

      TestSemaphoreJob.continue()
      val processed = eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
        .head.value.event
      assert(processed == OrderProcessed(Outcome.succeeded))

      eventWatch.await[ItemDetached](_.event.key == bareSubagentItem.id, after = eventId)
      eventWatch.await[ItemDeleted](_.event.key == bareSubagentItem.id, after = eventId)
      //subagent.shutdown(signal = None).await(99.s)
      subagent.untilStopped.await(99.s)
    }
  }

  // TODO Das Löschen kann dauern, wenn der Auftrag lange dauert,
  //  länger als ein HTTP-Request braucht.
  //  Also nicht auf Antwort auf AgentCommand.DetachItem warten,
  //  sondern auf ein Event: ItemDetachable (vom Agenten?)

  "Don't allow orders to start while SubagentItem is deleted ❓" in pending // TODO

  "Delete Subagent and continue deletion after Subagent's restart ❓" in pending // TODO
}

object SubagentDeleteTest
{
  val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
}
