package js7.tests.subagent

import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.subagent.SubagentItemStateEvent.SubagentCouplingFailed
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.SubagentMoveTwiceTest._
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class SubagentMoveTwiceTest extends AnyFreeSpec with SubagentTester
{
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val subagentsDisabled = true
  protected implicit val scheduler = Scheduler.global

  "Change URI twice before Subagent has restarted" in {
    // Start bareSubagent
    val (bareSubagent, bareSubagentRelease) = subagentResource(bareSubagentItem, awaitDedicated = false)
      .allocated.await(99.s)

    var eventId = eventWatch.lastAddedEventId
    val aOrderId = OrderId("A-ORDER")
    locally {
      controllerApi.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow
      val processingStarted = eventWatch
        .await[OrderProcessingStarted](_.key == aOrderId, after = eventId).head.value.event
      assert(processingStarted == OrderProcessingStarted(bareSubagentId))
      eventWatch.await[OrderStdoutWritten](_.key == aOrderId, after = eventId)
      // aOrderId is waiting for semaphore
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

    // Change URI again and start the corresponding Subagent
    val bare2SubagentItem = bareSubagentItem.copy(uri = Uri("http://localhost:" + findFreeTcpPort()))
    locally {
      val agentEventId = agent.eventWatch.lastAddedEventId
      controllerApi.updateItems(Observable(AddOrChangeSimple(bare2SubagentItem)))
        .await(99.s).orThrow
      agent.eventWatch.await[SubagentCouplingFailed](_.key == bareSubagentId, after = agentEventId)
    }

    // Start the replacing bareSubagent
    runSubagent(bare2SubagentItem, suffix = "-2") { _ =>
      val aProcessed = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId).head
      assert(aProcessed.value.event == OrderProcessed.processLost)

      // Shutdown the original Subagent
      bareSubagent.shutdown(Some(SIGKILL), dontWaitForDirector = true).await(99.s)
      bareSubagentRelease.await(99.s)

      // After ProcessLost at previous Subagent aOrderId restarts at current Subagent
      //TestSemaphoreJob.continue(1)  // aOrder still runs on bareSubagent (but it is ignored)
      TestSemaphoreJob.continue(1)
      val a2Processed = eventWatch
        .await[OrderProcessed](_.key == aOrderId, after = aProcessed.eventId)
        .head.value.event
      assert(a2Processed == OrderProcessed(Outcome.succeeded))

      eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)

      eventId = eventWatch.lastAddedEventId
      locally {
        // Start another order
        val bOrderId = OrderId("B-ORDER")
        TestSemaphoreJob.continue(1)
        controllerApi.addOrder(FreshOrder(bOrderId, workflow.path)).await(99.s).orThrow
        val bStarted = eventWatch.await[OrderProcessingStarted](_.key == bOrderId, after = eventId)
          .head.value.event
        assert(bStarted == OrderProcessingStarted(bareSubagentId))

        eventWatch.await[OrderStdoutWritten](_.key == bOrderId, after = eventId)

        eventWatch.await[OrderProcessed](_.key == bOrderId, after = eventId).head.value.event
        val bProcessed = eventWatch.await[OrderProcessed](_.key == bOrderId, after = eventId)
          .head.value.event
        assert(bProcessed == OrderProcessed(Outcome.succeeded))
        eventWatch.await[OrderFinished](_.key == bOrderId, after = eventId)
      }
    }
  }
}

object SubagentMoveTwiceTest
{
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
}