package js7.tests.subagent

import fs2.Stream
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.core.command.CommandMeta
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.subagent.Problems.ProcessLostDueSubagentUriChangeProblem
import js7.data.subagent.SubagentCommand
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.SubagentMoveTest.*
import js7.tests.subagent.SubagentTester.agentPath

final class SubagentMoveTest extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val primarySubagentsDisabled = true

  private lazy val bare1SubagentItem =
    bareSubagentItem.copy(uri = findFreeLocalUri())

  "Restart Subagent at another URI" in:
    runSubagent(bareSubagentItem, awaitDedicated = false) { bareSubagent =>
      val aOrderId = OrderId("A-MOVE-SUBAGENT")
      var eventId = eventWatch.lastAddedEventId
      locally:
        controller.api.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow
        val processingStarted = eventWatch
          .await[OrderProcessingStarted](_.key == aOrderId, after = eventId).head.value.event
        assert(processingStarted == OrderProcessingStarted(bareSubagentItem.id))
        eventWatch.await[OrderStdoutWritten](_.key == aOrderId, after = eventId)
        // aOrderId is waiting for semaphore

      eventId = eventWatch.lastAddedEventId
      //val agentEventId = myAgent.eventWatch.lastAddedEventId
      controller.api.updateItems(Stream(AddOrChangeSimple(bare1SubagentItem)))
        .await(99.s).orThrow
      //myAgent.eventWatch.await[ItemAttachedToMe](_.event.item.key == bare1SubagentItem.id,
      //  after = agentEventId)
      //myAgent.eventWatch.await[SubagentCouplingFailed](_.key == bare1SubagentItem.id, after = agentEventId)

      // Start the replacing c1Subagent while the previous bareSubagent is still running
      runSubagent(bare1SubagentItem, suffix = "-1") { _ =>
        val aProcessed = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId).head
        assert(aProcessed.value.event ==
          OrderProcessed.processLost(ProcessLostDueSubagentUriChangeProblem))

        // After ProcessLost at previous Subagent aOrderId restarts at current Subagent
        TestSemaphoreJob.continue(1)  // aOrder still runs on bareSubagent (but it is ignored)
        TestSemaphoreJob.continue(1)
        val a2Processed = eventWatch
          .await[OrderProcessed](_.key == aOrderId, after = aProcessed.eventId)
          .head.value.event
        assert(a2Processed == OrderProcessed(OrderOutcome.succeeded))

        eventWatch.await[OrderFinished](_.key == aOrderId, after = eventId)

        locally:
          // Start another order
          val bOrderId = OrderId("B-MOVE-SUBAGENT")
          TestSemaphoreJob.continue(1)
          controller.api.addOrder(FreshOrder(bOrderId, workflow.path)).await(99.s).orThrow
          val bStarted = eventWatch.await[OrderProcessingStarted](_.key == bOrderId, after = eventId)
            .head.value.event
          assert(bStarted == OrderProcessingStarted(bare1SubagentItem.id))

          eventWatch.await[OrderStdoutWritten](_.key == bOrderId, after = eventId)

          eventWatch.await[OrderProcessed](_.key == bOrderId, after = eventId).head.value.event
          val bProcessed = eventWatch.await[OrderProcessed](_.key == bOrderId, after = eventId)
            .head.value.event
          assert(bProcessed == OrderProcessed(OrderOutcome.succeeded))
          eventWatch.await[OrderFinished](_.key == bOrderId, after = eventId)
      }

      bareSubagent.shutdown(
        SubagentCommand.ShutDown(Some(SIGKILL), dontWaitForDirector = true),
        CommandMeta.test
      ).await(99.s)
    }


object SubagentMoveTest:
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
