package js7.tests.subagent

import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.useSync
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.controller.ControllerCommand.ResetSubagent
import js7.data.event.{Event, KeyedEvent}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.platform.PlatformInfo
import js7.data.subagent.SubagentItemStateEvent.{SubagentCoupled, SubagentCouplingFailed, SubagentDedicated, SubagentReset, SubagentResetStarted, SubagentResetStartedByController}
import js7.data.subagent.SubagentRunId
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.ResetSubagentTest.*
import js7.tests.subagent.SubagentTester.agentPath
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.collection.immutable.Seq

final class ResetSubagentTest extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  override protected val primarySubagentsDisabled = true
  protected lazy val items = Seq(workflow, bareSubagentItem)

  "ResetSubagent after Subagent has been shutdown" in:
    enableSubagents(directoryProvider.subagentId -> false)

    val orderId = OrderId("RESET-SUBAGENT-AFTER-SHUTDOWN")
    var firstSubagentRunId: SubagentRunId | Null = null

    runSubagent(bareSubagentItem): subagent =>
      eventWatch.await[SubagentCoupled](_.key == bareSubagentId)
      firstSubagentRunId = subagent.subagentRunId
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == orderId)

      // For this test, the terminating Subagent must no emit any event before shutdown
      subagent.journal.stopEventWatch()
      subagent.shutdown(Some(SIGKILL), dontWaitForDirector = true, restart = true).await(99.s)
      assert(subagent.untilTerminated.await(99.s).restart)

    eventWatch.await[SubagentCouplingFailed](_.key == bareSubagentId)

    execCmd(ResetSubagent(bareSubagentItem.id))
    val processed1 = eventWatch.await[OrderProcessed](_.key == orderId).head
    assert(processed1.value.event == OrderProcessed.processLostDueToReset)

    eventWatch.await[OrderMoved](_.key == orderId, after = processed1.eventId)
    eventWatch.await[SubagentReset](_.key == bareSubagentId)

    assert(eventWatch.allKeyedEvents[Event]
      .collect:
        case KeyedEvent(`bareSubagentId`, SubagentCouplingFailed(_)) => None
        case KeyedEvent(`bareSubagentId`, SubagentDedicated(runId, _)) =>
          Some(SubagentDedicated(runId, Some(PlatformInfo.test)))
        case KeyedEvent(`bareSubagentId`, event) => Some(event)
        case KeyedEvent(`orderId`, event) => Some(event)
        case _ => None
      .flatten ==
      Seq(
        SubagentDedicated(firstSubagentRunId.nn, Some(PlatformInfo.test)),
        SubagentCoupled,
        OrderAdded(workflow.id),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(bareSubagentId),
        OrderStdoutWritten("TestSemaphoreJob\n"),

        SubagentResetStartedByController(force = false),
        SubagentResetStarted(force = false),
        OrderProcessed.processLostDueToReset,
        SubagentReset,
        OrderMoved(Position(0))))

    runSubagent(bareSubagentItem): _ =>
      TestSemaphoreJob.continue()
      val processed2 = eventWatch.await[OrderProcessed](_.key == orderId, after = processed1.eventId)
        .head.value.event
      assert(processed2 == OrderProcessed(OrderOutcome.succeeded))
      eventWatch.await[OrderFinished](_.key == orderId).head.value.event

  "ResetSubagent with reset for a Subagent started as inactive Director" in:
    directoryProvider
      .directorEnvResource(
        bareSubagentItem,
        otherSubagentIds = Seq(toLocalSubagentId(agentPath)))
      .flatMap(_.directorResource)
      .useSync(99.s): director =>
        import director.subagent
        eventWatch.await[SubagentCoupled](_.key == bareSubagentId)
        subagent.shutdown(restart = true).await(99.s)

        // The director(!) must return restart=true
        assert(subagent.untilTerminated.await(99.s).restart)


object ResetSubagentTest:
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
