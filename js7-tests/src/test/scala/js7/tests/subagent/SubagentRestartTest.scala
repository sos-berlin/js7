package js7.tests.subagent

import js7.agent.RunningAgent
import js7.base.Problems.MessageSignedByUnknownProblem
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentReady
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.Outcome.Disrupted.ProcessLost
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.SubagentRestartTest._
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.View

final class SubagentRestartTest extends AnyFreeSpec with SubagentTester
{
  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val subagentsDisabled = true

  protected implicit val scheduler = Scheduler.global

  private var myAgent: RunningAgent = null

  override def beforeAll() = {
    super.beforeAll()
    myAgent = agent
  }

  override def afterAll() = {
    controllerApi.stop.await(99.s)
    controller.terminate().await(99.s)
    for (a <- Option(myAgent)) a.terminate().await(99.s)
    super.afterAll()
  }

  "Reject items if no signature keys are installed" in {
    val eventId = eventWatch.lastAddedEventId

    runSubagent(bareSubagentItem, suppressSignatureKeys = true) { _ =>
      val orderId = OrderId("ITEM-SIGNATURE")
      controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow

      val started = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(bareSubagentItem.id))

      val processed = eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
        .head.value.event
      assert(processed == OrderProcessed(Outcome.Disrupted(MessageSignedByUnknownProblem)))
    }
  }

  "Restart Director" in {
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("RESTART-DIRECTOR")

    runSubagent(bareSubagentItem) { _ =>
      locally {
        controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        val events = eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
        assert(events.head.value.event == OrderProcessingStarted(bareSubagentItem.id))

        // STOP DIRECTOR
        myAgent.terminate().await(99.s)
      }

      TestSemaphoreJob.continue()

      locally {
        val eventId = eventWatch.lastAddedEventId
        eventWatch.keyedEvents[OrderProcessed] foreach {
          case ke @ KeyedEvent(`orderId`, OrderProcessed(_)) => fail(s"Unexpected $ke")
          case _ =>
        }

        // START DIRECTOR
        myAgent = directoryProvider.startAgent(agentPath).await(99.s)
        eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
        val events = eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
        assert(events.head.value.event.isInstanceOf[OrderFinished])
      }
    }
  }

  "Restart remote Subagent while a job is running" in {
    var eventId = eventWatch.lastAddedEventId
    val aOrderId = OrderId("A-RESTART-SUBAGENT")

    TestSemaphoreJob.reset()

    runSubagent(bareSubagentItem) { subagent =>
      controller.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow

      val started = eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(bareSubagentItem.id))

      val written = eventWatch.await[OrderStdoutWritten](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(written == OrderStdoutWritten("STARTED\n"))

      // For this test, the terminating Subagent must no emit any event before shutdown
      subagent.journal.stopEventWatch()
      subagent.shutdown(Some(SIGKILL), dontWaitForDirector = true).await(99.s)
    }

    // Subagent is unreachable now
    eventId = eventWatch.lastAddedEventId
    val bOrderId = OrderId("B-RESTART-SUBAGENT")
    controller.addOrder(FreshOrder(bOrderId, workflow.path)).await(99.s).orThrow

    runSubagent(bareSubagentItem) { _ =>
      locally {
        val events = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId)
        assert(events.head.value.event == OrderProcessed(Outcome.Disrupted(ProcessLost)))

        // OrderProcessed must be followed by OrderMoved
        eventWatch.await[OrderMoved](_.key == aOrderId, after = events.last.eventId)
      }
      locally {
        sleep(4.s)
        TestSemaphoreJob.continue(2)
        eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        for (orderId <- View(aOrderId, bOrderId)) {
          val events = eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
          assert(events.head.value.event.isInstanceOf[OrderFinished])
        }
      }
    }
  }

  "Restart both Director and remote Subagent while a job is running" in {
    var eventId = eventWatch.lastAddedEventId
    val aOrderId = OrderId("A-RESTART-BOTH")

    TestSemaphoreJob.reset()

    runSubagent(bareSubagentItem) { subagent =>
      controller.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow

      val started = eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(started == OrderProcessingStarted(bareSubagentItem.id))

      val written = eventWatch.await[OrderStdoutWritten](_.key == aOrderId, after = eventId)
        .head.value.event
      assert(written == OrderStdoutWritten("STARTED\n"))

      // For this test, the terminating Subagent must no emit any event before shutdown
      subagent.journal.stopEventWatch()
      subagent.shutdown(Some(SIGKILL), dontWaitForDirector = true).await(99.s)

      // STOP DIRECTOR
      myAgent.terminate().await(99.s)
    }
    // Subagent is unreachable now

    // START DIRECTOR
    eventId = eventWatch.lastAddedEventId
    logger.debug(s"eventId=$eventId")
    myAgent = directoryProvider.startAgent(agentPath).await(99.s)
    eventWatch.await[AgentReady](after = eventId)

    val bOrderId = OrderId("B-RESTART-BOTH")
    controller.addOrder(FreshOrder(bOrderId, workflow.path)).await(99.s).orThrow

    runSubagent(bareSubagentItem) { _ =>
      locally {
        val events = eventWatch.await[OrderProcessed](_.key == aOrderId, after = eventId)
        assert(events.head.value.event == OrderProcessed(Outcome.Disrupted(ProcessLost)))

        // OrderProcessed must be followed by OrderMoved
        eventWatch.await[OrderMoved](_.key == aOrderId, after = events.last.eventId)
      }
      locally {
        TestSemaphoreJob.continue(2)
        eventWatch.await[OrderProcessingStarted](_.key == aOrderId, after = eventId)
        for (orderId <- View(aOrderId, bOrderId)) {
          val events = eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
          assert(events.head.value.event.isInstanceOf[OrderFinished])
        }
      }
    }
  }
}

object SubagentRestartTest
{
  val agentPath = AgentPath("AGENT")
  private val logger = Logger[this.type]

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
}