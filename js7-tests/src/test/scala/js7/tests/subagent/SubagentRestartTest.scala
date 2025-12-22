package js7.tests.subagent

import js7.agent.TestAgent
import js7.base.Problems.MessageSignedByUnknownProblem
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.core.command.CommandMeta
import js7.data.agent.AgentRefStateEvent.AgentReady
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentCommand}
import js7.data.value.expression.Expression.{StringConstant, expr}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.subagent.SubagentRestartTest.*
import js7.tests.subagent.SubagentTester.agentPath
import scala.collection.View
import scala.compiletime.uninitialized

final class SubagentRestartTest extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  protected lazy val items = Seq(workflow, bareSubagentItem)
  override protected val primarySubagentsDisabled = true

  private var myAgent: TestAgent = uninitialized

  override def beforeAll() =
    super.beforeAll()
    myAgent = agent

  override def afterAll() =
    try
      controller.terminate().await(99.s)
      for a <- Option(myAgent) do a.terminate().await(99.s)
    finally
      super.afterAll()

  "Reject items if no signature keys are installed" in:
    runSubagent(bareSubagentItem, suppressSignatureKeys = true): _ =>
      val orderId = OrderId("ITEM-SIGNATURE")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))

      val started = controller.awaitNextKey[OrderProcessingStarted](orderId).head.value
      assert(started == OrderProcessingStarted(bareSubagentItem.id))

      val processed = controller.awaitNextKey[OrderProcessed](orderId).head.value
      assert(processed == OrderProcessed(OrderOutcome.Disrupted(MessageSignedByUnknownProblem)))

  "Restart Director" in:
    val orderId = OrderId("RESTART-DIRECTOR")

    runSubagent(bareSubagentItem): _ =>
      locally:
        controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
        val events = controller.awaitNextKey[OrderProcessingStarted](orderId)
        assert(events.head.value == OrderProcessingStarted(bareSubagentItem.id))

        // STOP DIRECTOR
        myAgent.terminate().await(99.s)

      TestSemaphoreJob.continue()

      locally:
        controller.eventWatch.allKeyedEvents[OrderProcessed] foreach:
          case ke @ KeyedEvent(`orderId`, OrderProcessed(_)) => fail(s"Unexpected $ke")
          case _ =>

        // START DIRECTOR
        myAgent = directoryProvider.startAgent(agentPath).await(99.s)
        controller.awaitNextKey[OrderProcessed](orderId)
        val events = controller.awaitNextKey[OrderTerminated](orderId)
        assert(events.head.value.isInstanceOf[OrderFinished])

  "Restart remote Subagent while a job is running" in:
    controller.resetLastWatchedEventId()
    val aOrderId = OrderId("A-RESTART-SUBAGENT")

    TestSemaphoreJob.reset()

    runSubagent(bareSubagentItem): subagent =>
      controller.addOrderBlocking(FreshOrder(aOrderId, workflow.path))

      val started = controller.awaitNextKey[OrderProcessingStarted](aOrderId).head.value
      assert(started == OrderProcessingStarted(bareSubagentItem.id))

      val written = controller.awaitNextKey[OrderStdoutWritten](aOrderId).head.value
      assert(written == OrderStdoutWritten("TestSemaphoreJob\n"))

      // For this test, the terminating Subagent must not emit any event before shutdown
      subagent.journal.stopEventWatch()
      subagent.shutdown(
        SubagentCommand.ShutDown(Some(SIGKILL), dontWaitForDirector = true),
        CommandMeta.test
      ).await(99.s)

    // Subagent is unreachable now
    val eventId = controller.lastAddedEventId
    val bOrderId = OrderId("B-RESTART-SUBAGENT")
    controller.addOrderBlocking(FreshOrder(bOrderId, workflow.path))

    runSubagent(bareSubagentItem): _ =>
      locally:
        val events = controller.awaitNextKey[OrderProcessed](aOrderId)
        assert(events.head.value == OrderProcessed.processLostDueToRestart)

        // OrderProcessed must be followed by OrderMoved
        controller.awaitNextKey[OrderMoved](aOrderId)
      locally:
        sleep(4.s)
        TestSemaphoreJob.continue(2)
        controller.awaitNextKey[OrderProcessingStarted](aOrderId)
        for orderId <- View(aOrderId, bOrderId) do
          val events = controller.awaitKey[OrderTerminated](orderId, after = eventId)
          assert(events.head.value.isInstanceOf[OrderFinished])

  "Restart remote Subagent while a job is running, use SubagentBundle" in:
    controller.resetLastWatchedEventId()
    TestSemaphoreJob.reset()
    val aOrderId = OrderId("A-RESTART-BUNDLE")

    val subagentBundle = SubagentBundle(
      SubagentBundleId("BUNDLE"),
      Map(bareSubagentId -> expr"1"))

    val workflow = Workflow(
      WorkflowPath("BUNDLE-WORKFLOW"),
      Seq(
        TestSemaphoreJob.execute(
          agentPath,
          subagentBundleId = Some(StringConstant(subagentBundle.id.string)))))

    // Workflow cannot be deleted due to SubagentBundle ???
    //withItems((subagentBundle, workflow)): (subagentBundle, workflow) =>
    updateItems(subagentBundle, workflow)
    runSubagent(bareSubagentItem): subagent =>
      controller.addOrderBlocking(FreshOrder(aOrderId, workflow.path))

      val started = controller.awaitNextKey[OrderProcessingStarted](aOrderId).head.value
      assert(started == OrderProcessingStarted(Some(bareSubagentItem.id), Some(subagentBundle.id)))

      val written = controller.awaitNextKey[OrderStdoutWritten](aOrderId).head.value
      assert(written == OrderStdoutWritten("TestSemaphoreJob\n"))

      // For this test, the terminating Subagent must not emit any event before shutdown
      subagent.journal.stopEventWatch()
      subagent.shutdown(
        SubagentCommand.ShutDown(Some(SIGKILL), dontWaitForDirector = true),
        CommandMeta.test
      ).await(99.s)

    // Subagent is unreachable now
    val eventId = controller.lastAddedEventId
    val bOrderId = OrderId("B-RESTART-BUNDLE")
    controller.addOrderBlocking(FreshOrder(bOrderId, workflow.path))

    runSubagent(bareSubagentItem): _ =>
      locally:
        val events = controller.awaitNextKey[OrderProcessed](aOrderId)
        assert(events.head.value == OrderProcessed.processLostDueToRestart)

        // OrderProcessed must be followed by OrderMoved
        controller.awaitNextKey[OrderMoved](aOrderId)
      locally:
        sleep(4.s)
        TestSemaphoreJob.continue(2)
        controller.awaitNextKey[OrderProcessingStarted](aOrderId)
        for orderId <- View(aOrderId, bOrderId) do
          val events = controller.awaitKey[OrderTerminated](orderId, after = eventId)
          assert(events.head.value.isInstanceOf[OrderFinished])

  "Restart both Director and remote Subagent while a job is running" in:
    controller.resetLastWatchedEventId()
    val aOrderId = OrderId("A-RESTART-BOTH")

    TestSemaphoreJob.reset()

    runSubagent(bareSubagentItem): subagent =>
      controller.addOrderBlocking(FreshOrder(aOrderId, workflow.path))

      val started = controller.awaitNextKey[OrderProcessingStarted](aOrderId).head.value
      assert(started == OrderProcessingStarted(bareSubagentItem.id))

      val written = controller.awaitNextKey[OrderStdoutWritten](aOrderId).head.value
      assert(written == OrderStdoutWritten("TestSemaphoreJob\n"))

      // For this test, the terminating Subagent must not emit any event before shutdown
      subagent.journal.stopEventWatch()
      subagent.shutdown(
        SubagentCommand.ShutDown(Some(SIGKILL), dontWaitForDirector = true),
        CommandMeta.test
      ).await(99.s)

      // STOP DIRECTOR
      myAgent.terminate().await(99.s)

    // Subagent is unreachable now

    // START DIRECTOR
    val eventId = controller.lastAddedEventId
    myAgent = directoryProvider.startAgent(agentPath).await(99.s)
    controller.awaitNext[AgentReady]()

    val bOrderId = OrderId("B-RESTART-BOTH")
    controller.addOrderBlocking(FreshOrder(bOrderId, workflow.path))

    runSubagent(bareSubagentItem): _ =>
      locally:
        val events = controller.awaitNextKey[OrderProcessed](aOrderId)
        assert(events.head.value == OrderProcessed.processLostDueToRestart)

        // OrderProcessed must be followed by OrderMoved
        controller.awaitNextKey[OrderMoved](aOrderId)
      locally:
        TestSemaphoreJob.continue(2)
        controller.awaitNextKey[OrderProcessingStarted](aOrderId)
        for orderId <- View(aOrderId, bOrderId) do
          val events = controller.await[OrderTerminated](_.key == orderId, after = eventId)
          assert(events.head.value.event.isInstanceOf[OrderFinished])


object SubagentRestartTest:
  private val logger = Logger[this.type]

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      TestSemaphoreJob.execute(agentPath)))

  final class TestSemaphoreJob extends SemaphoreJob(TestSemaphoreJob)
  object TestSemaphoreJob extends SemaphoreJob.Companion[TestSemaphoreJob]
