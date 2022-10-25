package js7.tests

import izumi.reflect.Tag
import js7.base.configutils.Configs.*
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.event.{EventId, EventRequest, EventSeq}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCaught, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderRetrying, OrderStarted, OrderStepFailed}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.position.BranchId.{Else, Then, catch_, try_}
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.RetryTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.*
import scala.reflect.ClassTag

final class RetryTest extends OurTestSuite with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"js7.journal.simulate-sync = 10ms"  // Avoid excessive syncs in case of test failure
  override protected val agentConfig = config"js7.journal.simulate-sync = 10ms"  // Avoid excessive syncs in case of test failure
  protected val agentPaths = TestAgentPath :: Nil
  protected val items = Nil

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(RelativePathExecutable(s"OKAY$sh"), ":")
      a.writeExecutable(RelativePathExecutable(s"FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
      a.writeExecutable(RelativePathExecutable(s"FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
    }
    super.beforeAll()
  }

  "Nested try catch" in {
    val workflowNotation = s"""
       |define workflow {
       |  try execute executable="FAIL-1$sh", agent="AGENT";   // :0/try:0
       |  catch                                                 // :0/catch
       |    if (catchCount < 2)                                 // :0/catch:0
       |      try retry;                                        // :0/catch:0/then:0/try:0
       |      catch {}                                          // :0/catch:0/then:0/catch
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("TEST"), workflowNotation).orThrow
    val versionId = updateVersionedItems(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderAttachable(TestAgentPath),
      OrderAttached(TestAgentPath),
      OrderStarted,

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / catch_(0) % 0),
      OrderMoved(Position(0) / catch_(0) % 0 / Then % 0 / try_(0) % 0),

      OrderRetrying(Position(0) / try_(1) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / catch_(1) % 0),   // Retry limit reached
      OrderMoved(Position(1)),

      OrderDetachable,
      OrderDetached,
      OrderFinished)

    val orderId = OrderId("🔺")
    val afterEventId = eventWatch.lastAddedEventId
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    awaitAndCheckEventSeq[OrderFinished](afterEventId, orderId, expectedEvents)
  }

  "Nested try catch with outer non-failing catch" in {
    val workflowNotation = s"""
       |define workflow {
       |  try {                                                   // :0
       |    try {                                                 // :0/try:0
       |      execute executable="OKAY$sh", agent="AGENT";       // :0/try:0/try:0
       |      try {                                               // :0/try:0/try:1
       |        execute executable="FAIL-1$sh", agent="AGENT";   // :0/try:0/try:1/try:0   OrderCaught
       |        execute executable="OKAY$sh", agent="AGENT";     // :0/try:0/try:1/try:1   skipped
       |      } catch if (catchCount < 3) retry else fail;        // :0/try:0/try:1/catch:0
       |      execute executable="OKAY$sh", agent="AGENT";       // :0/try:0/try:2
       |    } catch if (catchCount < 2) retry else fail;
       |  } catch execute executable="OKAY$sh", agent="AGENT";   // :0/catch:0
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("TEST"), workflowNotation).orThrow
    val versionId = updateVersionedItems(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0),
      OrderAttachable(TestAgentPath),
      OrderAttached(TestAgentPath),
      OrderStarted,

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(0) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(0) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(0) % 0 / Then % 0),

      OrderRetrying(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(1) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(1) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(1) % 0 / Then % 0),

      OrderRetrying(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(2) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(2) % 0),   // Retry limit reached
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(2) % 0 / Else % 0),

      OrderStepFailed(Outcome.failed),
      OrderCaught(Position(0) / try_(0) % 0 / catch_(0) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / catch_(0) % 0 / Then % 0),

      OrderRetrying(Position(0) / try_(0) % 0 / try_(1) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(0) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(0) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(0) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(1) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(1) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(1) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(2) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCaught(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(2) % 0),  // Retry limit reached
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(2) % 0 / Else % 0),

      OrderStepFailed(Outcome.failed),  // Retry limit reached
      OrderCaught(Position(0) / try_(0) % 0 / catch_(1) % 0),
      OrderMoved(Position(0) / try_(0) % 0 / catch_(1) % 0 / Else % 0),

      OrderStepFailed(Outcome.failed),
      OrderCaught(Position(0) / catch_(0) % 0),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),

      OrderDetachable,
      OrderDetached,
      OrderFinished)

    val orderId = OrderId("🔷")
    val afterEventId = eventWatch.lastAddedEventId
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    awaitAndCheckEventSeq[OrderFinished](afterEventId, orderId, expectedEvents)
  }

  // For test of retryDelays see RetryDelayTest

  "maxTries=3, special handling of 'catch retry'" in {
    val workflowNotation = s"""
       |define workflow {
       |  try (maxTries=3) fail;
       |  catch retry;
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("TEST"), workflowNotation).orThrow
    val versionId = updateVersionedItems(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderStarted,

      OrderStepFailed(Outcome.failed),
      OrderCaught(Position(0) / catch_(0) % 0),
      OrderRetrying(Position(0) / try_(1) % 0),

      OrderStepFailed(Outcome.failed),
      OrderCaught(Position(0) / catch_(1) % 0),
      OrderRetrying(Position(0) / try_(2) % 0),

      // No OrderCaught here! OrderFailed has Outcome of last failed instruction in try block
      OrderStepFailed(Outcome.failed),
      OrderFailed(Position(0) / try_(2) % 0))

    val orderId = OrderId("🔶")
    val afterEventId = eventWatch.lastAddedEventId
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    awaitAndCheckEventSeq[OrderFailed](afterEventId, orderId, expectedEvents)
  }

  "maxTries=3, standard handling, stopping at retry instruction" in {
    val workflowNotation = s"""
       |define workflow {
       |  try (maxTries=3) fail;
       |  catch if (true) retry;
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("TEST"), workflowNotation).orThrow
    val versionId = updateVersionedItems(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderStarted,

      OrderStepFailed(Outcome.failed),
      OrderCaught(Position(0) / catch_(0) % 0),
      OrderMoved(Position(0) / catch_(0) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(1) % 0),

      OrderStepFailed(Outcome.failed),
      OrderCaught(Position(0) / catch_(1) % 0),
      OrderMoved(Position(0) / catch_(1) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(2) % 0),

      OrderStepFailed(Outcome.failed),
      OrderCaught(Position(0) / catch_(2) % 0),
      OrderMoved(Position(0) / catch_(2) % 0 / Then % 0),
      OrderFailed(Position(0) / catch_(2) % 0 / Then % 0))

    val orderId = OrderId("🔵")
    val afterEventId = eventWatch.lastAddedEventId
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    awaitAndCheckEventSeq[OrderFailed](afterEventId, orderId, expectedEvents)
  }

  private def awaitAndCheckEventSeq[E <: OrderEvent: ClassTag: Tag](after: EventId, orderId: OrderId, expected: Vector[OrderEvent]): Unit =
  {
    eventWatch.await[E](_.key == orderId, after = after)
    sleep(50.millis)  // No more events should arrive
    eventWatch.when[OrderEvent](EventRequest.singleClass(after = after)) await 99.seconds match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == expected)
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object RetryTest
{
  private val TestAgentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(TestAgentPath)
  private val logger = Logger(getClass)
}
