package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.common.configutils.Configs._
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.data.agent.AgentName
import js7.data.event.{EventId, EventRequest, EventSeq}
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCatched, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderRetrying, OrderStarted}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.position.BranchId.{Else, Then, catch_, try_}
import js7.data.workflow.position.Position
import js7.tests.RetryTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final class RetryTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"js7.journal.simulate-sync = 10ms"  // Avoid excessive syncs in case of test failure
  override protected val agentConfig = config"js7.journal.simulate-sync = 10ms"  // Avoid excessive syncs in case of test failure
  protected val agentNames = TestAgentName :: Nil
  protected val inventoryItems = Nil

  import controller.eventWatch

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
      a.writeExecutable(ExecutablePath(s"/OKAY$sh"), ":")
      a.writeExecutable(ExecutablePath(s"/FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
      a.writeExecutable(ExecutablePath(s"/FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
    }
    super.beforeAll()
  }

  "Nested try catch" in {
    val workflowNotation = s"""
       |define workflow {
       |  try execute executable="/FAIL-1$sh", agent="AGENT";   // :0/try:0
       |  catch                                                 // :0/catch
       |    if (catchCount < 2)                                 // :0/catch:0
       |      try retry;                                        // :0/catch:0/then:0/try:0
       |      catch {}                                          // :0/catch:0/then:0/catch
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("/TEST"), workflowNotation).orThrow
    val versionId = updateRepo(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderAttachable(TestAgentName),
      OrderAttached(TestAgentName),
      OrderStarted,

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCatched(Position(0) / catch_(0) % 0 / Then % 0 / try_(0) % 0),

      OrderRetrying(Position(0) / try_(1) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCatched(Position(1)),   // Retry limit reached

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
       |      execute executable="/OKAY$sh", agent="AGENT";       // :0/try:0/try:0
       |      try {                                               // :0/try:0/try:1
       |        execute executable="/FAIL-1$sh", agent="AGENT";   // :0/try:0/try:1/try:0   OrderCatched
       |        execute executable="/OKAY$sh", agent="AGENT";     // :0/try:0/try:1/try:1   skipped
       |      } catch if (catchCount < 3) retry else fail;        // :0/try:0/try:1/catch:0
       |      execute executable="/OKAY$sh", agent="AGENT";       // :0/try:0/try:2
       |    } catch if (catchCount < 2) retry else fail;
       |  } catch execute executable="/OKAY$sh", agent="AGENT";   // :0/catch:0
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("/TEST"), workflowNotation).orThrow
    val versionId = updateRepo(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0),
      OrderAttachable(TestAgentName),
      OrderAttached(TestAgentName),
      OrderStarted,

      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(0) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCatched(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(0) % 0 / Then % 0),

      OrderRetrying(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(1) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCatched(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(1) % 0 / Then % 0),

      OrderRetrying(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(2) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCatched(Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(2) % 0 / Else % 0),   // Retry limit reached

      OrderCatched(Some(Outcome.failed), Position(0) / try_(0) % 0 / catch_(0) % 0 / Then % 0),

      OrderRetrying(Position(0) / try_(0) % 0 / try_(1) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(0) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCatched(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(0) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(1) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCatched(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(1) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(2) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(NamedValues.rc(1))),
      OrderCatched(Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(2) % 0 / Else % 0),  // Retry limit reached

      OrderCatched(Some(Outcome.failed), Position(0) / try_(0) % 0 / catch_(1) % 0 / Else % 0),  // Retry limit reached

      OrderCatched(Some(Outcome.failed), Position(0) / catch_(0) % 0),

      OrderProcessingStarted,
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

  "retryDelay" in {
    val workflowNotation = s"""
       |define workflow {
       |  try (retryDelays=[2, 0]) execute executable="/FAIL-1$sh", agent="AGENT";
       |  catch if (catchCount < 4) retry else fail;
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("/TEST"), workflowNotation).orThrow
    updateRepo(change = workflow :: Nil)

    val orderId = OrderId("⭕")
    val afterEventId = eventWatch.lastAddedEventId
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    eventWatch.await[OrderFailed](_.key == orderId, after = afterEventId)

    val EventSeq.NonEmpty(stamped) = eventWatch.when(EventRequest.singleClass[OrderProcessingStarted](after = afterEventId)).await(9.seconds)
    logger.debug(0.to(2).map(i => (stamped(i+1).timestamp - stamped(i).timestamp).pretty).mkString(" "))
    assert(stamped(1).timestamp - stamped(0).timestamp > 2.second)     // First retry after a second
    assert(stamped(2).timestamp - stamped(1).timestamp < 1.5.second)   // Following retries immediately (0 seconds)
    assert(stamped(3).timestamp - stamped(2).timestamp < 1.5.second)
  }

  "maxTries=3, special handling of 'catch retry'" in {
    val workflowNotation = s"""
       |define workflow {
       |  try (maxTries=3) fail;
       |  catch retry;
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("/TEST"), workflowNotation).orThrow
    val versionId = updateRepo(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderStarted,

      OrderCatched(Some(Outcome.failed), Position(0) / catch_(0) % 0),
      OrderRetrying(Position(0) / try_(1) % 0),

      OrderCatched(Some(Outcome.failed), Position(0) / catch_(1) % 0),
      OrderRetrying(Position(0) / try_(2) % 0),

      // No OrderCatched here! OrderFailed has Outcome of last failed instruction in try block
      OrderFailed(Some(Outcome.failed)))

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
    val workflow = WorkflowParser.parse(WorkflowPath("/TEST"), workflowNotation).orThrow
    val versionId = updateRepo(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path ~ versionId),
      OrderMoved(Position(0) / try_(0) % 0),
      OrderStarted,

      OrderCatched(Some(Outcome.failed), Position(0) / catch_(0) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(1) % 0),

      OrderCatched(Some(Outcome.failed), Position(0) / catch_(1) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(2) % 0),

      OrderCatched(Some(Outcome.failed), Position(0) / catch_(2) % 0 / Then % 0),
      OrderFailed())

    val orderId = OrderId("🔵")
    val afterEventId = eventWatch.lastAddedEventId
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    awaitAndCheckEventSeq[OrderFailed](afterEventId, orderId, expectedEvents)
  }

  private def awaitAndCheckEventSeq[E <: OrderEvent: ClassTag: TypeTag](after: EventId, orderId: OrderId, expected: Vector[OrderEvent]): Unit =
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
  private val TestAgentName = AgentName("AGENT")
  private val logger = Logger(getClass)
}
