package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCatched, OrderDetachable, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderRetrying, OrderStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.BranchId.{Else, Then, catch_, try_}
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.RetryTest._
import com.sos.jobscheduler.tests.testenv.MasterAgentForScalaTest
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final class RetryTest extends FreeSpec with MasterAgentForScalaTest
{
  override protected val masterConfig = ConfigFactory.parseString(
    s"""jobscheduler.journal.simulate-sync = 10ms""")  // Avoid excessive syncs in case of test failure
  override protected val agentConfig = ConfigFactory.parseString(
    s"""jobscheduler.journal.simulate-sync = 10ms""")  // Avoid excessive syncs in case of test failure
  protected val agentRefPaths = TestAgentRefPath :: Nil
  protected val fileBased = Nil

  import master.eventWatch

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
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / catch_(0) % 0 / Then % 0 / try_(0) % 0),

      OrderRetrying(Position(0) / try_(1) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(1)),   // Retry limit reached

      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished)

    val orderId = OrderId("🔺")
    val afterEventId = eventWatch.lastAddedEventId
    master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
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
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,

      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
      OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(0) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(0) % 0 / Then % 0),

      OrderRetrying(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(1) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(1) % 0 / Then % 0),

      OrderRetrying(Position(0) / try_(0) % 0 / try_(0) % 1 / try_(2) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / try_(0) % 0 / try_(0) % 1 / catch_(2) % 0 / Else % 0),   // Retry limit reached

      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / try_(0) % 0 / catch_(0) % 0 / Then % 0),

      OrderRetrying(Position(0) / try_(0) % 0 / try_(1) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
      OrderMoved(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(0) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(0) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(1) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(1) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(0) % 0 / try_(1) % 1 / try_(2) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / try_(0) % 0 / try_(1) % 1 / catch_(2) % 0 / Else % 0),  // Retry limit reached

      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / try_(0) % 0 / catch_(1) % 0 / Else % 0),  // Retry limit reached

      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / catch_(0) % 0),

      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),

      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished)

    val orderId = OrderId("🔷")
    val afterEventId = eventWatch.lastAddedEventId
    master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
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
    master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
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

      OrderCatched(Outcome.Failed(ReturnCode(0)), Position(0) / catch_(0) % 0),
      OrderRetrying(Position(0) / try_(1) % 0),

      OrderCatched(Outcome.Failed(ReturnCode(0)), Position(0) / catch_(1) % 0),
      OrderRetrying(Position(0) / try_(2) % 0),

      // No OrderCatched here! OrderFailed has Outcome of last failed instruction in try block
      OrderFailed(Outcome.Failed(ReturnCode(0))))

    val orderId = OrderId("🔶")
    val afterEventId = eventWatch.lastAddedEventId
    master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
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

      OrderCatched(Outcome.Failed(ReturnCode(0)), Position(0) / catch_(0) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(1) % 0),

      OrderCatched(Outcome.Failed(ReturnCode(0)), Position(0) / catch_(1) % 0 / Then % 0),
      OrderRetrying(Position(0) / try_(2) % 0),

      OrderCatched(Outcome.Failed(ReturnCode(0)), Position(0) / catch_(2) % 0 / Then % 0),
      OrderFailed(Outcome.Disrupted(Problem("Retry stopped because maxRetries=3 has been reached"))))

    val orderId = OrderId("🔵")
    val afterEventId = eventWatch.lastAddedEventId
    master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
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
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val logger = Logger(getClass)
}
