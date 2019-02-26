package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCatched, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderRetrying, OrderStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.If.{Else, Then}
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction.{Catch_, Try_}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.RetryTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.reflect.ClassTag

final class RetryTest extends FreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = TestAgentRefPath :: Nil
  protected val fileBased = Nil

  override def beforeAll() = {
    for (a ← directoryProvider.agents) {
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
       |    if (retryCount < 1)                                 // :0/catch:0
       |      try retry;                                        // :0/catch:0/then:0/try:0
       |      catch {}                                          // :0/catch:0/then:0/catch
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("/TEST"), workflowNotation).orThrow
    val versionId = updateRepo(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path % versionId),
      OrderMoved(Position(0) / Try_ % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Catch_ % 0 / Then % 0 / Try_ % 0),

      OrderRetrying(Position(0) / 1 % 0),

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(1)),   // Retry limit reached

      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished)

    val orderId = OrderId("🔺")
    val afterEventId = master.eventWatch.lastAddedEventId
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
       |      } catch if (retryCount < 2) retry else fail;        // :0/try:0/try:1/catch:0
       |      execute executable="/OKAY$sh", agent="AGENT";       // :0/try:0/try:2
       |    } catch if (retryCount < 1) retry else fail;
       |  } catch execute executable="/OKAY$sh", agent="AGENT";   // :0/catch:0
       |}""".stripMargin
    val workflow = WorkflowParser.parse(WorkflowPath("/TEST"), workflowNotation).orThrow
    val versionId = updateRepo(change = workflow :: Nil)

    val expectedEvents = Vector(
      OrderAdded(workflow.path % versionId),
      OrderMoved(Position(0) / Try_ % 0 / Try_ % 0),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
      OrderMoved(Position(0) / Try_ % 0 / Try_ % 1 / Try_ % 0),

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Try_ % 0 / Try_ % 1 / Catch_ % 0 / Then % 0),

      OrderRetrying(Position(0) / Try_ % 0 / Try_ % 1 / 1 % 0),

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Try_ % 0 / Try_ % 1 / -1 % 0 / Then % 0),

      OrderRetrying(Position(0) / Try_ % 0 / Try_ % 1 / 2 % 0),

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Try_ % 0 / Try_ % 1 / -2 % 0 / Else % 0),   // Retry limit reached

      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Try_ % 0 / Catch_ % 0 / Then % 0),

      OrderRetrying(Position(0) / Try_ % 0 / 1 % 0),

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
      OrderMoved(Position(0) / Try_ % 0 / 1 % 1 / Try_ % 0),

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Try_ % 0 / 1 % 1 / Catch_ % 0 / Then % 0),
      OrderRetrying(Position(0) / Try_ % 0 / 1 % 1 / 1 % 0),

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Try_ % 0 / 1 % 1 / -1 % 0 / Then % 0),
      OrderRetrying(Position(0) / Try_ % 0 / 1 % 1 / 2 % 0),

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Try_ % 0 / 1 % 1 / -2 % 0 / Else % 0),  // Retry limit reached

      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Try_ % 0 / -1 % 0 / Else % 0),  // Retry limit reached

      OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0) / Catch_ % 0),

      OrderProcessingStarted,
      OrderProcessed(MapDiff.empty, Outcome.succeeded),
      OrderMoved(Position(1)),

      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished)

    val orderId = OrderId("🔷")
    val afterEventId = master.eventWatch.lastAddedEventId
    master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    awaitAndCheckEventSeq[OrderFinished](afterEventId, orderId, expectedEvents)
  }

  private def awaitAndCheckEventSeq[E <: OrderEvent: ClassTag](after: EventId, orderId: OrderId, expected: Vector[OrderEvent]): Unit = {
    master.eventWatch.await[E](_.key == orderId, after = after)
    master.eventWatch.when[OrderEvent](EventRequest.singleClass(after = after)) await 99.seconds match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == expected)
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object RetryTest {
  private val TestAgentRefPath = AgentRefPath("/AGENT")
}
