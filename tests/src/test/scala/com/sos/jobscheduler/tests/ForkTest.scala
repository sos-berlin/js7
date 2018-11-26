package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.EventSeq
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderBroken, OrderCanceled, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.MasterCommand.CancelOrder
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, script}
import com.sos.jobscheduler.tests.ForkTest._
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.language.higherKinds

final class ForkTest extends FreeSpec with DirectoryProvider.ForScalaTest
{
  protected val agentPaths = AAgentPath :: BAgentPath :: Nil
  override protected val masterConfig = ConfigFactory.parseString(
    s"""jobscheduler.TEST-ONLY.suppress-order-id-check-for = "DUPLICATE/🥕" """)

  override def beforeAll() = {
    directoryProvider.master.writeJson(TestWorkflow.withoutVersion)
    directoryProvider.master.writeJson(Workflow(
      DuplicateWorkflowId,
      Vector(
        Execute(WorkflowJob(AAgentPath, ExecutablePath("/SLOW"))))).withoutVersion)
    directoryProvider.agents(0).writeExecutable(ExecutablePath("/SLOW"), script(60.s))
    for (a ← directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(100.ms))
    super.beforeAll()
  }

  "Events" in {
    master.addOrderBlocking(TestOrder)
    master.eventWatch.await[OrderFinished](_.key == TestOrder.id)
    master.eventWatch.all[OrderEvent] match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val keyedEvents = stampeds.map(_.value).toVector
        for (orderId ← Array(TestOrder.id, XOrderId, YOrderId)) {  // But ordering if each order is determined
          assert(keyedEvents.filter(_.key == orderId) == ExpectedEvents.filter(_.key == orderId))
        }
        assert(keyedEvents.toSet == ExpectedEvents.toSet)  // XOrderId and YOrderId run in parallel and ordering is not determined
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }

  "Existing child OrderId yield broken (and cancelable) order" in {
    val order = TestOrder.copy(id = OrderId("DUPLICATE"))
    master.addOrderBlocking(FreshOrder(OrderId("DUPLICATE/🥕"), DuplicateWorkflowId.path))  // Invalid syntax is allowed for this OrderId, check is suppressed
    master.eventWatch.await[OrderProcessingStarted](_.key == OrderId("DUPLICATE/🥕"))

    master.addOrderBlocking(order)
    val expectedBroken = OrderBroken(Problem(
      "Forked OrderIds duplicate existing Order(Order:DUPLICATE/🥕,Workflow:/DUPLICATE (initial)/#0,Processing,Some(Attached(Agent:/AGENT-A (initial))),None,Payload(),false)"))
    assert(master.eventWatch.await[OrderBroken](_.key == order.id).head.value.event == expectedBroken)

    master.executeCommandAsSystemUser(CancelOrder(order.id)).await(99.s).orThrow
    master.eventWatch.await[OrderCanceled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(TestWorkflow.id, None, order.payload),
      OrderStarted,
      expectedBroken,
      OrderCanceled))

    // Kill SLOW job
    agents(0).executeCommand(AgentCommand.Terminate(sigkillProcessesAfter = Some(0.seconds))).await(99.s).orThrow
    agents(0).terminated await 99.s
  }
}

object ForkTest {
  private val DuplicateWorkflowId = WorkflowPath("/DUPLICATE") % "(initial)"
  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflow.id.path, payload = Payload(Map("VARIABLE" → "VALUE")))
  private val XOrderId = OrderId(s"🔺/🥕")
  private val YOrderId = OrderId(s"🔺/🍋")

  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestWorkflow.id, None, Payload(Map("VARIABLE" → "VALUE"))),

    TestOrder.id <-: OrderStarted,
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
      XOrderId <-: OrderAttachable(AAgentId.path),                             YOrderId <-: OrderAttachable(AAgentId.path),
      XOrderId <-: OrderTransferredToAgent(AAgentId),                          YOrderId <-: OrderTransferredToAgent(AAgentId),

      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(0, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(0, "🍋", 1)),

      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(1)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
      XOrderId <-: OrderAttachable(AAgentId.path),                             YOrderId <-: OrderAttachable(AAgentId.path),
      XOrderId <-: OrderTransferredToAgent(AAgentId),                          YOrderId <-: OrderTransferredToAgent(AAgentId),

      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(1, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(1, "🍋", 1)),

      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(2)),

    TestOrder.id <-: OrderAttachable(BAgentId.path),
    TestOrder.id <-: OrderTransferredToAgent(BAgentId),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(3)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderDetachable,
                                                                               YOrderId <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderAttachable(AAgentId.path),
                                                                               YOrderId <-: OrderTransferredToAgent(AAgentId),

      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(3, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(3, "🍋", 1)),

                                                                               YOrderId <-: OrderDetachable,
                                                                               YOrderId <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderAttachable(BAgentId.path),
                                                                               YOrderId <-: OrderTransferredToAgent(BAgentId),

                                                                               YOrderId <-: OrderProcessingStarted,
                                                                               YOrderId <-: OrderStdoutWritten(StdoutOutput),
                                                                               YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
                                                                               YOrderId <-: OrderMoved(Position(3, "🍋", 2)),

      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(4)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
      XOrderId <-: OrderAttachable(AAgentId.path),                             YOrderId <-: OrderAttachable(BAgentId.path),
      XOrderId <-: OrderTransferredToAgent(AAgentId),                          YOrderId <-: OrderTransferredToAgent(BAgentId),

      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(4, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(4, "🍋", 1)),

      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(5)),
    TestOrder.id <-: OrderFinished)
}
