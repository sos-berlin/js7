package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.event.EventSeq
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.MasterCommand.CancelOrder
import com.sos.jobscheduler.tests.ForkTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.{StdoutOutput, script}
import com.sos.jobscheduler.tests.testenv.DirectoryProviderForScalaTest
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._

final class ForkTest extends FreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = AAgentRefPath :: BAgentRefPath :: Nil
  override protected val masterConfig = ConfigFactory.parseString(
    s"""jobscheduler.TEST-ONLY.suppress-order-id-check-for = "DUPLICATE/🥕" """)
  protected val fileBased = TestWorkflow :: DuplicateWorkflow :: Nil

  override def beforeAll() = {
    directoryProvider.agents(0).writeExecutable(ExecutablePath("/SLOW.cmd"), script(60.s))
    for (a <- directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(100.ms))
    super.beforeAll()
  }

  "Events" in {
    master.addOrderBlocking(TestOrder)
    master.eventWatch.await[OrderFinished](_.key == TestOrder.id)
    master.eventWatch.all[OrderEvent] match {
      case EventSeq.NonEmpty(stampeds) =>
        val keyedEvents = stampeds.map(_.value).toVector
        for (orderId <- Array(TestOrder.id, XOrderId, YOrderId)) {  // But ordering if each order is determined
          assert(keyedEvents.filter(_.key == orderId) == ExpectedEvents.filter(_.key == orderId))
        }
        assert(keyedEvents.toSet == ExpectedEvents.toSet)  // XOrderId and YOrderId run in parallel and ordering is not determined
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }

  "Existing child OrderId yield broken (and cancelable) order" in {
    val order = TestOrder.copy(id = OrderId("DUPLICATE"))
    master.addOrderBlocking(FreshOrder(OrderId("DUPLICATE/🥕"), DuplicateWorkflow.id.path))  // Invalid syntax is allowed for this OrderId, check is suppressed
    master.eventWatch.await[OrderProcessingStarted](_.key == OrderId("DUPLICATE/🥕"))

    master.addOrderBlocking(order)
    val expectedBroken = OrderBroken(Problem(
      "Forked OrderIds duplicate existing Order(Order:DUPLICATE/🥕,Workflow:/DUPLICATE INITIAL:0,Processing,Succeeded(ReturnCode(0)),Some(Attached(AgentRef:/AGENT-A)),None,Payload(),None)"))
    assert(master.eventWatch.await[OrderBroken](_.key == order.id).head.value.event == expectedBroken)

    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted)).await(99.s).orThrow
    master.eventWatch.await[OrderCanceled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(TestWorkflow.id, None, order.payload),
      OrderStarted,
      expectedBroken,
      OrderCanceled))

    master.terminate() await 99.s
    // Kill SLOW job
    agents(0).executeCommand(AgentCommand.Terminate(sigkillProcessesAfter = Some(0.seconds))).await(99.s).orThrow
    agents(0).terminated await 99.s
  }
}

object ForkTest {
  private val DuplicateWorkflow = Workflow(
    WorkflowPath("/DUPLICATE") % "INITIAL",
    Vector(
      Execute(WorkflowJob(AAgentRefPath, ExecutablePath("/SLOW.cmd")))))
  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflow.id.path, payload = Payload(Map("VARIABLE" -> "VALUE")))
  private val XOrderId = OrderId(s"🔺/🥕")
  private val YOrderId = OrderId(s"🔺/🍋")

  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestWorkflow.id, None, Payload(Map("VARIABLE" -> "VALUE"))),

    TestOrder.id <-: OrderStarted,
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
      XOrderId <-: OrderAttachable(AAgentRefPath),                     YOrderId <-: OrderAttachable(AAgentRefPath),
      XOrderId <-: OrderTransferredToAgent(AAgentRefPath),             YOrderId <-: OrderTransferredToAgent(AAgentRefPath),

      XOrderId <-: OrderProcessingStarted,                             YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                   YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),   YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(0, "🥕", 1)),                   YOrderId <-: OrderMoved(Position(0, "🍋", 1)),

      XOrderId <-: OrderDetachable,                                    YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                           YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(1)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
      XOrderId <-: OrderAttachable(AAgentRefPath),                     YOrderId <-: OrderAttachable(AAgentRefPath),
      XOrderId <-: OrderTransferredToAgent(AAgentRefPath),             YOrderId <-: OrderTransferredToAgent(AAgentRefPath),

      XOrderId <-: OrderProcessingStarted,                             YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                   YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),   YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(1, "🥕", 1)),                   YOrderId <-: OrderMoved(Position(1, "🍋", 1)),

      XOrderId <-: OrderDetachable,                                    YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                           YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(2)),

    TestOrder.id <-: OrderAttachable(BAgentRefPath),
    TestOrder.id <-: OrderTransferredToAgent(BAgentRefPath),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(3)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
                                                                       YOrderId <-: OrderDetachable,
                                                                       YOrderId <-: OrderTransferredToMaster,
                                                                       YOrderId <-: OrderAttachable(AAgentRefPath),
                                                                       YOrderId <-: OrderTransferredToAgent(AAgentRefPath),

      XOrderId <-: OrderProcessingStarted,                             YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                   YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),   YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(3, "🥕", 1)),                   YOrderId <-: OrderMoved(Position(3, "🍋", 1)),

                                                                       YOrderId <-: OrderDetachable,
                                                                       YOrderId <-: OrderTransferredToMaster,
                                                                       YOrderId <-: OrderAttachable(BAgentRefPath),
                                                                       YOrderId <-: OrderTransferredToAgent(BAgentRefPath),

                                                                       YOrderId <-: OrderProcessingStarted,
                                                                       YOrderId <-: OrderStdoutWritten(StdoutOutput),
                                                                       YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
                                                                       YOrderId <-: OrderMoved(Position(3, "🍋", 2)),

      XOrderId <-: OrderDetachable,                                    YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                           YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(4)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
      XOrderId <-: OrderAttachable(AAgentRefPath),                     YOrderId <-: OrderAttachable(BAgentRefPath),
      XOrderId <-: OrderTransferredToAgent(AAgentRefPath),             YOrderId <-: OrderTransferredToAgent(BAgentRefPath),

      XOrderId <-: OrderProcessingStarted,                             YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                   YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),   YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(4, "🥕", 1)),                   YOrderId <-: OrderMoved(Position(4, "🍋", 1)),

      XOrderId <-: OrderDetachable,                                    YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                           YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(5)),
    TestOrder.id <-: OrderFinished)
}
