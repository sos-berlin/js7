package js7.tests

import js7.agent.data.commands.AgentCommand
import js7.base.configutils.Configs._
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.thread.Futures.implicits._
import js7.base.time.ScalaTime._
import js7.common.scalautil.MonixUtils.syntax._
import js7.data.command.CancelMode
import js7.data.controller.ControllerCommand.{CancelOrders, RemoveOrdersWhenTerminated}
import js7.data.event.EventSeq
import js7.data.job.{PathExecutable, RelativePathExecutable}
import js7.data.order.OrderEvent._
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.test.ForkTestSetting._
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ForkTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.{StdoutOutput, script}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class ForkTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = AAgentId :: BAgentId :: Nil
  override protected val controllerConfig = config"""
    js7.TEST-ONLY.suppress-order-id-check-for = "DUPLICATE|🥕" """
  protected val versionedItems = TestWorkflow :: DuplicateWorkflow :: Nil

  override def beforeAll() = {
    directoryProvider.agents(0).writeExecutable(RelativePathExecutable("SLOW.cmd"), script(60.s))
    for (a <- directoryProvider.agents) a.writeExecutable(TestPathExecutable, script(100.ms))
    super.beforeAll()
  }

  "Events" in {
    controller.addOrderBlocking(TestOrder)
    controller.eventWatch.await[OrderFinished](_.key == TestOrder.id)
    controller.executeCommandAsSystemUser(RemoveOrdersWhenTerminated(Seq(TestOrder.id))).await(99.s).orThrow
    controller.eventWatch.all[OrderEvent] match {
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

  "Existing child OrderId yields broken (and cancelable) order" in {
    val order = TestOrder.copy(id = OrderId("DUPLICATE"))
    controller.addOrderBlocking(FreshOrder.unchecked(OrderId("DUPLICATE|🥕"), DuplicateWorkflow.id.path))  // Invalid syntax is allowed for this OrderId, check is suppressed
    controller.eventWatch.await[OrderProcessingStarted](_.key == OrderId("DUPLICATE|🥕"))

    controller.addOrderBlocking(order)
    val expectedBroken = OrderBroken(Problem(
      "Forked OrderIds duplicate existing Order(Order:DUPLICATE|🥕,DUPLICATE~INITIAL:0,Processing,Map(),Vector(),Some(Attached(AGENT-A)),None,None,false,false)"))
    assert(controller.eventWatch.await[OrderBroken](_.key == order.id).head.value.event == expectedBroken)

    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), CancelMode.FreshOrStarted())).await(99.s).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(TestWorkflow.id, order.arguments),
      OrderStarted,
      expectedBroken,
      OrderCancelled))

    controller.terminate() await 99.s
    // Kill SLOW job
    agents(0).executeCommandAsSystemUser(AgentCommand.ShutDown(Some(SIGKILL))).await(99.s).orThrow
    agents(0).terminated await 99.s
  }
}

object ForkTest {
  private val DuplicateWorkflow = Workflow(
    WorkflowPath("DUPLICATE") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(AAgentId, PathExecutable("SLOW.cmd")))))
  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflow.id.path, Map("KEY" -> StringValue("VALUE")))
  private val XOrderId = OrderId(s"🔺|🥕")
  private val YOrderId = OrderId(s"🔺|🍋")

  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestWorkflow.id, Map("KEY" -> StringValue("VALUE"))),

    TestOrder.id <-: OrderStarted,
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
      XOrderId <-: OrderAttachable(AAgentId),                 YOrderId <-: OrderAttachable(AAgentId),
      XOrderId <-: OrderAttached(AAgentId),                   YOrderId <-: OrderAttached(AAgentId),

      XOrderId <-: OrderProcessingStarted,                    YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),          YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(Outcome.succeededRC0),      YOrderId <-: OrderProcessed(Outcome.succeededRC0),
      XOrderId <-: OrderMoved(Position(0) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(0) / "fork+🍋" % 1),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(1)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
      XOrderId <-: OrderAttachable(AAgentId),                 YOrderId <-: OrderAttachable(AAgentId),
      XOrderId <-: OrderAttached(AAgentId),                   YOrderId <-: OrderAttached(AAgentId),

      XOrderId <-: OrderProcessingStarted,                    YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),          YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(Outcome.succeededRC0),      YOrderId <-: OrderProcessed(Outcome.succeededRC0),
      XOrderId <-: OrderMoved(Position(1) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(1) / "fork+🍋" % 1),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(2)),

    TestOrder.id <-: OrderAttachable(BAgentId),
    TestOrder.id <-: OrderAttached(BAgentId),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(Outcome.succeededRC0),
    TestOrder.id <-: OrderMoved(Position(3)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderDetached,
                                                              YOrderId <-: OrderDetachable,
                                                              YOrderId <-: OrderDetached,
                                                              YOrderId <-: OrderAttachable(AAgentId),
                                                              YOrderId <-: OrderAttached(AAgentId),

      XOrderId <-: OrderProcessingStarted,                    YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),          YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(Outcome.succeededRC0),      YOrderId <-: OrderProcessed(Outcome.succeededRC0),
      XOrderId <-: OrderMoved(Position(3) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(3) / "fork+🍋" % 1),

                                                              YOrderId <-: OrderDetachable,
                                                              YOrderId <-: OrderDetached,
                                                              YOrderId <-: OrderAttachable(BAgentId),
                                                              YOrderId <-: OrderAttached(BAgentId),

                                                              YOrderId <-: OrderProcessingStarted,
                                                              YOrderId <-: OrderStdoutWritten(StdoutOutput),
                                                              YOrderId <-: OrderProcessed(Outcome.succeededRC0),
                                                              YOrderId <-: OrderMoved(Position(3) / "fork+🍋" % 2),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(4)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
      XOrderId <-: OrderAttachable(AAgentId),                 YOrderId <-: OrderAttachable(BAgentId),
      XOrderId <-: OrderAttached(AAgentId),                   YOrderId <-: OrderAttached(BAgentId),

      XOrderId <-: OrderProcessingStarted,                    YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),          YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(Outcome.succeededRC0),      YOrderId <-: OrderProcessed(Outcome.succeededRC0),
      XOrderId <-: OrderMoved(Position(4) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(4) / "fork+🍋" % 1),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(5)),
    TestOrder.id <-: OrderFinished,
    TestOrder.id <-: OrderRemoved)
}
