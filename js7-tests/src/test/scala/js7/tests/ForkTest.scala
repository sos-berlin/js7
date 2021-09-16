package js7.tests

import js7.base.configutils.Configs._
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteOrdersWhenTerminated, ResumeOrder}
import js7.data.event.EventSeq
import js7.data.job.{PathExecutable, RelativePathExecutable}
import js7.data.order.OrderEvent._
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fail, Fork}
import js7.data.workflow.position.Position
import js7.data.workflow.test.ForkTestSetting._
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ForkTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.{StdoutOutput, script}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class ForkTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.TEST-ONLY.suppress-order-id-check-for = "DUPLICATE|🥕"
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = AAgentPath :: BAgentPath :: Nil
  protected val items = Seq(TestWorkflow, DuplicateWorkflow,
    joinIfFailedForkWorkflow, failInForkWorkflow)

  override def beforeAll() = {
    directoryProvider.agents(0).writeExecutable(RelativePathExecutable("SLOW.cmd"), script(60.s))
    for (a <- directoryProvider.agents) a.writeExecutable(TestPathExecutable, script(100.ms))
    super.beforeAll()
  }

  "Events" in {
    controller.addOrderBlocking(TestOrder)
    eventWatch.await[OrderFinished](_.key == TestOrder.id)
    controller.executeCommandAsSystemUser(DeleteOrdersWhenTerminated(Seq(TestOrder.id))).await(99.s).orThrow
    eventWatch.all[OrderEvent] match {
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

  "joinIfFailed" in {
    val orderId = OrderId("JOIN-IF-FAILED")
    val childOrderId = orderId / "💥"
    controllerApi.addOrder(FreshOrder(orderId, joinIfFailedForkWorkflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderFailedInFork](_.key == childOrderId)
    controller.eventWatch.await[OrderFailed](_.key == orderId)
  }

  "Failed, resume failed child order" in {
    val orderId = OrderId("FAIL-THEN-RESUME")
    val childOrderId = orderId / "💥"
    controllerApi.addOrder(FreshOrder(orderId, failInForkWorkflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderFailed](_.key == childOrderId)

    controllerApi
      .executeCommand(ResumeOrder(
        childOrderId,
        position = Some(Position(0) / "fork+💥" % 1)))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderResumed](_.key == childOrderId)
    controller.eventWatch.await[OrderFinished](_.key == orderId)
  }

  "Failed, cancel failed child order" in {
    val orderId = OrderId("FAIL-THEN-CANCEL")
    val childOrderId = orderId / "💥"
    controllerApi.addOrder(FreshOrder(orderId, failInForkWorkflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderFailed](_.key == childOrderId)

    controllerApi.executeCommand(CancelOrders(Seq(childOrderId))).await(99.s).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == childOrderId)
    controller.eventWatch.await[OrderFailed](_.key == orderId)
  }

  "Existing child OrderId" in {
    // Existing child orders are thought only. This should not be possible.
    val order = TestOrder.copy(id = OrderId("DUPLICATE"))
    controller.addOrderBlocking(FreshOrder.unchecked(OrderId("DUPLICATE|🥕"), DuplicateWorkflow.id.path))  // Invalid syntax is allowed for this OrderId, check is suppressed
    eventWatch.await[OrderProcessingStarted](_.key == OrderId("DUPLICATE|🥕"))

    controller.addOrderBlocking(order)
    val expectedFailed = OrderFailed(
      Position(0),
      Some(Outcome.Disrupted(Problem(
      "Forked OrderIds duplicate existing Order(Order:DUPLICATE|🥕,DUPLICATE~INITIAL:0,Processing,Map(),None,None,Vector(),Some(Attached(AGENT-A)),None,None,false,false)"))))
    assert(eventWatch.await[OrderFailed](_.key == order.id).head.value.event == expectedFailed)

    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), CancellationMode.FreshOrStarted())).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(TestWorkflow.id, order.arguments),
      OrderStarted,
      expectedFailed,
      OrderCancelled))

    controllerApi.executeCommand(
      CancelOrders(Seq(order.id / "🥕"), CancellationMode.kill(immediately = true))
    ).await(99.s).orThrow
  }
}

object ForkTest {
  private val DuplicateWorkflow = Workflow(
    WorkflowPath("DUPLICATE") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(AAgentPath, PathExecutable("SLOW.cmd")))))
  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflow.id.path, Map("KEY" -> StringValue("VALUE")))
  private val XOrderId = OrderId(s"🔺|🥕")
  private val YOrderId = OrderId(s"🔺|🍋")

  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestWorkflow.id, Map("KEY" -> StringValue("VALUE"))),

    TestOrder.id <-: OrderStarted,
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
      XOrderId <-: OrderAttachable(AAgentPath),                 YOrderId <-: OrderAttachable(AAgentPath),
      XOrderId <-: OrderAttached(AAgentPath),                   YOrderId <-: OrderAttached(AAgentPath),

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
      XOrderId <-: OrderAttachable(AAgentPath),                 YOrderId <-: OrderAttachable(AAgentPath),
      XOrderId <-: OrderAttached(AAgentPath),                   YOrderId <-: OrderAttached(AAgentPath),

      XOrderId <-: OrderProcessingStarted,                    YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),          YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(Outcome.succeededRC0),      YOrderId <-: OrderProcessed(Outcome.succeededRC0),
      XOrderId <-: OrderMoved(Position(1) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(1) / "fork+🍋" % 1),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(2)),

    TestOrder.id <-: OrderAttachable(BAgentPath),
    TestOrder.id <-: OrderAttached(BAgentPath),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(Outcome.succeededRC0),
    TestOrder.id <-: OrderMoved(Position(3)),

    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderDetached,
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
      XOrderId <-: OrderAttachable(BAgentPath),               YOrderId <-: OrderAttachable(AAgentPath),
      XOrderId <-: OrderAttached(BAgentPath),                 YOrderId <-: OrderAttached(AAgentPath),

      XOrderId <-: OrderProcessingStarted,                    YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),          YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(Outcome.succeededRC0),      YOrderId <-: OrderProcessed(Outcome.succeededRC0),
      XOrderId <-: OrderMoved(Position(3) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(3) / "fork+🍋" % 1),

                                                              YOrderId <-: OrderDetachable,
                                                              YOrderId <-: OrderDetached,
                                                              YOrderId <-: OrderAttachable(BAgentPath),
                                                              YOrderId <-: OrderAttached(BAgentPath),

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
      XOrderId <-: OrderAttachable(AAgentPath),                 YOrderId <-: OrderAttachable(BAgentPath),
      XOrderId <-: OrderAttached(AAgentPath),                   YOrderId <-: OrderAttached(BAgentPath),

      XOrderId <-: OrderProcessingStarted,                    YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),          YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(Outcome.succeededRC0),      YOrderId <-: OrderProcessed(Outcome.succeededRC0),
      XOrderId <-: OrderMoved(Position(4) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(4) / "fork+🍋" % 1),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(5)),
    TestOrder.id <-: OrderFinished,
    TestOrder.id <-: OrderDeleted)

  private val joinIfFailedForkWorkflow = Workflow(
    WorkflowPath("JOIN-IF-FAILED-FORK") ~ "INITIAL",
    Seq(
      Fork(
        Vector(
          "💥" -> Workflow.of(
            Fail(),
            EmptyJob.execute(AAgentPath)),
          "🍋" -> Workflow.empty),
        joinIfFailed = true)))

  private val failInForkWorkflow = Workflow(
    WorkflowPath("FAIL-IN-FORK") ~ "INITIAL",
    Seq(
      Fork(Vector(
        "💥" -> Workflow.of(
          Fail(),
          EmptyJob.execute(AAgentPath)),
        "🍋" -> Workflow.empty))))
}
