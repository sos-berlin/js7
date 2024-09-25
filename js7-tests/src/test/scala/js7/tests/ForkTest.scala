package js7.tests

import js7.base.configutils.Configs.*
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteOrdersWhenTerminated, ResumeOrder}
import js7.data.order.OrderEvent.*
import js7.data.order.{FreshOrder, HistoricOutcome, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.instructions.{Fail, Fork}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.ForkTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import cats.effect.IO
import cats.effect.unsafe.IORuntime

final class ForkTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.TEST-ONLY.suppress-order-id-check-for = "DUPLICATE|🥕"
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = aAgentPath :: bAgentPath :: Nil
  protected val items = Seq(workflow, failingResultWorkflow, duplicateWorkflow,
    joinIfFailedForkWorkflow, failInForkWorkflow)

  "Events" in:
    controller.addOrderBlocking(TestOrder)
    eventWatch.await[OrderFinished](_.key == TestOrder.id)
    val keyedEvents = eventWatch.allKeyedEvents[OrderEvent]
    for orderId <- Array(TestOrder.id, XOrderId, YOrderId) do  // But ordering if each order is determined
      assert(keyedEvents.filter(_.key == orderId) == ExpectedEvents.filter(_.key == orderId))
    assert(keyedEvents.toSet == ExpectedEvents.toSet)  // XOrderId and YOrderId run in parallel and ordering is not determined

    assert(controllerState.idToOrder(TestOrder.id).historicOutcomes == Seq(
      HistoricOutcome(Position(0), OrderOutcome.succeeded),
      HistoricOutcome(Position(1), OrderOutcome.succeeded),
      HistoricOutcome(Position(2), OrderOutcome.succeeded),
      HistoricOutcome(Position(3), OrderOutcome.Succeeded(Map(
        "CARROT" -> NumberValue(11),
        "CITRON" -> NumberValue(21)))),
      HistoricOutcome(Position(4), OrderOutcome.succeeded)))

    controller.api.executeCommand(DeleteOrdersWhenTerminated(Seq(TestOrder.id)))
      .await(99.s).orThrow

  "failingResultWorkflow" in:
    val events = controller.runOrder(FreshOrder(OrderId("💥"), failingResultWorkflow.id.path))
      .map(_.value)
    assert(events == Seq(
      OrderAdded(failingResultWorkflow.id),
      OrderStarted,
      OrderForked(Vector("💥" -> OrderId("💥|💥"))),
      OrderJoined(OrderOutcome.Failed(Some("No such named value: UNKNOWN"))),
      OrderFailed(Position(0))))

  "joinIfFailed" in:
    val orderId = OrderId("JOIN-IF-FAILED")
    val childOrderId = orderId / "💥"
    controller.api.addOrder(FreshOrder(orderId, joinIfFailedForkWorkflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderFailedInFork](_.key == childOrderId)
    controller.eventWatch.await[OrderFailed](_.key == orderId)

  "Failed, resume failed child order" in:
    val orderId = OrderId("FAIL-THEN-RESUME")
    val childOrderId = orderId / "💥"
    controller.api.addOrder(FreshOrder(orderId, failInForkWorkflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderFailed](_.key == childOrderId)

    controller.api
      .executeCommand(ResumeOrder(
        childOrderId,
        position = Some(Position(0) / "fork+💥" % 1),
        asSucceeded = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderResumed](_.key == childOrderId)
    val terminated = controller.eventWatch.await[OrderTerminated](_.key == orderId).head.value
    assert(terminated == orderId <-: OrderFinished())

  "Failed, cancel failed child order" in:
    val orderId = OrderId("FAIL-THEN-CANCEL")
    val childOrderId = orderId / "💥"
    controller.api.addOrder(FreshOrder(orderId, failInForkWorkflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderFailed](_.key == childOrderId)

    controller.api.executeCommand(CancelOrders(Seq(childOrderId))).await(99.s).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == childOrderId)
    controller.eventWatch.await[OrderFailed](_.key == orderId)

  "Existing child OrderId" in:
    // Existing child orders are thought only. It is expected to be impossible in production.
    val order = TestOrder.copy(id = OrderId("DUPLICATE"))
    controller.addOrderBlocking(FreshOrder.unchecked(OrderId("DUPLICATE|🥕"), duplicateWorkflow.id.path))  // Invalid syntax is allowed for this OrderId, check is suppressed
    eventWatch.await[OrderProcessingStarted](_.key == OrderId("DUPLICATE|🥕"))

    controller.addOrderBlocking(order)

    val expectedOutcomeAdded = OrderOutcomeAdded(OrderOutcome.Disrupted(Problem(
      "Forked OrderIds duplicate existing Order(Order:DUPLICATE|🥕,DUPLICATE~INITIAL:0," +
        "Processing(Subagent:AGENT-A-0),Map(),None,None,Vector()," +
        "Some(Attached to Agent:AGENT-A),None,None,false,false,false,false,List(),List(),Set())")))
    assert(eventWatch.await[OrderOutcomeAdded](_.key == order.id).head.value.event == expectedOutcomeAdded)

    val expectedFailed = OrderFailed(Position(0))
    assert(eventWatch.await[OrderFailed](_.key == order.id).head.value.event == expectedFailed)

    controller.api.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOrStarted())).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(eventWatch.eventsByKey[OrderEvent](order.id) == Vector(
      OrderAdded(workflow.id, order.arguments),
      expectedOutcomeAdded,
      expectedFailed,
      OrderCancelled))

    controller.api.executeCommand(
      CancelOrders(Seq(OrderId("DUPLICATE|🥕")), CancellationMode.kill(immediately = true))
    ).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == OrderId("DUPLICATE|🥕"))


object ForkTest:
  private val aAgentPath = AgentPath("AGENT-A")
  private val bAgentPath = AgentPath("AGENT-B")
  private val aSubagentId = toLocalSubagentId(aAgentPath)
  private val bSubagentId = toLocalSubagentId(bAgentPath)


  private class SlowJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess.cancelable:
        IO.sleep(60.s).as(OrderOutcome.succeeded)

  private object SlowJob extends InternalJob.Companion[SlowJob]


  final class TestJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess.checkedOutcome:
        for arg <- step.arguments("ARG").asNumber yield
          OrderOutcome.Succeeded(Map(
            "jobResult" -> NumberValue(arg + 1)))

  object TestJob extends InternalJob.Companion[TestJob]


  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL" ,
    Vector(
      /*0*/ Fork.of(
        "🥕" -> Workflow.of(EmptyJob.execute(aAgentPath)),
        "🍋" -> Workflow.of(EmptyJob.execute(aAgentPath))),
      /*1*/ Fork.of(
        "🥕" -> Workflow.of(EmptyJob.execute(aAgentPath)),
        "🍋" -> Workflow.of(EmptyJob.execute(aAgentPath))),
      /*2*/ EmptyJob.execute(bAgentPath),
      /*3*/ Fork.checked(Seq(
        Fork.Branch(
          "🥕",
          Workflow.anonymous(
            Seq(
              TestJob.execute(bAgentPath, Map(
                "ARG" -> expr("10")))),
            result = Some(Map("CARROT" -> expr("$jobResult"))))),
        Fork.Branch(
          "🍋",
          Workflow.anonymous(
            Seq(
              TestJob.execute(aAgentPath, Map(
                "ARG" -> expr("20"))),
              EmptyJob.execute(bAgentPath)),
            result = Some(Map("CITRON" -> expr("$jobResult"))))))
      ).orThrow,
      /*4*/ Fork.of(
        "🥕" -> Workflow.of(EmptyJob.execute(aAgentPath)),
        "🍋" -> Workflow.of(EmptyJob.execute(bAgentPath)))))

  private val duplicateWorkflow = Workflow(
    WorkflowPath("DUPLICATE") ~ "INITIAL",
    Vector(
      SlowJob.execute(aAgentPath)))

  private val joinIfFailedForkWorkflow = Workflow(
    WorkflowPath("JOIN-IF-FAILED-FORK") ~ "INITIAL",
    Seq(
      Fork(
        Vector(
          "💥" -> Workflow.of(
            Fail(),
            EmptyJob.execute(aAgentPath)),
          "🍋" -> Workflow.empty),
        joinIfFailed = true)))

  private val failInForkWorkflow = Workflow(
    WorkflowPath("FAIL-IN-FORK") ~ "INITIAL",
    Seq(
      Fork(Vector(
        "💥" -> Workflow.of(
          Fail(),
          EmptyJob.execute(aAgentPath)),
        "🍋" -> Workflow.empty))))

  private val failingResultWorkflow = Workflow(
    WorkflowPath("FAILING-RESULT-FORK") ~ "INITIAL",
    Seq(
      Fork(Vector(
        Fork.Branch("💥",
          Workflow.anonymous(Nil, result = Some(Map(
            "DUPLICATE" -> expr("$UNKNOWN")))))))))

  private val TestOrder = FreshOrder(OrderId("🔺"), workflow.id.path, Map("KEY" -> StringValue("VALUE")))
  private val XOrderId = OrderId("🔺|🥕")
  private val YOrderId = OrderId("🔺|🍋")

  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(workflow.id, Map("KEY" -> StringValue("VALUE"))),

    TestOrder.id <-: OrderStarted,
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
      XOrderId <-: OrderAttachable(aAgentPath),               YOrderId <-: OrderAttachable(aAgentPath),
      XOrderId <-: OrderAttached(aAgentPath),                 YOrderId <-: OrderAttached(aAgentPath),

      XOrderId <-: OrderProcessingStarted(aSubagentId),        YOrderId <-: OrderProcessingStarted(aSubagentId),
      XOrderId <-: OrderProcessed(OrderOutcome.succeeded),         YOrderId <-: OrderProcessed(OrderOutcome.succeeded),
      XOrderId <-: OrderMoved(Position(0) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(0) / "fork+🍋" % 1),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(OrderOutcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(1)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
      XOrderId <-: OrderAttachable(aAgentPath),               YOrderId <-: OrderAttachable(aAgentPath),
      XOrderId <-: OrderAttached(aAgentPath),                 YOrderId <-: OrderAttached(aAgentPath),

      XOrderId <-: OrderProcessingStarted(aSubagentId),        YOrderId <-: OrderProcessingStarted(aSubagentId),
      XOrderId <-: OrderProcessed(OrderOutcome.succeeded),         YOrderId <-: OrderProcessed(OrderOutcome.succeeded),
      XOrderId <-: OrderMoved(Position(1) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(1) / "fork+🍋" % 1),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(OrderOutcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(2)),

    TestOrder.id <-: OrderAttachable(bAgentPath),
    TestOrder.id <-: OrderAttached(bAgentPath),
    TestOrder.id <-: OrderProcessingStarted(bSubagentId),
    TestOrder.id <-: OrderProcessed(OrderOutcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(3)),

    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderDetached,
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
      XOrderId <-: OrderAttachable(bAgentPath),               YOrderId <-: OrderAttachable(aAgentPath),
      XOrderId <-: OrderAttached(bAgentPath),                 YOrderId <-: OrderAttached(aAgentPath),

      XOrderId <-: OrderProcessingStarted(bSubagentId),       YOrderId <-: OrderProcessingStarted(aSubagentId),
      XOrderId <-: OrderProcessed(OrderOutcome.Succeeded(Map("jobResult" -> NumberValue(11)))),
                                                              YOrderId <-: OrderProcessed(OrderOutcome.Succeeded(Map("jobResult" -> NumberValue(21)))),
      XOrderId <-: OrderMoved(Position(3) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(3) / "fork+🍋" % 1),

                                                              YOrderId <-: OrderDetachable,
                                                              YOrderId <-: OrderDetached,
                                                              YOrderId <-: OrderAttachable(bAgentPath),
                                                              YOrderId <-: OrderAttached(bAgentPath),

                                                              YOrderId <-: OrderProcessingStarted(bSubagentId),
                                                              YOrderId <-: OrderProcessed(OrderOutcome.succeeded),
                                                              YOrderId <-: OrderMoved(Position(3) / "fork+🍋" % 2),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(OrderOutcome.Succeeded(Map(
      "CARROT" -> NumberValue(11),
      "CITRON" -> NumberValue(21)))),
    TestOrder.id <-: OrderMoved(Position(4)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId),                      OrderForked.Child("🍋", YOrderId))),
      XOrderId <-: OrderAttachable(aAgentPath),               YOrderId <-: OrderAttachable(bAgentPath),
      XOrderId <-: OrderAttached(aAgentPath),                 YOrderId <-: OrderAttached(bAgentPath),

      XOrderId <-: OrderProcessingStarted(aSubagentId),       YOrderId <-: OrderProcessingStarted(bSubagentId),
      XOrderId <-: OrderProcessed(OrderOutcome.succeeded),         YOrderId <-: OrderProcessed(OrderOutcome.succeeded),
      XOrderId <-: OrderMoved(Position(4) / "fork+🥕" % 1),   YOrderId <-: OrderMoved(Position(4) / "fork+🍋" % 1),

      XOrderId <-: OrderDetachable,                           YOrderId <-: OrderDetachable,
      XOrderId <-: OrderDetached,                             YOrderId <-: OrderDetached,
    TestOrder.id <-: OrderJoined(OrderOutcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(5)),
    TestOrder.id <-: OrderFinished())
