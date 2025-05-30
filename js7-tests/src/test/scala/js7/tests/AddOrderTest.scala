package js7.tests

import cats.effect.unsafe.IORuntime
import java.time.ZoneId
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problems.DuplicateKey
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{TestAlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.GoOrder
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderGoMarked, OrderGoes, OrderMoved, OrderOrderAdded, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderPrompted, OrderStarted}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{AddOrder, Fail, Prompt}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.AddOrderTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class AddOrderTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  private lazy val alarmClock = TestAlarmClock(ts"2099-01-01T00:00:00Z")

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(alarmClock))

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(aWorkflow, bWorkflow, c1Workflow, c2Workflow)

  "AddOrder" in:
    val orderId = OrderId("🔷")
    val events = controller.runOrder(FreshOrder(orderId, aWorkflow.path))
    assert(events.map(_.value) == Seq(
      OrderAdded(aWorkflow.id),
      OrderStarted,
      OrderOrderAdded(
        OrderId("🟦"),
        bWorkflow.id,
        Map(
          "year" -> StringValue("2099")),
        deleteWhenTerminated = true),
      OrderMoved(Position(1)),
      OrderFinished()))
    eventWatch.await[OrderPrompted](_.key == OrderId("🟦"))

  "AddOrder with duplicate OrderId" in:
    val orderId = OrderId("🟧")
    val events = controller.runOrder(FreshOrder(orderId, aWorkflow.path))
    assert(events.map(_.value) == Seq(
      OrderAdded(aWorkflow.id),
      OrderStarted,
      OrderOutcomeAdded(OrderOutcome.Failed.fromProblem(DuplicateKey("OrderId", OrderId("🟦")))),
      OrderFailed(Position(0))))

  "AddOrder with access to clock" in:
    val orderId = OrderId("CLOCK")
    val events = controller.runOrder(FreshOrder(orderId, aWorkflow.path))
    assert(events.map(_.value) == Seq(
      OrderAdded(aWorkflow.id),
      OrderStarted,
      OrderOutcomeAdded(OrderOutcome.Failed.fromProblem(DuplicateKey("OrderId", OrderId("🟦")))),
      OrderFailed(Position(0))))

  "AddOrder with startPosition and stopPositions (JS-2029)" in:
    locally:
      val orderId = OrderId("START-AND-STOP")
      val events = controller.runOrder(FreshOrder(orderId, c1Workflow.path)).map(_.value)
      assert(events == Seq(
        OrderAdded(c1Workflow.id),
        OrderStarted,
        OrderOrderAdded(
          OrderId("ADDED"),
          c2Workflow.id,
          startPosition = Some(Position(1)),
          stopPositions = Set(Position(2)),
          deleteWhenTerminated = true),
        OrderMoved(Position(1)),
        OrderFinished()))

    locally:
      val orderId = OrderId("ADDED")
      eventWatch.await[OrderDeleted](_.key == orderId)
      val events = eventWatch.eventsByKey[OrderEvent](orderId)
      assert(events == Seq(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(2)),
        OrderDetachable,
        OrderDetached,
        OrderFinished(None),
        OrderDeleted))

  "GoOrder a waiting Fresh Order" in:
    val workflow = Workflow(
      WorkflowPath("KICK-FRESH"),
      instructions = Seq(EmptyJob.execute(agentPath)))

    withItem(workflow): workflow =>
      val orderId = OrderId("#2024-04-02#KICK-FRESH")
      given ZoneId = ZoneId.of("UTC")
      controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true,
          scheduledFor = Some(local("2100-01-01T00:00"))))
        .await(99.s).orThrow
      var eventId = eventWatch.await[OrderAdded](_.key == orderId).head.eventId
      awaitAndAssert:
        val order = controllerState.idToOrder(orderId)
        order.isState[Order.Fresh] && order.isAttached
      execCmd(GoOrder(orderId, position = Position(0)))
      eventId = eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)
        .head.eventId

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true,
          scheduledFor = Some(local("2100-01-01T00:00"))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderGoMarked(Position(0)),
        OrderGoes,
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFinished(),
        OrderDeleted))

  "uniqueOrderId" in:
    val eventId = eventWatch.resetLastWatchedEventId()
    withItem(Workflow.empty): emptyWorkflow =>
      val workflow = Workflow.of:
        AddOrder(
          orderId = expr(s""" uniqueOrderId("ADDED-%03d") """),
          emptyWorkflow.path,
          deleteWhenTerminated = true)

      withItem(workflow): workflow =>
        val orderIds = (1 to 3).map(i => OrderId(s"ORDER-$i"))
        val addedOrderIds = Seq(OrderId("ADDED-001"), OrderId("ADDED-002"), OrderId("ADDED-002"))

        controller.api.addOrders:
          fs2.Stream.iterable(orderIds).map: orderId =>
            FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
        .await(99.s).orThrow

        for orderId <- orderIds ++ addedOrderIds do
          eventWatch.awaitKey[OrderFinished](orderId)


object AddOrderTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ "INITIAL",
    Seq(
      AddOrder(
        orderId = expr("'🟦'"),
        bWorkflow.path,
        arguments = Map(
          "year" -> expr("now('yyyy')")),
        deleteWhenTerminated = true)))

  private lazy val bWorkflow = Workflow(WorkflowPath("B-WORKFLOW") ~ "INITIAL",
    Seq(
      Prompt(expr("'?'"))))

  private val c1Workflow = Workflow(WorkflowPath("C1-WORKFLOW") ~ "INITIAL",
    Seq(
      AddOrder(
        orderId = expr("'ADDED'"),
        c2Workflow.path,
        startPosition = Some(Position(1)),
        stopPositions = Set(Position(2)),
        deleteWhenTerminated = true)))

  private lazy val c2Workflow = Workflow(WorkflowPath("C2-WORKFLOW") ~ "INITIAL",
    Seq(
      Prompt(expr("'?'")),
      EmptyJob.execute(agentPath),
      Fail(None)))
