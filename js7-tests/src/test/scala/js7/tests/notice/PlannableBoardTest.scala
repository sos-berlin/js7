package js7.tests.notice

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpressionParser.boardPathExpr
import js7.data.board.{BoardPath, BoardPathExpressionParser, PlannableBoard}
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.order.OrderEvent.{OrderAdded, OrderNoticePosted, OrderNoticesConsumed, OrderNoticesExpected, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.plan.{PlanId, PlanItem, PlanItemId}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ConsumeNotices, EmptyInstruction, PostNotices}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.notice.PlannableBoardTest.*
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}

final class PlannableBoardTest
  extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected def agentPaths = Seq(agentPath)

  protected def items = Nil

  "Two Plans of same PlanItem" in:
    withItems(
      (PlanItem.joc(PlanItemId("DailyPlan")), board, postingWorkflow, consumingWorkflow),
    ): (dailyPlan, _, postingWorkflow, consumingWorkflow) =>
      eventWatch.resetLastWatchedEventId()

      val aConsumingOrderId = OrderId("#2024-11-07#CONSUME")
      val aConsumingOrder = FreshOrder(aConsumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
      assert(controllerState.minimumOrderToPlanId(aConsumingOrder) == Right:
        Some(dailyPlan.id / "2024-11-07"))

      controller.addOrderBlocking(aConsumingOrder)
      eventWatch.awaitNext[OrderNoticesExpected](_.key == aConsumingOrderId)

      val bConsumingOrderId = OrderId("#2024-11-08#CONSUME")
      controller.addOrderBlocking:
        FreshOrder(bConsumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNext[OrderNoticesExpected](_.key == bConsumingOrderId)

      val postingOrderId = OrderId("#2024-11-08#POST")
      controller.addOrderBlocking:
        FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
      eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

      eventWatch.awaitNext[OrderNoticesConsumed](_.key == bConsumingOrderId)
      eventWatch.awaitNext[OrderTerminated](_.key == bConsumingOrderId)

      assert:
        controllerState.idToOrder(aConsumingOrderId).state.isInstanceOf[Order.ExpectingNotices]
      execCmd:
        CancelOrders(Seq(aConsumingOrderId))
      eventWatch.awaitNext[OrderTerminated](_.key == aConsumingOrderId)

  "Two PlanItems" in:
    withItems((
      PlanItem(
        PlanItemId("DailyPlan"),
        expr("match($js7OrderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1') ?")),
      PlanItem(
        PlanItemId("WeeklyPlan"),
        expr("match($js7OrderId, '#([0-9]{4}w[0-9]{2})#.*', '$1') ?")),
      board, postingWorkflow, consumingWorkflow),
    ): (_, weeklyPlan, _, postingWorkflow, consumingWorkflow) =>
      eventWatch.resetLastWatchedEventId()

      locally:
        val consumingOrderId = OrderId("#2024w47#CONSUME")
        val consumingOrder = FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
        assert(controllerState.minimumOrderToPlanId(consumingOrder) == Right:
          Some(weeklyPlan.id / "2024w47"))

        controller.addOrderBlocking(consumingOrder)
        eventWatch.awaitNext[OrderAdded](_.key == consumingOrderId)
        assert(controllerState.idToOrder(consumingOrderId).planId == Some(PlanItemId("WeeklyPlan") / "2024w47"))

        eventWatch.awaitNext[OrderNoticesExpected](_.key == consumingOrderId)

        val postingOrderId = OrderId("#2024w47#POST")
        controller.addOrderBlocking:
          FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
        eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

        eventWatch.awaitNext[OrderNoticesConsumed](_.key == consumingOrderId)
        eventWatch.awaitNext[OrderTerminated](_.key == consumingOrderId)

      locally:
        val postingOrderId = OrderId("#2024w47#POST")
        controller.addOrderBlocking:
          FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
        eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

        val consumingOrderId = OrderId("#2024w47#CONSUME")
        controller.addOrderBlocking:
          FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)

        eventWatch.awaitNext[OrderNoticesConsumed](_.key == consumingOrderId)
        eventWatch.awaitNext[OrderTerminated](_.key == consumingOrderId)

  "Two PlanItems with overlapping OrderId patterns" in:
    withItems((
      PlanItem(
        PlanItemId("APlan"),
        expr("match($js7OrderId, '.*A.*-#(.+)#.*', '$1') ?")),
      PlanItem(
        PlanItemId("BPlan"),
        expr("match($js7OrderId, '.*B.*-#(.+)#.*', '$1') ?")),
      postingWorkflow, board),
    ): (aPlan, bPlan, postingWorkflow, _) =>
      eventWatch.resetLastWatchedEventId()

      locally: // No Plan fits --> Global Plan //
        val order = FreshOrder(OrderId("X-#2#"), WorkflowPath("WORKFLOW"))
        assert(controllerState.minimumOrderToPlanId(order) == Right(None))

      locally: // Two Plans fit, Order is rejected //
        val order = FreshOrder(OrderId("AB-#1#"), postingWorkflow.path)
        assert(controllerState.minimumOrderToPlanId(order) == Left(Problem:
          "Order:AB-#1# fits 2 Plans: Plan:APlan/1, Plan:BPlan/1 — An Order must not fit multiple Plans"))

        val checked = controller.api.addOrder(order).await(99.s)
        assert(checked == Left(Problem:
          "Order:AB-#1# fits 2 Plans: Plan:APlan/1, Plan:BPlan/1 — An Order must not fit multiple Plans"))

      locally: // APlan fits //
        val order = FreshOrder(OrderId("A-#1#"), WorkflowPath("WORKFLOW"))
        assert(controllerState.minimumOrderToPlanId(order) == Right(Some(aPlan.id / "1")))

      locally: // BPlan fits //
        val order = FreshOrder(OrderId("B-#2#"), WorkflowPath("WORKFLOW"))
        assert(controllerState.minimumOrderToPlanId(order) == Right(Some(bPlan.id / "2")))


object PlannableBoardTest:

  private val agentPath = AgentPath("AGENT")

  private val board = PlannableBoard(BoardPath("PLANNABLE-BOARD"))

  private val postingWorkflow =
    Workflow(WorkflowPath("POSTING"), Seq(
      PostNotices(Seq(board.path))))

  private val consumingWorkflow =
    Workflow(WorkflowPath("CONSUMING"), Seq(
      ConsumeNotices(boardPathExpr(s"'${board.path.string}'")):
        EmptyInstruction()))
