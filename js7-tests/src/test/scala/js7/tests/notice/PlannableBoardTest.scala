package js7.tests.notice

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpressionParser.boardPathExpr
import js7.data.board.{BoardPath, BoardPathExpressionParser, PlannableBoard}
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.order.OrderEvent.{OrderNoticePosted, OrderNoticesConsumed, OrderNoticesExpected, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.plan.{PlanItem, PlanItemId}
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

  "test" in:
    val board = PlannableBoard(BoardPath("PLANNABLE-BOARD"))
    withItems((
      PlanItem.joc(PlanItemId("DailyPlan")),
      board,
      Workflow(WorkflowPath("CONSUMING"), Seq(
        ConsumeNotices(boardPathExpr(s"'${board.path.string}'")):
          EmptyInstruction())),
      Workflow(WorkflowPath("POSTING"), Seq(
        PostNotices(Seq(board.path)))),
    )): (planItem, board, consumingWorkflow, postingWorkflow) =>
      eventWatch.resetLastWatchedEventId()
      val aConsumingOrderId = OrderId("#2024-11-07#CONSUME")
      controller.addOrderBlocking:
        FreshOrder(aConsumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
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


object PlannableBoardTest:

  private val agentPath = AgentPath("AGENT")
