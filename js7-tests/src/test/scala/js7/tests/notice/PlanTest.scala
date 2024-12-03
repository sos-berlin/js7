package js7.tests.notice

import fs2.Stream
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpression.syntax.boardPathToExpr
import js7.data.board.{BoardPath, BoardPathExpression, BoardState, Notice, NoticeKey, NoticePlace, PlannableBoard}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders}
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation
import js7.data.order.OrderEvent.{OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderPrompted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.plan.{Plan, PlanKey, PlanTemplate, PlanTemplateId}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ConsumeNotices, PostNotices, Prompt}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.notice.PlanTest.*
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import scala.language.implicitConversions

final class PlanTest
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

  "Deletion of a PlanTemplate" - {
    def tryDeletePlan(planTemplateId: PlanTemplateId): Checked[Unit] =
      controller.api.updateItems(Stream:
        ItemOperation.Remove(planTemplateId))
      .rightAs(()).await(99.s)

    "PlanTemplate is only deletable if no Order is associated" in:
      val day = "2024-11-08"
      withItem(Workflow.of(Prompt(expr("'PROMPT'")))): workflow =>
        eventWatch.resetLastWatchedEventId()

        val planTemplate = updateItem(PlanTemplate.joc(PlanTemplateId("DailyPlan")))
        val postingOrderId = OrderId(s"#$day#POST")
        controller.addOrderBlocking:
          FreshOrder(postingOrderId, workflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNext[OrderPrompted](_.key == postingOrderId)

        assert(tryDeletePlan(planTemplate.path) == Left(Problem:
          s"PlanTemplate:DailyPlan is in use by Plan:$day with Order:#$day#POST"))

        execCmd:
          CancelOrders(Seq(postingOrderId))
        eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

        deleteItems(planTemplate.path)
        eventWatch.awaitNext[ItemDeleted](_.event.key == planTemplate.path)

    "When a PlanTemplate is being deleted, all its Boards are deleted (emptied)" in:
      val day = "2024-11-27"
      val aBoard = PlannableBoard(BoardPath("A-BOARD"))
      val bBoard = PlannableBoard(BoardPath("B-BOARD"))
      val cBoard = PlannableBoard(BoardPath("C-BOARD"))
      withItems((
        aBoard, bBoard,
        Workflow.of(
          Prompt(expr("'PROMPT'")),
          PostNotices(aBoard.path :: Nil),
          Prompt(expr("'PROMPT'")),
          PostNotices(bBoard.path :: Nil)),
        Workflow.of(
          ConsumeNotices(aBoard.path):
            Prompt(expr("'PROMPT'"))))
      ): (aBoard, bBoard, postingWorkflow, consumingWorkflow) =>
        eventWatch.resetLastWatchedEventId()

        val planTemplate = updateItem(PlanTemplate.joc(PlanTemplateId("DailyPlan-2")))
        val planKey = PlanKey(day)
        val planId = planTemplate.id / planKey

        val postingOrderId = OrderId(s"#$day#POST")
        controller.addOrderBlocking:
          FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNext[OrderPrompted](_.key == postingOrderId)

        assert(controllerState.slowPlanTemplateToPlan(planTemplate.id)(planKey) ==
          Plan(
            planId,
            Set(postingOrderId),
            Map(
              aBoard.path -> Set(NoticeKey.empty),
              bBoard.path -> Set(NoticeKey.empty))))

        val consumingOrderId = OrderId(s"#$day#CONSUME")
        controller.addOrderBlocking:
          FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNext[OrderNoticesExpected](_.key == consumingOrderId)

        assert(controllerState.slowPlanTemplateToPlan(planTemplate.id)(planKey) ==
          Plan(
            planId,
            Set(postingOrderId, consumingOrderId),
            Map(
              aBoard.path -> Set(NoticeKey.empty),
              bBoard.path -> Set(NoticeKey.empty))))

        assert(tryDeletePlan(planTemplate.path) == Left(Problem:
          s"PlanTemplate:DailyPlan-2 is in use by Plan:$day with 2 orders"))

        execCmd:
          AnswerOrderPrompt(postingOrderId)
        eventWatch.awaitNext[OrderNoticesConsumptionStarted](_.key == consumingOrderId)
        eventWatch.awaitNext[OrderPrompted](_.key == postingOrderId)
        eventWatch.awaitNext[OrderPrompted](_.key == consumingOrderId)

        assert(tryDeletePlan(planTemplate.path) == Left(Problem:
          s"PlanTemplate:DailyPlan-2 is in use by Plan:$day with 2 orders"))

        for orderId <- Seq(postingOrderId, consumingOrderId) do
          execCmd(CancelOrders(Seq(orderId)))
          eventWatch.awaitNext[OrderTerminated](_.key == orderId)

        assert(controllerState.slowPlanTemplateToPlan(planTemplate.id)(planKey) ==
          Plan(
            planId,
            orderIds = Set.empty,
            Map(
              aBoard.path -> Set(NoticeKey.empty),
              bBoard.path -> Set(NoticeKey.empty))))

        val noticeId = planId.noticeId
        assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice == Map(
          noticeId -> NoticePlace(noticeId, Some(Notice(noticeId, aBoard.path, endOfLife = None)))))
        assert(controllerState.keyTo(BoardState)(aBoard.path).orderToConsumptionStack == Map.empty)

        // When PlanItem has been deleted, its NoticePlaces are deleted, too //

        assert(controllerState.slowPlanTemplateToPlanToBoardToNoticeKey == Map(
          PlanTemplateId.Global -> Map.empty,
          planTemplate.id -> Map(
            planKey -> Map(
              aBoard.path -> Set(NoticeKey.empty),
              bBoard.path -> Set(NoticeKey.empty)))))

        deleteItems(planTemplate.path)
        eventWatch.awaitNext[ItemDeleted](_.event.key == planTemplate.path)

        assert(controllerState.slowPlanTemplateToPlanToBoardToNoticeKey == Map(
          PlanTemplateId.Global -> Map.empty))
  }


object PlanTest:

  private val agentPath = AgentPath("AGENT")
