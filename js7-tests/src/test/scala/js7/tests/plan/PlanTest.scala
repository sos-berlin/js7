package js7.tests.plan

import fs2.Stream
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpression.syntax.boardPathToExpr
import js7.data.board.{BoardPath, BoardPathExpression, Notice, NoticeKey, NoticePlace, PlannableBoard, PlannedBoard}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, ChangePlan}
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation
import js7.data.order.OrderEvent.{OrderDeleted, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderPrompted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.plan.{Plan, PlanKey, PlanSchema, PlanSchemaId, PlanSchemaState}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ConsumeNotices, PostNotices, Prompt}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.SemaphoreJob
import js7.tests.plan.PlanTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.language.implicitConversions

final class PlanTest
  extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected def agentPaths = Seq(agentPath)

  protected def items = Nil

  "Delete a PlanSchema" - {
    def tryDeletePlan(planSchemaId: PlanSchemaId): Checked[Unit] =
      controller.api.updateItems(Stream:
        ItemOperation.Remove(planSchemaId))
      .rightAs(()).await(99.s)

    "PlanSchema is only deletable if no Order is associated" in:
      val planSchemaId = PlanSchemaId("DailyPlan")
      val day = "2024-11-08"
      val planId = planSchemaId / day
      withItem(Workflow.of(Prompt(expr("'PROMPT'")))): workflow =>
        eventWatch.resetLastWatchedEventId()

        val planSchema = updateItem(PlanSchema.joc(PlanSchemaId("DailyPlan")))
        val postingOrderId = OrderId(s"#$day#POST")
        controller.addOrderBlocking:
          FreshOrder(postingOrderId, workflow.path, planId = planId, deleteWhenTerminated = true)
        eventWatch.awaitNextKey[OrderPrompted](postingOrderId)

        assert(tryDeletePlan(planSchema.path) == Left(Problem:
          s"PlanSchema:DailyPlan cannot be deleted because it is in use by Plan:$day with Order:#$day#POST"))

        execCmd:
          CancelOrders(Seq(postingOrderId))
        eventWatch.awaitNextKey[OrderTerminated](postingOrderId)

        execCmd: // Close the plan, so it can be deleted
          ChangePlan(planSchema.id / day, Plan.Status.Closed)

        deleteItems(planSchema.path)
        eventWatch.awaitNext[ItemDeleted](_.event.key == planSchema.path)

    "When a PlanSchema is being deleted, all its Plans are deleted" in:
      val day = "2024-11-27"
      val aBoard = PlannableBoard(BoardPath("A-BOARD"))
      val bBoard = PlannableBoard(BoardPath("B-BOARD"),
        postOrderToNoticeKey = expr("'🔸'"),
        expectOrderToNoticeKey = expr("'🔸'"))
      val postingWorkflow = Workflow.of(
        Prompt(expr("'PROMPT'")),
        PostNotices(aBoard.path :: Nil),
        Prompt(expr("'PROMPT'")),
        PostNotices(bBoard.path :: Nil))
      val consumingWorkflow = Workflow.of(
        ConsumeNotices(aBoard.path):
          Prompt(expr("'PROMPT'")))
      withItems(
        (aBoard, bBoard, postingWorkflow, consumingWorkflow)
      ): (aBoard, bBoard, postingWorkflow, consumingWorkflow) =>
        eventWatch.resetLastWatchedEventId()

        val planSchema = updateItem(PlanSchema.joc(PlanSchemaId("DailyPlan-2")))
        val planKey = PlanKey(day)
        val planId = planSchema.id / planKey

        val postingOrderId = OrderId(s"#$day#POST")
        controller.addOrderBlocking:
          FreshOrder(postingOrderId, postingWorkflow.path, planId = planId,
            deleteWhenTerminated = true)
        eventWatch.awaitNextKey[OrderPrompted](postingOrderId)

        val aNoticeKey = NoticeKey.empty
        val bNoticeKey = NoticeKey("🔸")

        assert(controllerState.toPlan(planSchema.id / planKey) ==
          Plan(
            planId,
            Plan.Status.Open,
            Set(postingOrderId),
            Seq(
              PlannedBoard(planId / aBoard.path, Map(
                aNoticeKey -> NoticePlace(isAnnounced = true))),
              PlannedBoard(planId / bBoard.path, Map(
                bNoticeKey -> NoticePlace(isAnnounced = true))))))

        val consumingOrderId = OrderId(s"#$day#CONSUME")
        controller.addOrderBlocking:
          FreshOrder(consumingOrderId, consumingWorkflow.path, planId = planId,
            deleteWhenTerminated = true)
        eventWatch.awaitNextKey[OrderNoticesExpected](consumingOrderId)

        assert(controllerState.toPlan(planSchema.id / planKey) ==
          Plan(
            planId,
            Plan.Status.Open,
            Set(postingOrderId, consumingOrderId),
            Seq(
              PlannedBoard(planId / aBoard.path, Map(
                aNoticeKey -> NoticePlace(isAnnounced = true, expectingOrderIds = Set(consumingOrderId)))),
              PlannedBoard(planId / bBoard.path, Map(
                bNoticeKey -> NoticePlace(isAnnounced = true))))))

        assert(tryDeletePlan(planSchema.path) == Left(Problem:
          s"PlanSchema:DailyPlan-2 cannot be deleted because it is in use by Plan:$day with Order:#2024-11-27#POST, Order:#2024-11-27#CONSUME"))

        execCmd:
          AnswerOrderPrompt(postingOrderId)
        eventWatch.awaitNextKey[OrderNoticesConsumptionStarted](consumingOrderId)
        eventWatch.awaitNextKey[OrderPrompted](postingOrderId)
        eventWatch.awaitNextKey[OrderPrompted](consumingOrderId)

        assert(tryDeletePlan(planSchema.path) == Left(Problem:
          s"PlanSchema:DailyPlan-2 cannot be deleted because it is in use by Plan:$day with Order:#2024-11-27#POST, Order:#2024-11-27#CONSUME"))

        for orderId <- Seq(postingOrderId, consumingOrderId) do
          execCmd(CancelOrders(Seq(orderId)))
          eventWatch.awaitNextKey[OrderDeleted](orderId)

        assert(controllerState.keyTo(PlanSchemaState).values.flatMap(_.plans).toSet ==
          Set(
            Plan(planId,
              Plan.Status.Open,
              plannedBoards = Seq(
                PlannedBoard(planId / aBoard.path, Map(
                  aNoticeKey -> NoticePlace(Some(Notice(planId / aBoard.path / aNoticeKey))))),
                PlannedBoard(planId / bBoard.path, Map(
                  bNoticeKey -> NoticePlace(isAnnounced = true)))))))

        // When PlanSchema is being deleted, its NoticePlaces are deleted, too //

        execCmd: // Close the plan, so it can be deleted
          ChangePlan(planId, Plan.Status.Closed)

        deleteItems(planSchema.path)
        eventWatch.awaitNext[ItemDeleted](_.event.key == planSchema.path)

        assert(controllerState.keyTo(PlanSchemaState).values.flatMap(_.plans).isEmpty)
  }


object PlanTest:

  private val agentPath = AgentPath("AGENT")

  private final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  private object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
