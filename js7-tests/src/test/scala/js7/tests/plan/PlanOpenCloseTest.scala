package js7.tests.plan

import java.nio.file.Files.delete
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.touchFile
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.SimplePattern
import js7.data.Problems.PlanIsClosedProblem
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, Notice, NoticeKey, NoticePlace, PlannableBoard, PlannedBoard}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, ChangePlan, ChangePlanSchema}
import js7.data.order.OrderEvent.{OrderDeleted, OrderFailed, OrderFinished, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderId, OrderOutcome}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderRejected, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.plan.Plan.Status.{Closed, Deleted, Open}
import js7.data.plan.{Plan, PlanSchema, PlanSchemaId}
import js7.data.value.Value.convenience.given
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{AddOrder, PostNotices, Prompt}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.DeleteFileJob
import js7.tests.plan.PlanOpenCloseTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.language.implicitConversions

final class PlanOpenCloseTest
  extends OurTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "Closed Plan" - {
    "When the last Order of a Plan is deleted, all Notices of the Plan are deleted and the Plan is dead" in:
      val board = PlannableBoard(BoardPath("BOARD"))
      val workflow = Workflow.of(
        Prompt(expr("'PROMPT'")),
        PostNotices(Vector(board.path)))

      withItems(
        (board, workflow, Workflow.empty, dailyPlan)
      ): (board, workflow, emptyWorkflow, dailyPlan) =>
        eventWatch.resetLastWatchedEventId()

        val yesterday = "2025-01-02"
        val yesterdayOrderId = OrderId(s"#$yesterday#")
        controller.runOrder:
          FreshOrder(yesterdayOrderId, emptyWorkflow.path, deleteWhenTerminated = true)

        val today = "2025-01-03"
        val todayPlanId = dailyPlan.id / today
        val aTodayOrderId = OrderId(s"#$today#A")
        val bTodayOrderId = OrderId(s"#$today#B")

        val tomorrow = "2025-01-04"
        val tomorrowPlanId = dailyPlan.id / tomorrow
        val tomorrowOrderId = OrderId(s"#$tomorrow#")

        val dayAfterTomorrow = "2025-01-05"

        assert(controllerState.toPlan.isEmpty)

        // Close yesterday's Plan //
        execCmd:
          ChangePlan(dailyPlan.id / yesterday, Closed)
        assert(controllerState.toPlan.values.toSeq == Seq:
          Plan(dailyPlan.id / yesterday, Deleted))

        // Remove deleted plan
        execCmd:
          ChangePlanSchema(dailyPlan.id, Map("openingDay" -> today))
        assert(controllerState.toPlan.isEmpty)

        // Add Orders //
        controller.addOrderBlocking:
          FreshOrder(aTodayOrderId, workflow.path, planId = todayPlanId, deleteWhenTerminated = true)
        controller.addOrderBlocking:
          FreshOrder(bTodayOrderId, workflow.path, planId = todayPlanId, deleteWhenTerminated = true)
        controller.addOrderBlocking:
          FreshOrder(tomorrowOrderId, workflow.path, planId = tomorrowPlanId, deleteWhenTerminated = true)

        // Now we have Plans for today and tomorrow
        assert(controllerState.toPlan.values.toSeq.sorted == Seq(
          Plan(
            todayPlanId,
            Open,
            orderIds = Set(aTodayOrderId, bTodayOrderId),
            plannedBoards = Seq:
              PlannedBoard(todayPlanId / board.path, Map(
                NoticeKey.empty -> NoticePlace(isAnnounced = true)))),
          Plan(
            tomorrowPlanId,
            Open,
            orderIds = Set(tomorrowOrderId),
            plannedBoards = Seq:
              PlannedBoard(tomorrowPlanId / board.path, Map(
                NoticeKey.empty -> NoticePlace(isAnnounced = true))))))

        // Close today's Plan //
        execCmd:
          ChangePlan(todayPlanId, Closed)

        execCmd:
          AnswerOrderPrompt(aTodayOrderId)
        eventWatch.awaitNextKey[OrderTerminated](aTodayOrderId)

        assert(controllerState.toPlan.values.toVector.sorted == Vector(
          Plan(
            todayPlanId,
            Closed,
            orderIds = Set(bTodayOrderId),
            plannedBoards = Seq:
              PlannedBoard(todayPlanId / board.path, Map(
                NoticeKey.empty -> NoticePlace(Some(Notice(todayPlanId / board.path / NoticeKey.empty)))))),
          Plan(
            tomorrowPlanId,
            Open,
            orderIds = Set(tomorrowOrderId),
            plannedBoards = Seq:
              PlannedBoard(tomorrowPlanId / board.path, Map(
                NoticeKey.empty -> NoticePlace(isAnnounced = true))))))

        // Closed today's Plan will be deleted when the last Order leaves //
        assert(controllerState.toPlan.contains(todayPlanId))
        execCmd:
          AnswerOrderPrompt(bTodayOrderId)
        eventWatch.awaitNextKey[OrderTerminated](bTodayOrderId)
        assert(controllerState.toPlan.values.toSet == Set(
          Plan(todayPlanId, Deleted),
          Plan(tomorrowPlanId, Open,
            Set(tomorrowOrderId),
            Seq(PlannedBoard(tomorrowPlanId / board.path, Map(
              NoticeKey.empty -> NoticePlace(isAnnounced = true)))))))

        // Terminate tomorrow's Orders //
        execCmd:
          AnswerOrderPrompt(tomorrowOrderId)
        eventWatch.awaitNextKey[OrderTerminated](tomorrowOrderId)

        // Close tomorrow's Plan //
        execCmd:
          ChangePlan(tomorrowPlanId, Closed)

        // Tomorrow's Plan is Deleted
        assert(controllerState.toPlan.values.toSeq == Seq(
          Plan(todayPlanId, Deleted),
          Plan(tomorrowPlanId, Deleted)))

        execCmd:
          ChangePlanSchema(dailyPlan.id, Map("openingDay" -> dayAfterTomorrow))

        // Tomorrow's Plan as been removed
        assert(controllerState.toPlan.isEmpty)

    "No order can be added via web service to a closed Plan" in:
      val workflow = Workflow.of(Prompt(expr("'PROMPT'")))
      withItems((workflow, dailyPlan)): (workflow, planSchema) =>
        eventWatch.resetLastWatchedEventId()

        val yesterday = "2024-12-02"
        val yesterdayOrderId = OrderId(s"#$yesterday#")
        val yesterdayPlanId = dailyPlan.id / yesterday
        val today = "2024-12-03"
        val todayPlanId = dailyPlan.id / today
        val todayOrderId = OrderId(s"#$today#")

        execCmd:
          ChangePlanSchema(planSchema.id, Map("openingDay" -> today))

        assert:
          controller.api.addOrder:
            FreshOrder(yesterdayOrderId, workflow.path, planId = yesterdayPlanId, deleteWhenTerminated = true)
          .await(99.s)
            == Left(PlanIsClosedProblem(planSchema.id / yesterday))

        controller.addOrderBlocking:
          FreshOrder(todayOrderId, workflow.path, planId = todayPlanId, deleteWhenTerminated = true)
        assert(controllerState.idToOrder(todayOrderId).isState[Order.Prompting])

        execCmd:
          CancelOrders(todayOrderId :: Nil)

    "No Order can be added via FileWatch to a closed Plan" in:
      eventWatch.resetLastWatchedEventId()
      directoryProvider.withTemporaryDirectory: dir =>
        val workflow = Workflow.of(WorkflowPath("WORKFLOW"),
          DeleteFileJob.execute(agentPath))

        withItems((workflow, dailyPlan)): (workflow, planSchema) =>
          val fileWatch = FileWatch(OrderWatchPath("FILEWATCH"), workflow.path, agentPath,
            directoryExpr = StringConstant(dir.toString),
            pattern = Some(SimplePattern("(.+)")),
            orderIdExpression = Some(expr(""" "#$1#" """)),
            planIdExpr = Some(FileWatch.todayPlanIdExpr))
          withItem(fileWatch, awaitDeletion = true): fileWatch =>
            val yesterday = "2025-01-07"
            val yesterdayExternalName = ExternalOrderName(yesterday)
            val yesterdayOrderId = OrderId(s"#$yesterday#")
            val yesterdayPlanId = dailyPlan.id / yesterday
            val today = "2025-01-08"
            val todayOrderId = OrderId(s"#$today#")

            // Close yesterday's Plan
            execCmd:
              ChangePlanSchema(dailyPlan.id, Map("openingDay" -> today))

            // ExternalOrderRejected //
            touchFile(dir / yesterday)
            val orderRejected = eventWatch.awaitNextKey[ExternalOrderRejected](fileWatch.path)
              .head.value
            assert(orderRejected == fileWatch.path <-: ExternalOrderRejected(
              yesterdayExternalName,
              yesterdayOrderId,
              PlanIsClosedProblem(yesterdayPlanId)))

            assert(controllerState.keyTo(OrderWatchState)(fileWatch.path) == OrderWatchState(
              fileWatch,
              Map(yesterdayExternalName ->
                OrderWatchState.Rejected(yesterdayOrderId, PlanIsClosedProblem(yesterdayPlanId)))))

            delete(dir / yesterday)
            eventWatch.awaitNextKey[ExternalOrderVanished](fileWatch.path,
              _.event.externalOrderName == yesterdayExternalName)

            assert(controllerState.keyTo(OrderWatchState)(fileWatch.path) == OrderWatchState(
              fileWatch))

            touchFile(dir / today)
            eventWatch.awaitNextKey[OrderFinished](todayOrderId)
            eventWatch.awaitNextKey[OrderDeleted](todayOrderId)

    "No other order can be added via AddOrder instruction to a closed Plan" in:
      eventWatch.resetLastWatchedEventId()
      val yesterday = "2025-01-09"
      val yesterdayPlanId = dailyPlan.id / yesterday
      val today = "2025-01-10"
      val todayOrderId = OrderId(s"#$today#")
      val todayPlanId = dailyPlan.id / today
      val workflowPath = WorkflowPath("WORKFLOW")
      val workflow = Workflow.of(workflowPath,
        AddOrder(orderId = expr(s"'#$yesterday#'"), workflowPath,
          planId = Some(yesterdayPlanId.toExpression)))

      withItems((workflow, dailyPlan)): (workflow, planSchema) =>
        // Close yesterday's Plan
        execCmd:
          ChangePlanSchema(dailyPlan.id, Map("openingDay" -> today))

        controller.addOrderBlocking:
          FreshOrder(todayOrderId, workflow.path, planId = todayPlanId, deleteWhenTerminated = true)
        eventWatch.awaitNextKey[OrderFailed](todayOrderId)

        assert(controllerState.idToOrder(todayOrderId).lastOutcome ==
          OrderOutcome.Failed(Some(PlanIsClosedProblem(yesterdayPlanId).toString)))
        execCmd:
          CancelOrders(todayOrderId :: Nil)
  }


object PlanOpenCloseTest:
  private val agentPath = AgentPath("AGENT")

  private val dailyPlan = PlanSchema.joc(PlanSchemaId("DailyPlan"))
