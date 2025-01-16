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
import js7.data.board.{BoardPath, Notice, NoticeId, NoticePlace, PlannableBoard, PlannedBoard}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, ChangePlanSchema}
import js7.data.order.OrderEvent.{OrderDeleted, OrderFinished, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderRejected, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.plan.{Plan, PlanSchema, PlanSchemaId}
import js7.data.value.Value.convenience.given
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.workflow.instructions.{PostNotices, Prompt}
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

      withItems((board, workflow, dailyPlan)): (board, workflow, dailyPlan) =>
        eventWatch.resetLastWatchedEventId()

        val today = "2024-01-03"
        val todaysPlanId = dailyPlan.id / today
        val aTodaysOrderId = OrderId(s"#$today#A")
        val bTodaysOrderId = OrderId(s"#$today#B")

        val tomorrow = "2024-01-04"
        val tomorrowsPlanId = dailyPlan.id / tomorrow
        val tomorrowsOrderId = OrderId(s"#$tomorrow#")

        val dayAfterTomorrow = "2024-01-05"

        // Close yesterday's Plan //
        execCmd:
          ChangePlanSchema(dailyPlan.id, Map("openingDay" -> today))

        // No Plan
        assert(controllerState.toPlan.isEmpty)

        // Add Orders //
        for orderId <- Seq(aTodaysOrderId, bTodaysOrderId, tomorrowsOrderId) do
          controller.addOrderBlocking:
            FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)

        // Now we have Plans for today and tomorrow
        assert(controllerState.toPlan.values.toVector.sorted == Vector(
          Plan(
            todaysPlanId,
            orderIds = Set(aTodaysOrderId, bTodaysOrderId),
            plannedBoards = Seq:
              PlannedBoard(
                todaysPlanId / board.path,
                Seq:
                  NoticePlace(todaysPlanId.noticeId, isAnnounced = true)),
            isClosed = false),
          Plan(
            tomorrowsPlanId,
            orderIds = Set(tomorrowsOrderId),
            plannedBoards = Seq:
              PlannedBoard(
                tomorrowsPlanId / board.path,
                Seq:
                  NoticePlace(tomorrowsPlanId.noticeId, isAnnounced = true)),
            isClosed = false)))

        // Close today's Plan //
        execCmd:
          ChangePlanSchema(dailyPlan.id, Map("openingDay" -> tomorrow))

        execCmd:
          AnswerOrderPrompt(aTodaysOrderId)
        eventWatch.awaitNextKey[OrderTerminated](aTodaysOrderId)

        assert(controllerState.toPlan.values.toVector.sorted == Vector(
          Plan(
            todaysPlanId,
            orderIds = Set(bTodaysOrderId),
            plannedBoards = Seq:
              PlannedBoard(
                todaysPlanId / board.path,
                Seq:
                  NoticePlace(
                    todaysPlanId.noticeId,
                    Some(Notice.forPlannedBoard(todaysPlanId / board.path)))),
            isClosed = true),
          Plan(
            tomorrowsPlanId,
            orderIds = Set(tomorrowsOrderId),
            plannedBoards = Seq:
              PlannedBoard(
                tomorrowsPlanId / board.path,
                Seq:
                  NoticePlace(
                    tomorrowsPlanId.noticeId,
                    isAnnounced = true)),
            isClosed = false)))

        // Closed today's Plan will be deleted when the last Order leaves //
        assert(controllerState.toPlan.contains(todaysPlanId))
        execCmd:
          AnswerOrderPrompt(bTodaysOrderId)
        eventWatch.awaitNextKey[OrderTerminated](aTodaysOrderId)
        assert(!controllerState.toPlan.contains(todaysPlanId))

        // Terminate tomorrow's Orders //
        execCmd:
          AnswerOrderPrompt(tomorrowsOrderId)
        eventWatch.awaitNextKey[OrderTerminated](aTodaysOrderId)

        // Close tomorrow's Plan //
        execCmd:
          ChangePlanSchema(dailyPlan.id, Map("openingDay" -> dayAfterTomorrow))

        // Tomorrow's Plan has been deleted
        assert(controllerState.toPlan.isEmpty)

    "No order can be added via web service to a closed Plan" in:
      val workflow = Workflow.of(Prompt(expr("'PROMPT'")))
      withItems((workflow, dailyPlan)): (workflow, planSchema) =>
        eventWatch.resetLastWatchedEventId()

        val yesterday = "2024-12-02"
        val today = "2024-12-03"
        execCmd:
          ChangePlanSchema(planSchema.id, Map("openingDay" -> today))
        val yesterdayOrderId = OrderId(s"#$yesterday#")
        val todayOrderId = OrderId(s"#$today#")

        assert:
          controller.api.addOrder:
            FreshOrder(yesterdayOrderId, workflow.path, deleteWhenTerminated = true)
          .await(99.s)
            == Left(PlanIsClosedProblem(planSchema.id / yesterday))

        controller.addOrderBlocking:
          FreshOrder(todayOrderId, workflow.path, deleteWhenTerminated = true)
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
            orderIdExpression = Some(expr(""" "#$1#" """)))
          withItem(fileWatch, awaitDeletion = true): fileWatch =>
            val yesterday = "2025-12-07"
            val yesterdayExternalName = ExternalOrderName(yesterday)
            val yesterdayOrderId = OrderId(s"#$yesterday#")
            val yesterdayPlanId = dailyPlan.id / yesterday
            val today = "2025-12-08"

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
            eventWatch.awaitNextKey[OrderFinished](OrderId(yesterday))
            eventWatch.awaitNextKey[OrderDeleted](OrderId(yesterday))

    "No other order can be added via AddOrder instruction to a closed dead Plan" in:
      // AddOrder instruction
      // Fork: cannot be dead because the forking Order is in the Plan
      // FileWatch:
      missingTest
  }

object PlanOpenCloseTest:
  private val agentPath = AgentPath("AGENT")

  private val dailyPlan = PlanSchema.joc(
    PlanSchemaId("DailyPlan"),
    planIsClosedFunction = Some(exprFunction("(day) => $day < $openingDay")))
