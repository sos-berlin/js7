package js7.tests.plan

import java.nio.file.Files.delete
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.touchFile
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.utils.SimplePattern
import js7.data.Problems.{PlanIsClosedProblem, PlanIsDeletedProblem}
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, Notice, NoticeKey, NoticePlace, PlannableBoard, PlannedBoard}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, ChangePlan, ChangePlanSchema}
import js7.data.order.OrderEvent.{OrderDeleted, OrderFailed, OrderFinished, OrderForked, OrderOrderAdded, OrderPrompted, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderId, OrderOutcome}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderRejected, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.plan.PlanStatus.{Closed, Deleted, Open}
import js7.data.plan.{Plan, PlanSchema, PlanSchemaId}
import js7.data.value.Value.convenience.given
import js7.data.value.expression.Expression.{StringConstant, expr}
import js7.data.workflow.instructions.{AddOrder, Fork, PostNotices, Prompt}
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
        Prompt(expr"'PROMPT'"),
        PostNotices(Vector(board.path)))

      withItems(
        (board, workflow, Workflow.empty, dailyPlan)
      ): (board, workflow, emptyWorkflow, dailyPlan) =>
        eventWatch.resetLastWatchedEventId()

        val yesterday = "2025-01-02"
        val yesterdayPlanId = dailyPlan.id / yesterday
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
          ChangePlan(yesterdayPlanId, Closed)
        assert(controllerState.toPlan.toMap == Map:
          yesterdayPlanId -> Plan(yesterdayPlanId, Deleted))

        // Remove deleted plan
        execCmd:
          ChangePlanSchema(dailyPlan.id, Some(Map("unknownPlansAreOpenFrom" -> today)))
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
        assert(controllerState.toPlan.toMap == Map(
          todayPlanId -> Plan(todayPlanId, Deleted),
          tomorrowPlanId -> Plan(tomorrowPlanId, Deleted)))

        execCmd:
          ChangePlanSchema(dailyPlan.id, Some(Map("unknownPlansAreOpenFrom" -> dayAfterTomorrow)))

        // Tomorrow's Plan has been removed
        assert(controllerState.toPlan.isEmpty)

    "AddOrder instruction may add an Order to the own closed Plan" in:
      eventWatch.resetLastWatchedEventId()
      val otherWorkflow = Workflow.of(WorkflowPath("OTHER"))
      val workflow = Workflow.of(
        Prompt(expr"'PROMPT'"),
        AddOrder(orderId = expr"'ADDED'", otherWorkflow.path, deleteWhenTerminated = true))

      withItems((otherWorkflow, workflow, dailyPlan)): (_, workflow, dailyPlan) =>
        val planId = dailyPlan.id / "2025-03-26"
        val orderId = OrderId("ORDER")
        controller.addOrderBlocking:
          FreshOrder(orderId, workflow.path, planId = planId, deleteWhenTerminated = true)
        eventWatch.awaitNextKey[OrderPrompted](orderId)

        execCmd:
          ChangePlan(planId, status = Closed)
        execCmd:
          AnswerOrderPrompt(orderId)
        val orderAdded = eventWatch.awaitNextKey[OrderOrderAdded](orderId)
          .head.value
        assert(orderAdded.planId == planId)

        eventWatch.awaitNextKey[OrderFinished](orderId)
        eventWatch.awaitNextKey[OrderFinished](OrderId("ADDED"))

    "AddOrder instruction may NOT add an Order to a different closed Plan" in:
      eventWatch.resetLastWatchedEventId()
      val planId = dailyPlan.id / "2025-03-26"
      val closedPlanSchema = PlanSchema.joc(PlanSchemaId("CLOSED"))
      val closedPlanId = closedPlanSchema.id / "2025-03-26"
      val otherWorkflow = Workflow.of(WorkflowPath("EMPTY"))
      val workflow = Workflow.of(
        AddOrder(orderId = expr"'ADDED'", otherWorkflow.path,
          planId = Some(closedPlanId.toExpression),
          deleteWhenTerminated = true))

      withItems((otherWorkflow, workflow, dailyPlan, closedPlanSchema)): (_, workflow, _, _) =>
        // Add an keepingOrderId to closedPlanId to prevent a PlanStatus change to Finished and Deleted
        val keepingOrderId = OrderId("KEEPING")
        controller.addOrderBlocking:
          FreshOrder(keepingOrderId, otherWorkflow.path, planId = closedPlanId, deleteWhenTerminated = true,
            scheduledFor = Some(ts"2100-01-01T12:00:00Z"))
        execCmd:
          ChangePlan(closedPlanId, status = Closed)

        val orderId = OrderId("ORDER")
        controller.addOrderBlocking:
          FreshOrder(orderId, workflow.path, planId = planId, deleteWhenTerminated = true)

        eventWatch.awaitNextKey[OrderFailed](orderId).head.value
        assert(controllerState.idToOrder(orderId).lastOutcome ==
          OrderOutcome.Failed(Some(PlanIsClosedProblem(closedPlanId).toString)))
        execCmd:
          CancelOrders(Seq(orderId, keepingOrderId))

    "A Fork instruction may add Orders to the own closed Plan" in:
      eventWatch.resetLastWatchedEventId()
      val workflow = Workflow.of(
        Prompt(expr"'PROMPT'"),
        Fork(Vector("BRANCH" -> Workflow.of:
          Prompt(expr"'PROMPT'"))))

      withItems((workflow, dailyPlan)): (workflow, dailyPlan) =>
        val planId = dailyPlan.id / "2025-03-26"
        val orderId = OrderId("ORDER")
        controller.addOrderBlocking:
          FreshOrder(orderId, workflow.path, planId = planId, deleteWhenTerminated = true)
        eventWatch.awaitNextKey[OrderPrompted](orderId)

        execCmd:
          ChangePlan(planId, status = Closed)
        execCmd:
          AnswerOrderPrompt(orderId)
        eventWatch.awaitNextKey[OrderForked](orderId)

        val childOrderId = orderId / "BRANCH"
        eventWatch.awaitNextKey[OrderPrompted](childOrderId)
        assert(controllerState.idToOrder(childOrderId).planId == planId)
        execCmd:
          AnswerOrderPrompt(childOrderId)
        eventWatch.awaitNextKey[OrderFinished](orderId)

    "No order can be added via web service to a closed Plan" in:
      val workflow = Workflow.of(Prompt(expr"'PROMPT'"))
      withItems((workflow, dailyPlan)): (workflow, dailyPlan) =>
        eventWatch.resetLastWatchedEventId()

        val yesterday = "2024-12-02"
        val yesterdayOrderId = OrderId("YESTERDAY")
        val yesterdayPlanId = dailyPlan.id / yesterday
        val today = "2024-12-03"
        val todayPlanId = dailyPlan.id / today
        val todayOrderId = OrderId("TODAY")

        execCmd:
          ChangePlanSchema(dailyPlan.id, Some(Map("unknownPlansAreOpenFrom" -> today)))

        assert:
          controller.api.addOrder:
            FreshOrder(yesterdayOrderId, workflow.path, planId = yesterdayPlanId,
              deleteWhenTerminated = true)
          .await(99.s)
            == Left(PlanIsDeletedProblem(dailyPlan.id / yesterday))

        controller.addOrderBlocking:
          FreshOrder(todayOrderId, workflow.path, planId = todayPlanId, deleteWhenTerminated = true)
        assert(controllerState.idToOrder(todayOrderId).isState[Order.Prompting])

        execCmd:
          ChangePlan(todayPlanId, Closed)

        assert:
          controller.api.addOrder:
            FreshOrder(OrderId("OTHER"), workflow.path, planId = todayPlanId,
              deleteWhenTerminated = true)
          .await(99.s) == Left(PlanIsClosedProblem(todayPlanId))

        execCmd:
          CancelOrders(todayOrderId :: Nil)

    "No Order can be added via FileWatch to a closed Plan" in:
      eventWatch.resetLastWatchedEventId()
      directoryProvider.withTemporaryDirectory: dir =>
        val workflow = Workflow.of(WorkflowPath("WORKFLOW"),
          DeleteFileJob.execute(agentPath))

        withItems((workflow, dailyPlan)): (workflow, dailyPlan) =>
          val fileWatch = FileWatch(OrderWatchPath("FILEWATCH"), workflow.path, agentPath,
            directoryExpr = StringConstant(dir.toString),
            pattern = Some(SimplePattern("(.+)")),
            orderExpr = Some(expr"""{
              orderId: "#$$1#",
              planId: [ 'DailyPlan', "$$1" ]
            }"""))
          withItem(fileWatch, awaitDeletion = true): fileWatch =>
            val yesterday = "2025-01-07"
            val yesterdayExternalName = ExternalOrderName(yesterday)
            val yesterdayPlanId = dailyPlan.id / yesterday
            val today = "2025-01-08"
            val todayOrderId = OrderId(s"#$today#")

            // Close yesterday's Plan
            execCmd:
              ChangePlan(yesterdayPlanId, Closed)

            // ExternalOrderRejected //
            touchFile(dir / yesterday)
            val orderRejected = eventWatch.awaitNextKey[ExternalOrderRejected](fileWatch.path)
              .head.value
            assert(orderRejected == ExternalOrderRejected(
              yesterdayExternalName,
              PlanIsDeletedProblem(yesterdayPlanId)))

            assert(controllerState.keyTo(OrderWatchState)(fileWatch.path) == OrderWatchState(
              fileWatch,
              Map(yesterdayExternalName ->
                OrderWatchState.Rejected(PlanIsDeletedProblem(yesterdayPlanId)))))

            delete(dir / yesterday)
            eventWatch.awaitNextKey[ExternalOrderVanished](fileWatch.path,
              _.event.externalOrderName == yesterdayExternalName)

            assert(controllerState.keyTo(OrderWatchState)(fileWatch.path) == OrderWatchState(
              fileWatch))

            touchFile(dir / today)
            eventWatch.awaitNextKey[OrderFinished](todayOrderId)
            eventWatch.awaitNextKey[OrderDeleted](todayOrderId)
  }


object PlanOpenCloseTest:
  private val agentPath = AgentPath("AGENT")
  private val dailyPlan = PlanSchema.joc(PlanSchemaId("DailyPlan"))
