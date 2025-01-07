package js7.tests.plan

import fs2.Stream
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.Problems.{OrderCannotAttachedToPlanProblem, OrderWouldNotMatchChangedPlanTemplateProblem}
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpression.syntax.boardPathToExpr
import js7.data.board.{BoardPath, BoardPathExpression, BoardState, GlobalBoard, Notice, NoticeId, NoticeKey, NoticePlace, PlannableBoard, PlannedBoard}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders}
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation
import js7.data.order.OrderEvent.{OrderAdded, OrderAttached, OrderCancelled, OrderDeleted, OrderFinished, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderPlanAttached, OrderPrompted, OrderStarted, OrderStateReset, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.plan.{Plan, PlanId, PlanKey, PlanTemplate, PlanTemplateId, PlanTemplateState}
import js7.data.value.Value.convenience.*
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

  "When a PlanTemplates is added, matching planless Orders are attached to the new Plans" in :
    eventWatch.resetLastWatchedEventId()
    val day = "2024-12-03"
    val orderId = OrderId(s"#$day#")

    withItem(Workflow.of(Prompt(expr("'PROMPT'")))): workflow =>
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.awaitNext[OrderPrompted](_.key == orderId)
      assert(controllerState.idToOrder(orderId).planId == PlanId.Global)

      withItem(PlanTemplate.joc(PlanTemplateId("DailyPlan"))): dailyPlan =>
        val planId = dailyPlan.id / day

        // Order is attached to our DailyPlan //
        assert(controllerState.idToOrder(orderId).planId == planId)

        execCmd:
          CancelOrders(Seq(orderId))
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,
          OrderPrompted("PROMPT"),
          OrderPlanAttached(planId),
          OrderStateReset,
          OrderCancelled,
          OrderDeleted))

  "Adding a PlanTemplates is rejected when a planless Order is attached to an Agent" in:
    // It's recommended to SuspendOrders(resetState) all Orders before
    // adding or changing a PlanTemplate.

    eventWatch.resetLastWatchedEventId()
    val orderId = OrderId(s"#2024-12-04#")
    val workflow = Workflow.of(ASemaphoreJob.execute(agentPath))

    withItem(workflow): workflow =>
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.awaitNextKey[OrderAttached](orderId)

      val dailyPlan = PlanTemplate.joc(PlanTemplateId("DailyPlan"))
      val checked = controller.api.updateItems(fs2.Stream:
          ItemOperation.AddOrChangeSimple(dailyPlan))
        .await(99.s)
      assert(checked == Left(OrderCannotAttachedToPlanProblem(orderId)))
      ASemaphoreJob.continue()
      eventWatch.awaitNextKey[OrderFinished](orderId)

  "Adding a PlanTemplates is rejected when a planless Orders is expecting a global Notice" in :
    // It's recommended to SuspendOrders(resetState) all Orders before
    // adding or changing a PlanTemplate.

    eventWatch.resetLastWatchedEventId()
    val orderId = OrderId(s"#2024-12-05#")
    val board = GlobalBoard.joc(BoardPath("BOARD"))
    val workflow = Workflow.of:
      ConsumeNotices(board.path):
        Prompt(expr("'PROMPT'"))

    withItems((workflow, board)): (workflow, board) =>
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.awaitNextKey[OrderNoticesExpected](orderId)

      val dailyPlan = PlanTemplate.joc(PlanTemplateId("DailyPlan"))
      val checked = controller.api.updateItems(fs2.Stream:
          ItemOperation.AddOrChangeSimple(dailyPlan))
        .await(99.s)
      assert(checked == Left(OrderCannotAttachedToPlanProblem(orderId)))
      execCmd(CancelOrders(Seq(orderId)))
      eventWatch.awaitNextKey[OrderTerminated](orderId)

  "Update a PlanTemplate and check existing Orders" in:
    eventWatch.resetLastWatchedEventId()
    val templateId = PlanTemplateId("DailyPlan")

    val aPlanTemplate = PlanTemplate.joc(templateId)
    val aKey = "2024-12-06"
    val aOrderId = OrderId(s"#$aKey#")
    val aPlanId = templateId / aKey

    val bKey = "2024w49"
    val bOrderId = OrderId(s"#$bKey#")
    val bPlanId = templateId / bKey
    val workflow = Workflow.of:
      Prompt(expr("'PROMPT'"))

    withItems((aPlanTemplate, workflow)): (aPlanTemplate, workflow) =>
      // aOrderId is in the plan
      controller.addOrderBlocking(FreshOrder(aOrderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.awaitNextKey[OrderPrompted](aOrderId)

      // aOrderId is not in a plan (that means, in the global plan)
      controller.addOrderBlocking(FreshOrder(bOrderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.awaitNextKey[OrderPrompted](bOrderId)

      // Change planTemplate such that aOrderId no longer match, but bOrderId match
      val bPlanTemplate = PlanTemplate.weekly(aPlanTemplate.id)
      val checked = controller.api.updateItems(fs2.Stream:
          ItemOperation.AddOrChangeSimple(bPlanTemplate))
        .await(99.s)
      assert(checked == Left(OrderWouldNotMatchChangedPlanTemplateProblem(aOrderId, aPlanId)))

      // Me must delete aOrderId to change the PlanTemplate
      execCmd(CancelOrders(Seq(aOrderId)))
      eventWatch.awaitNextKey[OrderTerminated](aOrderId)

      updateItem(bPlanTemplate)

      // Now, bOrderId is attached to the updated PlanTemplatae
      assert(controllerState.idToOrder(bOrderId).planId == bPlanId)
      assert(eventWatch.eventsByKey[OrderEvent](bOrderId) == Seq(
        OrderAdded(workflow.id, planId = None, deleteWhenTerminated = true),
        OrderStarted,
        OrderPrompted("PROMPT"),
        OrderPlanAttached(bPlanId)))

      execCmd(CancelOrders(Seq(bOrderId)))

  "Delete a PlanTemplate" - {
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
        eventWatch.awaitNextKey[OrderPrompted](postingOrderId)

        assert(tryDeletePlan(planTemplate.path) == Left(Problem:
          s"PlanTemplate:DailyPlan cannot be deleted because it is in use by Plan:$day with Order:#$day#POST"))

        execCmd:
          CancelOrders(Seq(postingOrderId))
        eventWatch.awaitNextKey[OrderTerminated](postingOrderId)

        deleteItems(planTemplate.path)
        eventWatch.awaitNext[ItemDeleted](_.event.key == planTemplate.path)

    "When a PlanTemplate is being deleted, all its PlannedBoards and NoticeIds are deleted" in:
      val day = "2024-11-27"
      val aBoard = PlannableBoard(BoardPath("A-BOARD"))
      val bBoard = PlannableBoard(BoardPath("B-BOARD"))
      val cBoard = PlannableBoard(BoardPath("C-BOARD"))
      val postingWorkflow = Workflow.of(
        Prompt(expr("'PROMPT'")),
        PostNotices(aBoard.path :: Nil),
        Prompt(expr("'PROMPT'")),
        PostNotices(bBoard.path :: Nil))
      val consumingWorkflow = Workflow.of(
        ConsumeNotices(aBoard.path):
          Prompt(expr("'PROMPT'")))
      withItems((aBoard, bBoard, postingWorkflow, consumingWorkflow)
      ): (aBoard, bBoard, postingWorkflow, consumingWorkflow) =>
        eventWatch.resetLastWatchedEventId()

        val planTemplate = updateItem(PlanTemplate.joc(PlanTemplateId("DailyPlan-2")))
        val planKey = PlanKey(day)
        val planId = planTemplate.id / planKey

        val postingOrderId = OrderId(s"#$day#POST")
        controller.addOrderBlocking:
          FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNextKey[OrderPrompted](postingOrderId)

        val noticeId = planId.noticeId

        assert(controllerState.toPlan(planTemplate.id / planKey) ==
          Plan(
            planId,
            Set(postingOrderId),
            Seq(
              PlannedBoard(planId / aBoard.path, Map(
                NoticeKey.empty -> NoticePlace(noticeId, isAnnounced = true))),
              PlannedBoard(planId / bBoard.path, Map(
                NoticeKey.empty -> NoticePlace(noticeId, isAnnounced = true)))),
            isClosed = false))

        val consumingOrderId = OrderId(s"#$day#CONSUME")
        controller.addOrderBlocking:
          FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNextKey[OrderNoticesExpected](consumingOrderId)

        assert(controllerState.toPlan(planTemplate.id / planKey) ==
          Plan(
            planId,
            Set(postingOrderId, consumingOrderId),
            Seq(
              PlannedBoard(planId / aBoard.path, Seq(
                NoticePlace(noticeId, expectingOrderIds = Set(consumingOrderId), isAnnounced = true))),
              PlannedBoard(planId / bBoard.path, Seq(
                NoticePlace(noticeId,  isAnnounced = true)))),
            isClosed = false))

        assert(tryDeletePlan(planTemplate.path) == Left(Problem:
          s"PlanTemplate:DailyPlan-2 cannot be deleted because it is in use by Plan:$day with 2 orders"))

        execCmd:
          AnswerOrderPrompt(postingOrderId)
        eventWatch.awaitNextKey[OrderNoticesConsumptionStarted](consumingOrderId)
        eventWatch.awaitNextKey[OrderPrompted](postingOrderId)
        eventWatch.awaitNextKey[OrderPrompted](consumingOrderId)

        assert(tryDeletePlan(planTemplate.path) == Left(Problem:
          s"PlanTemplate:DailyPlan-2 cannot be deleted because it is in use by Plan:$day with 2 orders"))

        for orderId <- Seq(postingOrderId, consumingOrderId) do
          execCmd(CancelOrders(Seq(orderId)))
          eventWatch.awaitNextKey[OrderDeleted](orderId)

        assert(controllerState.toPlan(planTemplate.id / planKey) ==
          Plan(
            planId,
            orderIds = Set.empty,
            Seq(
              PlannedBoard(planId / aBoard.path, Seq(
                NoticePlace(noticeId, Some(Notice(noticeId, aBoard.path, endOfLife = None))))),
              PlannedBoard(planId / bBoard.path, Seq(
                NoticePlace(noticeId, isAnnounced = true)))),
            isClosed = false))

        assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice == Map(
          noticeId -> NoticePlace(noticeId, Some(Notice(noticeId, aBoard.path, endOfLife = None)))))
        assert(controllerState.keyTo(BoardState)(aBoard.path).orderToConsumptionStack == Map.empty)

        // When PlanTemplate has been deleted, its NoticePlaces are deleted, too //

        assert(controllerState.keyTo(PlanTemplateState).values.flatMap(_.plans).toSet ==
          Set(
            Plan(planId, plannedBoards = Seq(
              PlannedBoard(planId / aBoard.path, Seq(
                NoticePlace(noticeId, Some(Notice(noticeId, aBoard.path, endOfLife = None))))),
              PlannedBoard(planId / bBoard.path, Seq(
                NoticePlace(noticeId, isAnnounced = true)))),
              isClosed = false)))

        deleteItems(planTemplate.path)
        eventWatch.awaitNext[ItemDeleted](_.event.key == planTemplate.path)

        assert(controllerState.keyTo(PlanTemplateState).values.flatMap(_.plans).isEmpty)
  }


object PlanTest:

  private val agentPath = AgentPath("AGENT")

  private final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  private object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
