package js7.tests.notice

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpression.ExpectNotice
import js7.data.board.BoardPathExpression.syntax.*
import js7.data.board.BoardPathExpression.syntax.boardPathToExpr
import js7.data.board.{BoardPath, BoardPathExpression, NoticeKey, PlannableBoard}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, DeleteOrdersWhenTerminated}
import js7.data.order.OrderEvent.{OrderAdded, OrderDeleted, OrderFinished, OrderMoved, OrderNoticeAnnounced, OrderNoticePosted, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanSchema, PlanSchemaId}
import js7.data.value.StringValue
import js7.data.value.Value.convenience.given
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced.{DontWait, SkipWhenNoNotice, Wait}
import js7.data.workflow.instructions.{ConsumeNotices, EmptyInstruction, ExpectNotices, PostNotices, Prompt}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.notice.PlannableBoardTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.language.implicitConversions
import scala.util.control.NonFatal

final class PlannableBoardTest
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

  "Two Plans of same PlanSchema" in:
    withItems((
      PlanSchema.joc(PlanSchemaId("DailyPlan")),
      board,
      Workflow(WorkflowPath("POSTING"), Seq(
        PostNotices(Seq(board.path)))),
      consumingWorkflow
    )): (dailyPlan, _, postingWorkflow, consumingWorkflow) =>
      eventWatch.resetLastWatchedEventId()
      val day = "2024-11-08"
      val planId = dailyPlan.id / day

      // otherConsumingOrderId is in some other Plan //
      val day0 = "2024-11-07"
      val otherConsumingOrderId = OrderId(s"#$day0#CONSUME")

      locally:
        val otherConsumingOrder = FreshOrder(otherConsumingOrderId, consumingWorkflow.path,
          deleteWhenTerminated = true, arguments = Map("ARG" -> "🔸"))
        controller.addOrderBlocking(otherConsumingOrder)
        execCmd(DeleteOrdersWhenTerminated(Set(otherConsumingOrderId)))
        eventWatch.awaitNextKey[OrderNoticesExpected](otherConsumingOrderId)


      // Wait for notice, post notice, consume notice //
      val consumingOrderId = OrderId(s"#$day#CONSUME")
      controller.addOrderBlocking:
        FreshOrder(consumingOrderId, consumingWorkflow.path, planId = planId,
          deleteWhenTerminated = true,
          arguments = Map("ARG" -> "🔸"))
      eventWatch.awaitNextKey[OrderNoticesExpected](consumingOrderId)

      // Post a Notice //
      val postingOrderId = OrderId(s"#$day#POST")
      controller.addOrderBlocking:
        FreshOrder(postingOrderId, postingWorkflow.path, planId = planId,
          deleteWhenTerminated = true,
          arguments = Map("ARG" -> "🔸"))
      eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
      eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

      eventWatch.awaitNextKey[OrderNoticesConsumed](consumingOrderId)
      eventWatch.awaitNextKey[OrderTerminated](consumingOrderId)

      val noticeId = planId / board.path / NoticeKey("🔸")

      assert(eventWatch.eventsByKey[OrderEvent](postingOrderId) == Seq(
        OrderAdded(postingWorkflow.id, planId = planId, deleteWhenTerminated = true,
          arguments = Map("ARG" -> "🔸")),
        OrderNoticeAnnounced(noticeId),
        OrderStarted,
        OrderNoticePosted(noticeId, endOfLife = None),
        OrderMoved(Position(1), None),
        OrderFinished(),
        OrderDeleted))

      assert(eventWatch.eventsByKey[OrderEvent](consumingOrderId) == Seq(
        OrderAdded(consumingWorkflow.id, planId = planId, deleteWhenTerminated = true,
          arguments = Map("ARG" -> "🔸")),
        OrderStarted,
        OrderNoticesExpected(Vector(
          planId / board.path / NoticeKey("🔸"))),
        OrderNoticesConsumptionStarted(Vector(
          planId / board.path / NoticeKey("🔸"))),
        OrderMoved(Position(0) / "consumeNotices" % 1),
        OrderNoticesConsumed(),
        OrderFinished(),
        OrderDeleted))

      assert:
        controllerState.idToOrder(otherConsumingOrderId).state.isInstanceOf[Order.ExpectingNotices]
      execCmd:
        CancelOrders(Seq(otherConsumingOrderId))
      eventWatch.awaitNext[OrderTerminated](_.key == otherConsumingOrderId)

  "Two PlanSchemas" in:
    withItems((
      PlanSchema(PlanSchemaId("DailyPlan"), PlanSchema.EachUnknownPlanIsOpen),
      PlanSchema(PlanSchemaId("WeeklyPlan"), PlanSchema.EachUnknownPlanIsOpen),
      board,
      Workflow(WorkflowPath("POSTING"), Seq(
        PostNotices(Seq(board.path)))),
      consumingWorkflow),
    ): (_, weeklyPlan, _, postingWorkflow, consumingWorkflow) =>
      eventWatch.resetLastWatchedEventId()
      val weeklyPlanId = weeklyPlan.id / "2024w47"
      locally:
        val postingOrderId = OrderId("POST")
        controller.addOrderBlocking:
          FreshOrder(postingOrderId, postingWorkflow.path, planId = weeklyPlanId,
            deleteWhenTerminated = true,
            arguments = Map("ARG" -> "🔸"))
        eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
        eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

      locally:
        val consumingOrderId = OrderId("CONSUME")
        val consumingOrder = FreshOrder(consumingOrderId, consumingWorkflow.path,
          planId = weeklyPlanId,
          arguments = Map("ARG" -> "🔸"))

        controller.addOrderBlocking(consumingOrder)
        eventWatch.awaitNextKey[OrderAdded](consumingOrderId)
        eventWatch.awaitNextKey[OrderNoticesConsumed](consumingOrderId)
        eventWatch.awaitNextKey[OrderTerminated](consumingOrderId)

        assert(controllerState.idToOrder(consumingOrderId).planId == weeklyPlanId)
        execCmd(DeleteOrdersWhenTerminated(consumingOrderId :: Nil))

      //locally:
      //  val postingOrderId = OrderId("POST")
      //  controller.addOrderBlocking:
      //    FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
      //  eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
      //  eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)
      //
      //  val consumingOrderId = OrderId("CONSUME")
      //  controller.addOrderBlocking:
      //    FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
      //
      //  eventWatch.awaitNextKey[OrderNoticesConsumed](consumingOrderId)
      //  eventWatch.awaitNextKey[OrderTerminated](consumingOrderId)

  "Announced Notices" - {
    // Each test will announce a Notice at bBoard

    val boardPathExpr = board.path & bBoard.path & cBoard.path

    def announcingTest[A](boardPath: BoardPath, planId: PlanId)(body: OrderId => A): A =
      val postingWorkflow = Workflow(WorkflowPath("POSTING"), Seq(
        Prompt(expr("'PROMPT'")),
        PostNotices(Seq(boardPath))))
      withItem(postingWorkflow): postingWorkflow =>
        val announcingOrderId = OrderId("POST")
        controller.addOrderBlocking:
          FreshOrder(announcingOrderId, postingWorkflow.path, planId = planId,
            deleteWhenTerminated = true,
            arguments = Map("ARG" -> "🔸"))
        try
          body(announcingOrderId)
        finally
          if controllerState.idToOrder.contains(announcingOrderId) then
            try execCmd(CancelOrders(Seq(announcingOrderId)))
            catch case NonFatal(t) =>
              Logger.error(s"Failed to cancel order $announcingOrderId", t)

    "WhenNotAnnounced.Wait" - {
      "ConsumeNotices" in:
        eventWatch.resetLastWatchedEventId()
        val dailyPlan = PlanSchema.joc(PlanSchemaId("DailyPlan"))
        val day = "2024-11-22"
        val planId = dailyPlan.id / day
        val workflow = Workflow(WorkflowPath("EXPECTING"), Seq(
          ConsumeNotices(boardPathExpr, whenNotAnnounced = Wait):
            EmptyInstruction()))
        withItems(
          (dailyPlan, board, bBoard, cBoard, workflow)
        ): (dailyPlan, _, _, _, workflow) =>
          announcingTest(bBoard.path, planId): announcingOrderId =>
            val plannedNoticeKey = planId / NoticeKey("🔸")
            val orderId = OrderId(s"#$day#CONSUME")
            controller.addOrderBlocking:
              FreshOrder(orderId, workflow.path, planId = planId, deleteWhenTerminated = true,
                arguments = Map("ARG" -> "🔸"))
            eventWatch.awaitNextKey[OrderNoticesExpected](orderId)

            // Post board and cBoard
            execCmd:
              ControllerCommand.PostNotice:
                planId / board.path / NoticeKey("🔸")
            execCmd:
              ControllerCommand.PostNotice:
                planId / cBoard.path / NoticeKey("🔸")

            // Post bBoard
            execCmd(AnswerOrderPrompt(announcingOrderId))
            eventWatch.awaitNextKey[OrderNoticePosted](announcingOrderId)

            eventWatch.awaitNextKey[OrderTerminated](orderId)
            assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
              OrderAdded(workflow.id, planId = planId,
                deleteWhenTerminated = true, arguments = Map("ARG" -> "🔸")),
              OrderStarted,
              OrderNoticesExpected(Vector(
                planId / board.path / NoticeKey("🔸"),
                planId / bBoard.path / NoticeKey("🔸"),
                planId / cBoard.path / NoticeKey("🔸"))),
              OrderNoticesConsumptionStarted(Vector(
                planId / board.path / NoticeKey("🔸"),
                planId / bBoard.path / NoticeKey("🔸"),
                planId / cBoard.path / NoticeKey("🔸"))),
              OrderMoved(Position(0) / "consumeNotices" % 1),
              OrderNoticesConsumed(),
              OrderFinished(),
              OrderDeleted))

      "ExpectNotices" in:
        eventWatch.resetLastWatchedEventId()
        val dailyPlan = PlanSchema.joc(PlanSchemaId("DailyPlan"))
        val day = "2024-11-23"
        val planId = dailyPlan.id / day
        val workflow =
          Workflow(WorkflowPath("CONSUMING"), Seq(
            ExpectNotices(boardPathExpr, whenNotAnnounced = Wait)))
        withItems(
          (dailyPlan, board, bBoard, cBoard, workflow)
        ): (dailyPlan, _, _, _, workflow) =>
          announcingTest(bBoard.path, planId): announcingOrderId =>
            val orderId = OrderId(s"#$day#EXPECT")
            controller.addOrderBlocking:
              FreshOrder(orderId, workflow.path, planId = planId, deleteWhenTerminated = true,
                arguments = Map("ARG" -> "🔸"))
            eventWatch.awaitNext[OrderNoticesExpected](_.key == orderId)

            val plannedNoticeKey = planId / NoticeKey("🔸")

            // Post board and cBoard
            execCmd(ControllerCommand.PostNotice(board.path / plannedNoticeKey))
            execCmd(ControllerCommand.PostNotice(cBoard.path / plannedNoticeKey))

            // Post cBoard
            execCmd(AnswerOrderPrompt(announcingOrderId))
            eventWatch.awaitNext[OrderNoticePosted](_.key == announcingOrderId)

            eventWatch.awaitNext[OrderTerminated](_.key == orderId)
            assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
              OrderAdded(workflow.id, planId = planId,
                deleteWhenTerminated = true, arguments = Map("ARG" -> "🔸")),
              OrderStarted,
              OrderNoticesExpected(Vector(
                planId / board.path / NoticeKey("🔸"),
                planId / bBoard.path / NoticeKey("🔸"),
                planId / cBoard.path / NoticeKey("🔸"))),
              OrderNoticesRead,
              OrderMoved(Position(1)),
              OrderFinished(),
              OrderDeleted))
    }

    "WhenNotAnnounced.SkipWhenNoNotice when Notice is not announced" in:
      eventWatch.resetLastWatchedEventId()
      val planSchema = PlanSchema.joc(PlanSchemaId("DailyPlan"))
      val day = "2024-11-24"
      val planId = planSchema.id / day
      val workflow =
        Workflow(WorkflowPath("CONSUMING"), Seq(
          ExpectNotices(boardPathExpr, whenNotAnnounced = SkipWhenNoNotice),
          ConsumeNotices(boardPathExpr, whenNotAnnounced = SkipWhenNoNotice):
            EmptyInstruction()))
      withItems(
        (planSchema, board, bBoard, cBoard, workflow)
      ): (_, _, _, _, workflow) =>
        val noticeId = planId / board.path / NoticeKey("🔸")

        // Post one of the two required Notices
        execCmd:
          ControllerCommand.PostNotice(noticeId)

        val orderId = OrderId(s"#$day#CONSUME")
        controller.addOrderBlocking:
          FreshOrder(orderId, workflow.path, planId = planId, deleteWhenTerminated = true,
            arguments = Map("ARG" -> "🔸"))
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, planId = planId,
            deleteWhenTerminated = true, arguments = Map("ARG" -> "🔸")),
          OrderStarted,
          OrderNoticesRead,
          OrderMoved(Position(1), Some(OrderMoved.NoNotice)),
          OrderNoticesConsumptionStarted(Vector(
            planId / board.path / NoticeKey("🔸"),
            planId / bBoard.path / NoticeKey("🔸"),
            planId / cBoard.path / NoticeKey("🔸"))),
          OrderMoved(Position(1) / "consumeNotices" % 1),
          OrderNoticesConsumed(),
          OrderFinished(),
          OrderDeleted))

    "WhenNotAnnounced.Ignore when Notice is not announced" in:
      eventWatch.resetLastWatchedEventId()
      val planSchema = PlanSchema.joc(PlanSchemaId("DailyPlan"))
      val day = "2024-11-25"
      val planId = planSchema.id / day
      val workflow =
        Workflow(WorkflowPath("CONSUMING"), Seq(
          ExpectNotices(boardPathExpr, whenNotAnnounced = DontWait),
          ConsumeNotices(boardPathExpr, whenNotAnnounced = DontWait):
            EmptyInstruction()))
      withItems(
        (planSchema, board, bBoard, cBoard, workflow)
      ): (_, _, _, _, workflow) =>
        val noticeId = planId / board.path / NoticeKey("🔸")

        // Post one of the two required Notices
        execCmd:
          ControllerCommand.PostNotice(noticeId)

        val orderId = OrderId(s"#$day#CONSUME")
        controller.addOrderBlocking:
          FreshOrder(orderId, workflow.path, planId = planId, deleteWhenTerminated = true,
            arguments = Map("ARG" -> "🔸"))
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, planId = planId,
            deleteWhenTerminated = true, arguments = Map("ARG" -> "🔸")),
          OrderStarted,
          OrderNoticesRead,
          OrderMoved(Position(1), Some(OrderMoved.NoNotice)),
          OrderNoticesConsumptionStarted(Vector(
            planId / board.path / NoticeKey("🔸"), // <-- only the posted Notice is consumed
            planId / bBoard.path / NoticeKey("🔸")/*Despite it's not consumed !!!*/,
            planId / cBoard.path / NoticeKey("🔸")/*Despite it's not consumed !!!*/)),
          OrderMoved(Position(1) / "consumeNotices" % 1),
          OrderNoticesConsumed(),
          OrderFinished(),
          OrderDeleted))
  }


object PlannableBoardTest:

  private val agentPath = AgentPath("AGENT")

  private val board = PlannableBoard(BoardPath("A-BOARD"),
    postOrderToNoticeKey = expr("$ARG"),
    expectOrderToNoticeKey = expr("$ARG"))
  private val bBoard = PlannableBoard(BoardPath("B-BOARD"),
    postOrderToNoticeKey = expr("$ARG"),
    expectOrderToNoticeKey = expr("$ARG"))
  private val cBoard = PlannableBoard(BoardPath("C-BOARD"),
    postOrderToNoticeKey = expr("$ARG"),
    expectOrderToNoticeKey = expr("$ARG"))

  private val consumingWorkflow =
    Workflow(WorkflowPath("CONSUMING"), Seq(
      ConsumeNotices(ExpectNotice(board.path)):
        EmptyInstruction()))
