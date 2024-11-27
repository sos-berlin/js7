package js7.tests.notice

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpression.ExpectNotice
import js7.data.board.BoardPathExpression.syntax.*
import js7.data.board.BoardPathExpression.syntax.boardPathToExpr
import js7.data.board.{BoardPath, BoardPathExpression, Notice, PlannableBoard}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, DeleteOrdersWhenTerminated}
import js7.data.order.OrderEvent.OrderNoticesConsumptionStarted.Consumption
import js7.data.order.OrderEvent.OrderNoticesExpected.Expected
import js7.data.order.OrderEvent.{OrderAdded, OrderDeleted, OrderFinished, OrderMoved, OrderNoticeAnnounced, OrderNoticePosted, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanItem, PlanItemId}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced.{DontWait, SkipWhenNoNotice, Wait}
import js7.data.workflow.instructions.{ConsumeNotices, EmptyInstruction, ExpectNotices, PostNotices, Prompt}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.notice.PlannableBoardTest.*
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import scala.language.implicitConversions
import scala.util.control.NonFatal

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
    withItems((
      PlanItem.joc(PlanItemId("DailyPlan")),
      board,
      Workflow(WorkflowPath("POSTING"), Seq(
        PostNotices(Seq(board.path)))),
      consumingWorkflow),
    ): (dailyPlan, _, postingWorkflow, consumingWorkflow) =>
      eventWatch.resetLastWatchedEventId()
      val day = "2024-11-08"

      // otherConsumingOrderId is in some other Plan //
      val day0 = "2024-11-07"
      val otherConsumingOrderId = OrderId(s"#$day0#CONSUME")

      locally:
        val otherConsumingOrder = FreshOrder(otherConsumingOrderId, consumingWorkflow.path)
        controller.addOrderBlocking(otherConsumingOrder)
        assert(controllerState.minimumOrderToPlanId(otherConsumingOrder) == Right(Some(
          dailyPlan.id / day0)))
        execCmd(DeleteOrdersWhenTerminated(Set(otherConsumingOrderId)))
        eventWatch.awaitNext[OrderNoticesExpected](_.key == otherConsumingOrderId)


      // Wait for notice, post notice, consume notice //
      val consumingOrderId = OrderId(s"#$day#CONSUME")
      controller.addOrderBlocking:
        FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNext[OrderNoticesExpected](_.key == consumingOrderId)

      // Post a Notice //
      val postingOrderId = OrderId(s"#$day#POST")
      controller.addOrderBlocking:
        FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
      eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

      eventWatch.awaitNext[OrderNoticesConsumed](_.key == consumingOrderId)
      eventWatch.awaitNext[OrderTerminated](_.key == consumingOrderId)

      val noticeId = (dailyPlan.id / day).noticeId

      assert(eventWatch.eventsByKey[OrderEvent](postingOrderId) == Seq(
        OrderAdded(postingWorkflow.id, planId = Some(dailyPlan.id / day), deleteWhenTerminated = true),
        OrderNoticeAnnounced(board.path, noticeId),
        OrderStarted,
        OrderNoticePosted(Notice(noticeId, board.path, endOfLife = None)),
        OrderMoved(Position(1), None),
        OrderFinished(),
        OrderDeleted))

      assert(eventWatch.eventsByKey[OrderEvent](consumingOrderId) == Seq(
        OrderAdded(consumingWorkflow.id, planId = Some(dailyPlan.id / day), deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticesExpected(Vector(
          Expected(board.path, noticeId))),
        OrderNoticesConsumptionStarted(Vector(
          Consumption(board.path, noticeId))),
        OrderMoved(Position(0) / "consumeNotices" % 1),
        OrderNoticesConsumed(),
        OrderFinished(),
        OrderDeleted))

      assert:
        controllerState.idToOrder(otherConsumingOrderId).state.isInstanceOf[Order.ExpectingNotices]
      execCmd:
        CancelOrders(Seq(otherConsumingOrderId))
      eventWatch.awaitNext[OrderTerminated](_.key == otherConsumingOrderId)

  "Two PlanItems" in:
    withItems((
      PlanItem(
        PlanItemId("DailyPlan"),
        expr("match($js7OrderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1') ?")),
      PlanItem(
        PlanItemId("WeeklyPlan"),
        expr("match($js7OrderId, '#([0-9]{4}w[0-9]{2})#.*', '$1') ?")),
      board,
      Workflow(WorkflowPath("POSTING"), Seq(
        PostNotices(Seq(board.path)))),
      consumingWorkflow),
    ): (_, weeklyPlan, _, postingWorkflow, consumingWorkflow) =>
      eventWatch.resetLastWatchedEventId()
      locally:
        val postingOrderId = OrderId("#2024w47#POST")
        controller.addOrderBlocking:
          FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
        eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

      locally:
        val consumingOrderId = OrderId("#2024w47#CONSUME")
        val consumingOrder = FreshOrder(consumingOrderId, consumingWorkflow.path)
        assert(controllerState.minimumOrderToPlanId(consumingOrder) ==
          Right(Some(weeklyPlan.id / "2024w47")))

        controller.addOrderBlocking(consumingOrder)
        eventWatch.awaitNext[OrderAdded](_.key == consumingOrderId)
        eventWatch.awaitNext[OrderNoticesConsumed](_.key == consumingOrderId)
        eventWatch.awaitNext[OrderTerminated](_.key == consumingOrderId)

        assert(controllerState.idToOrder(consumingOrderId).planId ==
          Some(PlanItemId("WeeklyPlan") / "2024w47"))
        execCmd(DeleteOrdersWhenTerminated(consumingOrderId :: Nil))

      //locally:
      //  val postingOrderId = OrderId("#2024w47#POST")
      //  controller.addOrderBlocking:
      //    FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
      //  eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
      //  eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)
      //
      //  val consumingOrderId = OrderId("#2024w47#CONSUME")
      //  controller.addOrderBlocking:
      //    FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
      //
      //  eventWatch.awaitNext[OrderNoticesConsumed](_.key == consumingOrderId)
      //  eventWatch.awaitNext[OrderTerminated](_.key == consumingOrderId)

  "Two PlanItems with overlapping OrderId patterns" in:
    withItems((
      PlanItem(
        PlanItemId("APlan"),
        expr("match($js7OrderId, '.*A.*-#(.+)#.*', '$1') ?")),
      PlanItem(
        PlanItemId("BPlan"),
        expr("match($js7OrderId, '.*B.*-#(.+)#.*', '$1') ?")),
      Workflow(WorkflowPath("POSTING"), Seq(
        PostNotices(Seq(board.path)))),
      board),
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

  "Announced Notices" - {
    // Each test will announce a Notice at bBoard

    val boardPathExpr = board.path & bBoard.path & cBoard.path

    def announcingTest[A](boardPath: BoardPath, day: String)(body: OrderId => A): A =
      val postingWorkflow = Workflow(WorkflowPath("POSTING"), Seq(
        Prompt(expr("'PROMPT'")),
        PostNotices(Seq(boardPath))))
      withItem(postingWorkflow): postingWorkflow =>
        val announcingOrderId = OrderId(s"#$day#POST")
        controller.addOrderBlocking:
          FreshOrder(announcingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
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
        val day = "2024-11-22"
        val workflow = Workflow(WorkflowPath("EXPECTING"), Seq(
          ConsumeNotices(boardPathExpr, whenNotAnnounced = Wait):
            EmptyInstruction()))
        withItems(
          (PlanItem.joc(PlanItemId("DailyPlan")), board, bBoard, cBoard, workflow)
        ): (dailyPlan, _, _, _, workflow) =>
          announcingTest(bBoard.path, day): announcingOrderId =>
            val orderId = OrderId(s"#$day#CONSUME")
            controller.addOrderBlocking:
              FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
            eventWatch.awaitNext[OrderNoticesExpected](_.key == orderId)

            val noticeId = (dailyPlan.id / day).noticeId

            // Post board and cBoard
            execCmd(ControllerCommand.PostNotice(board.path, noticeId))
            execCmd(ControllerCommand.PostNotice(cBoard.path, noticeId))

            // Post bBoard
            execCmd(AnswerOrderPrompt(announcingOrderId))
            eventWatch.awaitNext[OrderNoticePosted](_.key == announcingOrderId)

            eventWatch.awaitNext[OrderTerminated](_.key == orderId)
            assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
              OrderAdded(workflow.id, planId = Some(dailyPlan.id / day),
                deleteWhenTerminated = true),
              OrderStarted,
              OrderNoticesExpected(Vector(
                Expected(board.path, noticeId),
                Expected(bBoard.path, noticeId),
                Expected(cBoard.path, noticeId))),
              OrderNoticesConsumptionStarted(Vector(
                Consumption(board.path, noticeId),
                Consumption(bBoard.path, noticeId),
                Consumption(cBoard.path, noticeId))),
              OrderMoved(Position(0) / "consumeNotices" % 1),
              OrderNoticesConsumed(),
              OrderFinished(),
              OrderDeleted))

      "ExpectNotices" in:
        eventWatch.resetLastWatchedEventId()
        val day = "2024-11-23"
        val workflow =
          Workflow(WorkflowPath("CONSUMING"), Seq(
            ExpectNotices(boardPathExpr, whenNotAnnounced = Wait)))
        withItems(
          (PlanItem.joc(PlanItemId("DailyPlan")), board, bBoard, cBoard, workflow)
        ): (dailyPlan, _, _, _, workflow) =>
          announcingTest(bBoard.path, day): announcingOrderId =>
            val orderId = OrderId(s"#$day#EXPECT")
            controller.addOrderBlocking:
              FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
            eventWatch.awaitNext[OrderNoticesExpected](_.key == orderId)

            val noticeId = (dailyPlan.id / day).noticeId

            // Post board and cBoard
            execCmd(ControllerCommand.PostNotice(board.path, noticeId))
            execCmd(ControllerCommand.PostNotice(cBoard.path, noticeId))

            // Post cBoard
            execCmd(AnswerOrderPrompt(announcingOrderId))
            eventWatch.awaitNext[OrderNoticePosted](_.key == announcingOrderId)

            eventWatch.awaitNext[OrderTerminated](_.key == orderId)
            assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
              OrderAdded(workflow.id, planId = Some(dailyPlan.id / day),
                deleteWhenTerminated = true),
              OrderStarted,
              OrderNoticesExpected(Vector(
                Expected(board.path, noticeId),
                Expected(bBoard.path, noticeId),
                Expected(cBoard.path, noticeId))),
              OrderNoticesRead,
              OrderMoved(Position(1)),
              OrderFinished(),
              OrderDeleted))
    }

    "WhenNotAnnounced.SkipWhenNoNotice when Notice is not announced" in:
      eventWatch.resetLastWatchedEventId()
      val day = "2024-11-24"
      val workflow =
        Workflow(WorkflowPath("CONSUMING"), Seq(
          ExpectNotices(boardPathExpr, whenNotAnnounced = SkipWhenNoNotice),
          ConsumeNotices(boardPathExpr, whenNotAnnounced = SkipWhenNoNotice):
            EmptyInstruction()))
      withItems(
        (PlanItem.joc(PlanItemId("DailyPlan")), board, bBoard, cBoard, workflow)
      ): (dailyPlan, _, _, _, workflow) =>
        val planId = dailyPlan.id / day
        val noticeId = planId.noticeId

        // Post one of the two required Notices
        execCmd:
          ControllerCommand.PostNotice(board.path, noticeId)

        val orderId = OrderId(s"#$day#CONSUME")
        controller.addOrderBlocking:
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, planId = Some(planId),
            deleteWhenTerminated = true),
          OrderStarted,
          OrderNoticesRead,
          OrderMoved(Position(1), Some(OrderMoved.NoNotice)),
          OrderNoticesConsumptionStarted(Vector(Consumption(board.path, noticeId))),
          OrderMoved(Position(1) / "consumeNotices" % 1),
          OrderNoticesConsumed(),
          OrderFinished(),
          OrderDeleted))

    "WhenNotAnnounced.Ignore when Notice is not announced" in:
      eventWatch.resetLastWatchedEventId()
      val day = "2024-11-25"
      val workflow =
        Workflow(WorkflowPath("CONSUMING"), Seq(
          ExpectNotices(boardPathExpr, whenNotAnnounced = DontWait),
          ConsumeNotices(boardPathExpr, whenNotAnnounced = DontWait):
            EmptyInstruction()))
      withItems(
        (PlanItem.joc(PlanItemId("DailyPlan")), board, bBoard, cBoard, workflow)
      ): (dailyPlan, _, _, _, workflow) =>
        val planId = dailyPlan.id / day
        val noticeId = planId.noticeId

        // Post one of the two required Notices
        execCmd:
          ControllerCommand.PostNotice(board.path, noticeId)

        val orderId = OrderId(s"#$day#CONSUME")
        controller.addOrderBlocking:
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, planId = Some(planId),
            deleteWhenTerminated = true),
          OrderStarted,
          OrderNoticesRead,
          OrderMoved(Position(1), Some(OrderMoved.NoNotice)),
          OrderNoticesConsumptionStarted(Vector(
            Consumption(board.path, noticeId))), // <-- only the posted Notice is consumed
          OrderMoved(Position(1) / "consumeNotices" % 1),
          OrderNoticesConsumed(),
          OrderFinished(),
          OrderDeleted))
  }


object PlannableBoardTest:

  private val agentPath = AgentPath("AGENT")

  private val board = PlannableBoard(BoardPath("A-BOARD"))
  private val bBoard = PlannableBoard(BoardPath("B-BOARD"))
  private val cBoard = PlannableBoard(BoardPath("C-BOARD"))

  private val consumingWorkflow =
    Workflow(WorkflowPath("CONSUMING"), Seq(
      ConsumeNotices(ExpectNotice(board.path)):
        EmptyInstruction()))
