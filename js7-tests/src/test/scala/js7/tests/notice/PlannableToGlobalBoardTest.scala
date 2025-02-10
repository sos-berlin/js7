package js7.tests.notice

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.time.TestAlarmClock
import js7.base.time.TimestampForTests.ts
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpression.syntax.boardPathToExpr
import js7.data.board.NoticeEvent.{NoticeMoved, NoticePosted}
import js7.data.board.{BoardPath, GlobalBoard, GlobalNoticeKey, Notice, NoticeKey, NoticePlace, PlannableBoard}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{CancelOrders, ChangePlannableToGlobalBoard, PostNotice}
import js7.data.event.Event
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.item.ItemRevision
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemChanged
import js7.data.order.Order.ExpectingNotices
import js7.data.order.OrderEvent.{OrderAdded, OrderDeleted, OrderFinished, OrderMoved, OrderNoticeAnnounced, OrderNoticePosted, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderStarted, OrderStateReset, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanSchema, PlanSchemaId}
import js7.data.value.expression.Expression.exprFun
import js7.data.value.expression.ExpressionParser
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ConsumeNotices, PostNotices}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.notice.PlannableToGlobalBoardTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.language.implicitConversions

final class PlannableToGlobalBoardTest
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

  private lazy val clock = TestAlarmClock(ts"2099-01-01T00:00:00Z")

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  "Change PlannableBoard to GlobalBoard" in:
    withItems((
      dailyPlan, weeklyPlan, plannableBoard, postingWorkflow, consumingWorkflow
    )): (dailyPlan, weeklyPlan, plannableBoard, postingWorkflow, consumingWorkflow) =>
      val startEventId = eventWatch.resetLastWatchedEventId()
      val day = "2025-01-20"
      val planId = dailyPlan.id / day

      // Post a Notice via Order //
      val postingOrderId = OrderId(s"#$day#ALPHA")
      controller.addOrderBlocking:
        FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNext[OrderNoticePosted](_.key == postingOrderId)
      eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

      // Post a Notice via Command //
      execCmd:
        PostNotice(planId / boardPath / "BETA")

      // Expect a notice //
      val otherDay = "2025-01-21"
      val otherPlanId = dailyPlan.id / otherDay
      val consumingOrderId = OrderId(s"#$otherDay#NOTICE")
      controller.addOrderBlocking:
        FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNextKey[OrderNoticesExpected](consumingOrderId)

      // Expect a weekly notice — should be be changed //
      val week = "2025w03"
      val weekPlanId = weeklyPlan.id / week
      val weekConsumingOrderId = OrderId(s"#$week#NOTICE")
      controller.addOrderBlocking:
        FreshOrder(weekConsumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNextKey[OrderNoticesExpected](weekConsumingOrderId)

      execCmd:
        PostNotice(weekPlanId / boardPath / "POSTED")

      assert(controllerState.toNoticePlace.toMap == Map(
        planId / boardPath / NoticeKey("ALPHA") -> NoticePlace(Some(Notice(planId / boardPath / "ALPHA"))),
        planId / boardPath / NoticeKey("BETA") -> NoticePlace(Some(Notice(planId / boardPath / "BETA"))),
        otherPlanId / boardPath / NoticeKey("NOTICE") -> NoticePlace(expectingOrderIds = Set(consumingOrderId)),
        weekPlanId / boardPath / NoticeKey("NOTICE") -> NoticePlace(expectingOrderIds = Set(weekConsumingOrderId)),
        weekPlanId / boardPath / NoticeKey("POSTED") -> NoticePlace(Some(Notice(weekPlanId / boardPath / "POSTED")))))

      // Change PlannableBoard to GlobalBoard//
      // Notices and expecting Order will be changed

      val checked = controller.api.updateItems(fs2.Stream.emit:
        AddOrChangeSimple(globalBoard))
      .await(99.s)
      assert(checked == Left(Problem("Type of BoardItem cannot be changed")))

      execCmd:
        ChangePlannableToGlobalBoard(globalBoard, dailyPlan.id,
          exprFun"""(planKey, noticeKey) => "$$planKey$$noticeKey" """)

      assert(controllerState.toNoticePlace.toMap == Map(
        PlanId.Global / boardPath / NoticeKey("2025-01-20ALPHA") -> NoticePlace(
          Some(Notice(
            PlanId.Global / boardPath / "2025-01-20ALPHA",
            endOfLife = Some(ts"2099-01-01T01:00:00Z")))),
        PlanId.Global / boardPath / NoticeKey("2025-01-20BETA") -> NoticePlace(
          Some(Notice(
            PlanId.Global / boardPath / "2025-01-20BETA",
            endOfLife = Some(ts"2099-01-01T01:00:00Z")))),
        PlanId.Global / boardPath / NoticeKey("2025-01-21NOTICE") -> NoticePlace(
          expectingOrderIds = Set(consumingOrderId)),
        weekPlanId / boardPath / NoticeKey("NOTICE") -> NoticePlace(
          expectingOrderIds = Set(weekConsumingOrderId)),
        weekPlanId / boardPath / NoticeKey("POSTED") -> NoticePlace(
          Some(Notice(weekPlanId / boardPath / "POSTED")))))

      assert(controllerState.idToOrder(consumingOrderId).isState[ExpectingNotices])
      execCmd:
        PostNotice(boardPath \ "2025-01-21NOTICE")
      eventWatch.awaitNextKey[OrderFinished](consumingOrderId)

      val events = eventWatch.keyedEvents(after = startEventId)
      assert(events.dropWhile(ke => !ke.event.isInstanceOf[OrderAdded]) == Seq(
        postingOrderId <-: OrderAdded(postingWorkflow.id, planId = Some(planId), deleteWhenTerminated = true),
        postingOrderId <-: OrderNoticeAnnounced(planId / boardPath / "ALPHA"),
        postingOrderId <-: OrderStarted,
        postingOrderId <-: OrderNoticePosted(planId / boardPath / "ALPHA"),
        postingOrderId <-: OrderMoved(Position(1)),
        postingOrderId <-: OrderFinished(),
        postingOrderId <-: OrderDeleted,

        boardPath <-: NoticePosted(planId / NoticeKey("BETA")),

        consumingOrderId <-: OrderAdded(consumingWorkflow.id, planId = Some(otherPlanId),
          deleteWhenTerminated = true),
        consumingOrderId <-: OrderStarted,
        consumingOrderId <-: OrderNoticesExpected(Vector(otherPlanId / boardPath / "NOTICE")),

        weekConsumingOrderId <-: OrderAdded(consumingWorkflow.id, planId = Some(weekPlanId),
          deleteWhenTerminated = true),
        weekConsumingOrderId <-: OrderStarted,
        weekConsumingOrderId <-: OrderNoticesExpected(Vector(weekPlanId / boardPath / "NOTICE")),

        boardPath <-: NoticePosted(weekPlanId / NoticeKey("POSTED")),

        // ChangeGlobalToPlannableBoard //
        NoKey <-: UnsignedSimpleItemChanged(globalBoard.withRevision(Some(ItemRevision(1)))),
        boardPath <-: NoticeMoved(planId / NoticeKey("ALPHA"), GlobalNoticeKey("2025-01-20ALPHA"),
          endOfLife = Some(ts"2099-01-01T01:00:00Z")),
        boardPath <-: NoticeMoved(planId / NoticeKey("BETA"), GlobalNoticeKey("2025-01-20BETA"),
          endOfLife = Some(ts"2099-01-01T01:00:00Z")),
        consumingOrderId <-: OrderStateReset,
        consumingOrderId <-: OrderNoticesExpected(Vector(boardPath \ "2025-01-21NOTICE")),

        boardPath <-: NoticePosted(GlobalNoticeKey("2025-01-21NOTICE"),
          endOfLife = Some(ts"2099-01-01T01:00:00Z")),
        consumingOrderId <-: OrderNoticesConsumptionStarted(Vector(boardPath \ "2025-01-21NOTICE")),
        consumingOrderId <-: OrderNoticesConsumed(),
        consumingOrderId <-: OrderFinished(),
        consumingOrderId <-: OrderDeleted))

      execCmd:
        CancelOrders(weekConsumingOrderId :: Nil)


object PlannableToGlobalBoardTest:

  private val agentPath = AgentPath("AGENT")
  private val boardPath = BoardPath("BOARD")

  private val dailyPlan = PlanSchema.joc(PlanSchemaId("DailyPlan"))
  private val weeklyPlan = PlanSchema.weekly(PlanSchemaId("WeeklyPlan"))

  private val plannableBoard = PlannableBoard(
    boardPath,
    postOrderToNoticeKey = expr("""match(orderId, '#.+#(.*)', '$1')"""),
    expectOrderToNoticeKey = expr("""match(orderId, '#.+#(.*)', '$1')"""))

  private val globalBoard = GlobalBoard(
    boardPath,
    postOrderToNoticeKey = expr("""match(orderId, '#(.+)#(.*)', '$1$2')"""),
    expectOrderToNoticeKey = expr("""match(orderId, '#(.+)#(.*)', '$1$2')"""),
    endOfLife = expr("""$js7EpochMilli + 3600*1000"""))

  private val postingWorkflow = Workflow(WorkflowPath("POSTING"), Seq(
    PostNotices(Seq(boardPath))))

  private val consumingWorkflow = Workflow(WorkflowPath("CONSUMING"), Seq(
    ConsumeNotices(boardPath)()))
