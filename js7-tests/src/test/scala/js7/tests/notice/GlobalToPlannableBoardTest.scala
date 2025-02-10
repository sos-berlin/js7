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
import js7.data.board.{BoardPath, GlobalBoard, GlobalNoticeKey, Notice, NoticeEvent, NoticeKey, NoticePlace, PlannableBoard}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{ChangeGlobalToPlannableBoard, PostNotice}
import js7.data.event.Event
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.item.ItemRevision
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemChanged
import js7.data.order.Order.ExpectingNotices
import js7.data.order.OrderEvent.{OrderAdded, OrderDeleted, OrderFinished, OrderMoved, OrderNoticePosted, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderStarted, OrderStateReset, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanSchema, PlanSchemaId}
import js7.data.value.expression.ExprFunction.testing.|=>
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.workflow.instructions.{ConsumeNotices, PostNotices}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.notice.GlobalToPlannableBoardTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.language.implicitConversions

final class GlobalToPlannableBoardTest
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

  "Two Plans of same PlanSchema" in:
    withItems((
      dailyPlan, globalBoard, postingWorkflow, consumingWorkflow
    )): (dailyPlan, globalBoard, postingWorkflow, consumingWorkflow) =>
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
        PostNotice(PlanId.Global / boardPath / s"${day}BETA")

      // Expect a notice //
      val otherDay = "2025-01-21"
      val otherPlanId = dailyPlan.id / otherDay
      val consumingOrderId = OrderId(s"#$otherDay#NOTICE")
      controller.addOrderBlocking:
        FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNextKey[OrderNoticesExpected](consumingOrderId)

      assert(controllerState.toNoticePlace.toMap == Map(
        boardPath \ NoticeKey("2025-01-20ALPHA") -> NoticePlace(
          Some(Notice(
            PlanId.Global / boardPath / "2025-01-20ALPHA",
            endOfLife = Some(ts"2099-01-01T01:00:00Z")))),
        boardPath \ NoticeKey("2025-01-20BETA") -> NoticePlace(
          Some(Notice(
            PlanId.Global / boardPath / "2025-01-20BETA",
            endOfLife = Some(ts"2099-01-01T01:00:00Z")))),
        boardPath \ NoticeKey("2025-01-21NOTICE") -> NoticePlace(
          expectingOrderIds = Set(consumingOrderId))))

      // Change GlobalBoard to PlannableBoard //
      // Notices and expecting Order will be changed

      val checked = controller.api.updateItems(fs2.Stream.emit:
        AddOrChangeSimple(plannableBoard))
      .await(99.s)
      assert(checked == Left(Problem("Type of BoardItem cannot be changed")))

      execCmd:
        ChangeGlobalToPlannableBoard(plannableBoard, dailyPlan.id,
          exprFunction("(noticeKey) => [ substring($noticeKey, 0, 10), substring($noticeKey, 10) ]"))

      assert(controllerState.toNoticePlace.toMap == Map(
        planId / boardPath / NoticeKey("ALPHA") -> NoticePlace(Some(Notice(planId / boardPath / "ALPHA"))),
        planId / boardPath / NoticeKey("BETA") -> NoticePlace(Some(Notice(planId / boardPath / "BETA"))),
        otherPlanId / boardPath / NoticeKey("NOTICE") -> NoticePlace(
          expectingOrderIds = Set(consumingOrderId))))

      assert(controllerState.idToOrder(consumingOrderId).isState[ExpectingNotices])
      execCmd:
        PostNotice(otherPlanId / boardPath / "NOTICE")
      eventWatch.awaitNextKey[OrderFinished](consumingOrderId)

      val events = eventWatch.keyedEvents(after = startEventId)
      assert(events.dropWhile(ke => !ke.event.isInstanceOf[OrderAdded]) == Seq(
        postingOrderId <-: OrderAdded(postingWorkflow.id, planId = Some(planId), deleteWhenTerminated = true),
        postingOrderId <-: OrderStarted,
        postingOrderId <-: OrderNoticePosted(boardPath \ "2025-01-20ALPHA", endOfLife = Some(ts"2099-01-01T01:00:00Z")),
        postingOrderId <-: OrderMoved(Position(1)),
        postingOrderId <-: OrderFinished(),
        postingOrderId <-: OrderDeleted,

        boardPath <-: NoticePosted(PlanId.Global / NoticeKey("2025-01-20BETA"), Some(ts"2099-01-01T01:00:00Z")),

        consumingOrderId <-: OrderAdded(consumingWorkflow.id, planId = Some(otherPlanId), deleteWhenTerminated = true),
        consumingOrderId <-: OrderStarted,
        consumingOrderId <-: OrderNoticesExpected(Vector(boardPath \ "2025-01-21NOTICE")),

        // ChangeGlobalToPlannableBoard //
        NoKey <-: UnsignedSimpleItemChanged(plannableBoard.withRevision(Some(ItemRevision(1)))),
        boardPath <-: NoticeMoved(GlobalNoticeKey("2025-01-20ALPHA"), planId / NoticeKey("ALPHA"), endOfLife = None),
        boardPath <-: NoticeMoved(GlobalNoticeKey("2025-01-20BETA"), planId / NoticeKey("BETA"), endOfLife = None),
        consumingOrderId <-: OrderStateReset,
        consumingOrderId <-: OrderNoticesExpected(Vector(otherPlanId / boardPath / "NOTICE")),

        boardPath <-: NoticePosted(otherPlanId / NoticeKey("NOTICE")),
        consumingOrderId <-: OrderNoticesConsumptionStarted(Vector(otherPlanId / boardPath / "NOTICE")),
        consumingOrderId <-: OrderNoticesConsumed(),
        consumingOrderId <-: OrderFinished(),
        consumingOrderId <-: OrderDeleted))

  "Change GlobalBoard to PlannableBoard, using a wrong NoticeKey pattern" in:
    withItems((
      dailyPlan, globalBoard, postingWorkflow, consumingWorkflow
    )): (dailyPlan, globalBoard, postingWorkflow, consumingWorkflow) =>
      val startEventId = eventWatch.resetLastWatchedEventId()
      val day = "2025-01-22"
      val planId = dailyPlan.id / day

      // Post a Notice via Command //
      execCmd:
        PostNotice(PlanId.Global / boardPath / s"${day}A")

      // Expect a notice //
      val consumingOrderId = OrderId(s"#$day#B")
      controller.addOrderBlocking:
        FreshOrder(consumingOrderId, consumingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNextKey[OrderNoticesExpected](consumingOrderId)

      assert(controllerState.toNoticePlace.toMap == Map(
        boardPath \ NoticeKey("2025-01-22A") -> NoticePlace(
          Some(Notice(
            PlanId.Global / boardPath / "2025-01-22A",
            endOfLife = Some(ts"2099-01-01T01:00:00Z")))),
        boardPath \ NoticeKey("2025-01-22B") -> NoticePlace(
          expectingOrderIds = Set(consumingOrderId))))

      // Change GlobalBoard to PlannableBoard //
      // Notices and expecting Order will be changed

      val checked = controller.api.updateItems(fs2.Stream.emit(AddOrChangeSimple(plannableBoard)))
        .await(99.s)
      assert(checked == Left(Problem("Type of BoardItem cannot be changed")))

      execCmd:
        // Insert a wrong 🔵 into the PlanKey such that the PlanId no longer match
        // the  ConsumeNotice instruction.
        // Because the Engine does not detect this (it's up to the responsible user),
        // the Engine must at least do not fail.

        ChangeGlobalToPlannableBoard(plannableBoard, dailyPlan.id,
          "noticeKey" |=> expr:
            """[ "$(substring($noticeKey, 0, 7))🔵$(substring($noticeKey, 8, 10))", substring($noticeKey, 10) ]""")

      val wrongPlanId = dailyPlan.id / "2025-01🔵22"
      assert(controllerState.toNoticePlace.toMap == Map(
        wrongPlanId / boardPath / NoticeKey("A") -> NoticePlace(Some(Notice(wrongPlanId / boardPath / "A"))),
        planId / boardPath / NoticeKey("B") -> NoticePlace(expectingOrderIds = Set(consumingOrderId))))

      assert(controllerState.idToOrder(consumingOrderId).isState[ExpectingNotices])
      execCmd:
        PostNotice(planId / boardPath / "B")
      eventWatch.awaitNextKey[OrderFinished](consumingOrderId)

      val events = eventWatch.keyedEvents(after = startEventId)
      assert(events.dropWhile(ke => !ke.event.isInstanceOf[NoticePosted]) == Seq(
        boardPath <-: NoticePosted(PlanId.Global / NoticeKey("2025-01-22A"), Some(ts"2099-01-01T01:00:00Z")),

        consumingOrderId <-: OrderAdded(consumingWorkflow.id, planId = Some(planId), deleteWhenTerminated = true),
        consumingOrderId <-: OrderStarted,
        consumingOrderId <-: OrderNoticesExpected(Vector(boardPath \ "2025-01-22B")),

        NoKey <-: UnsignedSimpleItemChanged(plannableBoard.withRevision(Some(ItemRevision(1)))),
        boardPath <-: NoticeMoved(GlobalNoticeKey("2025-01-22A"), wrongPlanId / NoticeKey("A"), endOfLife = None),
        consumingOrderId <-: OrderStateReset,
        consumingOrderId <-: OrderNoticesExpected(Vector(planId / boardPath / "B")),

        boardPath <-: NoticePosted(planId / NoticeKey("B")),
        consumingOrderId <-: OrderNoticesConsumptionStarted(Vector(planId / boardPath / "B")),
        consumingOrderId <-: OrderNoticesConsumed(),
        consumingOrderId <-: OrderFinished(),
        consumingOrderId <-: OrderDeleted))


object GlobalToPlannableBoardTest:

  private val agentPath = AgentPath("AGENT")
  private val boardPath = BoardPath("BOARD")

  private val dailyPlan = PlanSchema.joc(PlanSchemaId("DailyPlan"))

  private val globalBoard = GlobalBoard(
    boardPath,
    postOrderToNoticeKey = expr("""match(orderId, '#(.+)#(.*)', '$1$2')"""),
    expectOrderToNoticeKey = expr("""match(orderId, '#(.+)#(.*)', '$1$2')"""),
    endOfLife = expr("""$js7EpochMilli + 3600*1000"""))

  private val plannableBoard = PlannableBoard(
    boardPath,
    postOrderToNoticeKey = expr("""match(orderId, '#.+#(.*)', '$1')"""),
    expectOrderToNoticeKey = expr("""match(orderId, '#.+#(.*)', '$1')"""))

  private val postingWorkflow = Workflow(WorkflowPath("POSTING"), Seq(
    PostNotices(Seq(boardPath))))

  private val consumingWorkflow = Workflow(WorkflowPath("CONSUMING"), Seq(
    ConsumeNotices(boardPath)()))
