package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.time.{TestWallClock, Timestamp}
import js7.data.board.BoardPathExpression.syntax.*
import js7.data.board.BoardPathExpression.syntax.boardPathToExpr
import js7.data.board.{BoardPath, BoardPathExpression, BoardState, Notice, NoticeKey, NoticePlace, PlannableBoard}
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.instructions.ConsumeNoticesExecutorTest.*
import js7.data.execution.workflow.instructions.ConsumeNoticesExecutorTest.NoticeState.{Announced, Posted, Unknown}
import js7.data.order.OrderEvent.OrderMoved.NoNotice
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.plan.{PlanSchema, PlanSchemaId}
import js7.data.state.ControllerTestStateView
import js7.data.workflow.instructions.ConsumeNotices
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced.{DontWait, SkipWhenNoNotice, Wait}
import js7.data.workflow.position.Position
import js7.data.workflow.{Instruction, Workflow, WorkflowPath}
import org.scalactic.source
import scala.language.implicitConversions
import scala.runtime.stdLibPatches.Predef.assert

final class ConsumeNoticesExecutorTest extends OurTestSuite:

  "toEvents" - {
    "Every Notice is announced or posted" - {
      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Announced, B -> Announced, C -> Announced), any,
          OrderNoticesExpected(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Announced, C -> Announced), any,
          OrderNoticesConsumptionStarted(Vector(
            A / NoticeKey.empty)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Announced, B -> Posted, C -> Announced), any,
          OrderNoticesExpected(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Announced, B -> Announced, C -> Posted), any,
          OrderNoticesExpected(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Announced, B -> Posted, C -> Posted), any,
          OrderNoticesConsumptionStarted(Vector(
            B / NoticeKey.empty,
            C / NoticeKey.empty)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), any,
          OrderNoticesConsumptionStarted(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))
    }

    "Nothing announced or posted Notices" - {
      "Wait: no difference between unknown and announced Notices (see above)" - {
        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> notPosted, B -> notPosted, C -> notPosted), Wait,
            OrderNoticesExpected(Vector(
              A / NoticeKey.empty,
              B / NoticeKey.empty,
              C / NoticeKey.empty)))

        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> Posted, B -> notPosted, C -> notPosted), Wait,
            OrderNoticesConsumptionStarted(Vector(
              A / NoticeKey.empty)))

        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> notPosted, B -> Posted, C -> notPosted), Wait,
            OrderNoticesExpected(Vector(
              A / NoticeKey.empty,
              B / NoticeKey.empty,
              C / NoticeKey.empty)))

        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> notPosted, B -> notPosted, C -> Posted), Wait,
            OrderNoticesExpected(Vector(
              A / NoticeKey.empty,
              B / NoticeKey.empty,
              C / NoticeKey.empty)))

        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> notPosted, B -> Posted, C -> Posted), Wait,
            OrderNoticesConsumptionStarted(Vector(
              B / NoticeKey.empty,
              C / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), Wait,
          OrderNoticesConsumptionStarted(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))
      }

      "Nothing announced or posted" - {
        testConsumeNotices(A | B & C, Map(), Wait,
          OrderNoticesExpected(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(), SkipWhenNoNotice,
          OrderNoticesRead,
          OrderMoved(Position(1), Some(NoNotice)))

        testConsumeNotices(A | B & C, Map(), DontWait,
          OrderNoticesConsumptionStarted(Vector()))
      }

      "A posted" - {
        testConsumeNotices(A | B & C, Map(A -> Posted), Wait,
          OrderNoticesConsumptionStarted(Vector(
            A / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(A -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            A / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(A -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            A / NoticeKey.empty)))
      }

      "B posted" - {
        testConsumeNotices(A | B & C, Map(B -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            B / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(B -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            B / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(B -> Posted), Wait,
          OrderNoticesExpected(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))
      }

      "C posted" - {
        testConsumeNotices(A | B & C, Map(C -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            C / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(C -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            C / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(C -> Posted), Wait,
          OrderNoticesExpected(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))
      }

      "B and C posted" - {
        testConsumeNotices(A | B & C, Map(B -> Posted, C -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            B / NoticeKey.empty,
            C / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(B -> Posted, C -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            B / NoticeKey.empty,
            C / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(B -> Posted, C -> Posted), Wait,
          OrderNoticesConsumptionStarted(Vector(
            B / NoticeKey.empty,
            C / NoticeKey.empty)))
      }

      "A, B and C posted" - {
        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))

        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), Wait,
          OrderNoticesConsumptionStarted(Vector(
            A / NoticeKey.empty,
            B / NoticeKey.empty,
            C / NoticeKey.empty)))
      }
    }

    "Mixed unknown, announced and posted notices" - {
      "When A (left-side of OR) is posted, the others don't matter" - {
        for any <- Seq(Wait, SkipWhenNoNotice, DontWait) do
          testConsumeNotices(A | B & C, Map(A -> Posted, B -> Announced), any,
            OrderNoticesConsumptionStarted(Vector(
              A / NoticeKey.empty)))
        }

      testConsumeNotices(
        A | B & C,
        Map(A -> Announced, B -> Posted),
        Wait,
        OrderNoticesExpected(Vector(
          A / NoticeKey.empty,
          B / NoticeKey.empty,
          C / NoticeKey.empty)))

      testConsumeNotices(
        A | B & C,
        Map(A -> Announced, B -> Posted),
        DontWait,
        OrderNoticesConsumptionStarted(Vector(
          B / NoticeKey.empty)))

      testConsumeNotices(
        A | B & C,
        Map(A -> Announced, B -> Posted),
        SkipWhenNoNotice,
        OrderNoticesConsumptionStarted(Vector(
          B / NoticeKey.empty)))

      testConsumeNotices(
        A | B & C,
        Map(B -> Announced, C -> Posted),
        Wait,
        OrderNoticesExpected(Vector(
          A / NoticeKey.empty,
          B / NoticeKey.empty,
          C / NoticeKey.empty)))

      testConsumeNotices(
        A | B & C,
        Map(B -> Announced, C -> Posted),
        DontWait,
        OrderNoticesConsumptionStarted(Vector(
          C / NoticeKey.empty)))

      testConsumeNotices(
        A | B & C,
        Map(B -> Announced, C -> Posted),
        SkipWhenNoNotice,
        OrderNoticesConsumptionStarted(Vector(
          C / NoticeKey.empty)))
    }
  }

  /** Execute a ConsumeNotices instruction. */
  private def testConsumeNotices(
    boardExpr: BoardPathExpression,
    boardToNoticeState: Map[BoardPath, NoticeState],
    whenNotAnnounced: WhenNotAnnounced,
    expect: OrderEvent.OrderActorEvent*)
    (using source.Position)
  : Unit =
    val bToNS = boardToNoticeState.filter(_._2 != Unknown)
    val notices =
      if bToNS.isEmpty then
        "nothing announced or posted"
      else
        bToNS.map((k, v) => s"${k.string}: $v").mkString(" ")
    val testName =
      s"$boardExpr · $notices · $whenNotAnnounced"
    testName in:
      assert(consumeNotices(boardExpr, whenNotAnnounced, bToNS) == Right(expect))


object ConsumeNoticesExecutorTest:

  private val A = BoardPath("A")
  private val B = BoardPath("B")
  private val C = BoardPath("C")
  private val boardPaths = Seq(A, B, C)
  private val dailyPlan = PlanSchema.joc(PlanSchemaId("DailyPlan"))
  private val day = "2024-11-25"
  private val planId = dailyPlan.id / day
  private val plannedNoticeKey = planId.emptyPlannedNoticeKey
  private val orderId = OrderId(s"#$day#")

  /** Execute a ConsumeNotices instruction. */
  private def consumeNotices(
    boardExpr: BoardPathExpression,
    whenNotAnnounced: WhenNotAnnounced,
    boardToNoticeState: Map[BoardPath, NoticeState] = Map.empty)
  : Checked[List[OrderEvent.OrderActorEvent]] =
    val instr = ConsumeNotices(
      boardExpr,
      whenNotAnnounced = whenNotAnnounced,
      Workflow.empty)
    val workflow = Workflow.of(WorkflowPath("WORKFLOW"), instr)
    val order = Order(orderId, workflow.id /: Position(0), Order.Ready, maybePlanId = Some(planId))
    val controllerState = ControllerTestStateView.of(
      workflows = Some(Seq(workflow)),
      orders = Some(Seq(order)),
      itemStates = boardPaths.map: boardPath =>
        toBoardState(boardPath, boardToNoticeState.getOrElse(boardPath, Unknown)))

    val result = ConsumeNoticesExecutor:
      InstructionExecutorService(TestWallClock(ts"2024-11-25T12:00:00Z"))
    .toEvents(instr, order, controllerState)
    result.map(_.map:
      case KeyedEvent(`orderId`, event) => event)

  private def toBoardState(boardPath: BoardPath, noticeState: NoticeState)
  : BoardState =
    BoardState(
      PlannableBoard(boardPath),
      toNoticePlace =
        noticeState match
          case Announced =>
            Map(plannedNoticeKey -> NoticePlace(isAnnounced = true))
          case Posted =>
            Map(plannedNoticeKey -> NoticePlace(Some(Notice(boardPath / plannedNoticeKey))))
          case Unknown =>
            Map.empty)

  private[instructions] enum NoticeState:
    case Announced
    case Posted
    /** Same as a missing NoticeState. */
    case Unknown
