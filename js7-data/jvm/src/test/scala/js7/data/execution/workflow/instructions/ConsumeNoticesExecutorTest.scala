package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.time.{TestWallClock, Timestamp}
import js7.data.board.BoardPathExpression.syntax.*
import js7.data.board.BoardPathExpression.syntax.boardPathToExpr
import js7.data.board.{BoardPath, BoardPathExpression, BoardState, Notice, NoticeKey, NoticePlace, PlannableBoard, PlannedBoard}
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.instructions.ConsumeNoticesExecutorTest.*
import js7.data.execution.workflow.instructions.ConsumeNoticesExecutorTest.NoticeState.{Announced, Posted, Unknown}
import js7.data.order.OrderEvent.OrderMoved.NoNotice
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.plan.{Plan, PlanSchema, PlanSchemaId, PlanSchemaState}
import js7.data.state.ControllerTestStateView
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.ConsumeNotices
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced.{DontWait, SkipWhenNoNotice, Wait}
import js7.data.workflow.position.Position
import js7.data.workflow.{Instruction, Workflow, WorkflowPath}
import org.scalactic.source
import scala.language.implicitConversions
import scala.runtime.stdLibPatches.Predef.assert
import js7.base.time.ScalaTime.*

final class ConsumeNoticesExecutorTest extends OurTestSuite:

  "toEvents" - {
    "Every Notice is announced or posted" - {
      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Announced, B -> Announced, C -> Announced), any,
          OrderNoticesExpected(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Announced, C -> Announced), any,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey /*despite notice does not exist*/,
            planId / C / noticeKey /*despite notice does not exist*/)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Announced, B -> Posted, C -> Announced), any,
          OrderNoticesExpected(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Announced, B -> Announced, C -> Posted), any,
          OrderNoticesExpected(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Announced, B -> Posted, C -> Posted), any,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey /*despite notice does not exist*/,
            planId / B / noticeKey,
            planId / C / noticeKey)))

      for any <- Seq(Wait, DontWait, SkipWhenNoNotice) do
        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), any,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))
    }

    "Nothing announced, or posted Notices" - {
      "Wait: no difference between unknown and announced Notices (see above)" - {
        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> notPosted, B -> notPosted, C -> notPosted), Wait,
            OrderNoticesExpected(Vector(
              planId / A / noticeKey,
              planId / B / noticeKey,
              planId / C / noticeKey)))

        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> Posted, B -> notPosted, C -> notPosted), Wait,
            OrderNoticesConsumptionStarted(Vector(
              planId / A / noticeKey,
              planId / B / noticeKey /*despite notice does not exist*/,
              planId / C / noticeKey /*despite notice does not exist*/)))

        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> notPosted, B -> Posted, C -> notPosted), Wait,
            OrderNoticesExpected(Vector(
              planId / A / noticeKey,
              planId / B / noticeKey,
              planId / C / noticeKey)))

        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> notPosted, B -> notPosted, C -> Posted), Wait,
            OrderNoticesExpected(Vector(
              planId / A / noticeKey,
              planId / B / noticeKey,
              planId / C / noticeKey)))

        for notPosted <- Seq(Unknown, Announced) do
          testConsumeNotices(A | B & C, Map(A -> notPosted, B -> Posted, C -> Posted), Wait,
            OrderNoticesConsumptionStarted(Vector(
              planId / A / noticeKey /*despite notice does not exist*/,
              planId / B / noticeKey,
              planId / C / noticeKey)))

        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), Wait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))
      }

      "Nothing announced or posted" - {
        testConsumeNotices(A | B & C, Map(), Wait,
          OrderNoticesExpected(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))

        testConsumeNotices(A | B & C, Map(), SkipWhenNoNotice,
          OrderNoticesRead,
          OrderMoved(Position(1), Some(NoNotice)))

        testConsumeNotices(A | B & C, Map(), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey /*despite notice does not exist*/,
            planId / B / noticeKey /*despite notice does not exist*/,
            planId / C / noticeKey /*despite notice does not exist*/)))
      }

      "A posted" - {
        testConsumeNotices(A | B & C, Map(A -> Posted), Wait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey, /*despite notice does not exist*/
            planId / C / noticeKey /*despite notice does not exist*/)))

        testConsumeNotices(A | B & C, Map(A -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey, /*despite notice does not exist*/
            planId / C / noticeKey /*despite notice does not exist*/)))

        testConsumeNotices(A | B & C, Map(A -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey, /*despite notice does not exist*/
            planId / C / noticeKey /*despite notice does not exist*/)))
      }

      "B posted" - {
        testConsumeNotices(A | B & C, Map(B -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey /*despite notice does not exist*/,
            planId / B / noticeKey,
            planId / C / noticeKey /*despite notice does not exist*/)))

        testConsumeNotices(A | B & C, Map(B -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey /*despite notice does not exist*/ ,
            planId / B / noticeKey,
            planId / C / noticeKey /*despite notice does not exist*/)))

        testConsumeNotices(A | B & C, Map(B -> Posted), Wait,
          OrderNoticesExpected(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))
      }

      "C posted" - {
        testConsumeNotices(A | B & C, Map(C -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey /*despite notice does not exist*/,
            planId / B / noticeKey /*despite notice does not exist*/,
            planId / C / noticeKey)))

        testConsumeNotices(A | B & C, Map(C -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey /*despite notice does not exist*/,
            planId / B / noticeKey /*despite notice does not exist*/,
            planId / C / noticeKey)))

        testConsumeNotices(A | B & C, Map(C -> Posted), Wait,
          OrderNoticesExpected(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))
      }

      "B and C posted" - {
        testConsumeNotices(A | B & C, Map(B -> Posted, C -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey /*despite notice does not exist*/,
            planId / B / noticeKey,
            planId / C / noticeKey)))

        testConsumeNotices(A | B & C, Map(B -> Posted, C -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey /*despite notice does not exist*/,
            planId / B / noticeKey,
            planId / C / noticeKey)))

        testConsumeNotices(A | B & C, Map(B -> Posted, C -> Posted), Wait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey /*despite notice does not exist*/,
            planId / B / noticeKey,
            planId / C / noticeKey)))
      }

      "A, B and C posted" - {
        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), SkipWhenNoNotice,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))

        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), DontWait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))

        testConsumeNotices(A | B & C, Map(A -> Posted, B -> Posted, C -> Posted), Wait,
          OrderNoticesConsumptionStarted(Vector(
            planId / A / noticeKey,
            planId / B / noticeKey,
            planId / C / noticeKey)))
      }
    }

    "Mixed unknown, announced and posted notices" - {
      "When A (left-side of OR) is posted, the others don't matter" - {
        for any <- Seq(Wait, SkipWhenNoNotice, DontWait) do
          testConsumeNotices(A | B & C, Map(A -> Posted, B -> Announced), any,
            OrderNoticesConsumptionStarted(Vector(
              planId / A / noticeKey,
              planId / B / noticeKey /*despite notice does not exist*/,
              planId / C / noticeKey /*despite notice does not exist*/)))
        }

      testConsumeNotices(
        A | B & C,
        Map(A -> Announced, B -> Posted),
        Wait,
        OrderNoticesExpected(Vector(
          planId / A / noticeKey,
          planId / B / noticeKey,
          planId / C / noticeKey)))

      testConsumeNotices(
        A | B & C,
        Map(A -> Announced, B -> Posted),
        DontWait,
        OrderNoticesConsumptionStarted(Vector(
          planId / A / noticeKey /*despite notice does not exist*/,
          planId / B / noticeKey,
          planId / C / noticeKey /*despite notice does not exist*/)))

      testConsumeNotices(
        A | B & C,
        Map(A -> Announced, B -> Posted),
        SkipWhenNoNotice,
        OrderNoticesConsumptionStarted(Vector(
          planId / A / noticeKey /*despite notice does not exist*/,
          planId / B / noticeKey,
          planId / C / noticeKey /*despite notice does not exist*/)))

      testConsumeNotices(
        A | B & C,
        Map(B -> Announced, C -> Posted),
        Wait,
        OrderNoticesExpected(Vector(
          planId / A / noticeKey,
          planId / B / noticeKey,
          planId / C / noticeKey)))

      testConsumeNotices(
        A | B & C,
        Map(B -> Announced, C -> Posted),
        DontWait,
        OrderNoticesConsumptionStarted(Vector(
          planId / A / noticeKey /*despite notice does not exist*/,
          planId / B / noticeKey /*despite notice does not exist*/,
          planId / C / noticeKey)))

      testConsumeNotices(
        A | B & C,
        Map(B -> Announced, C -> Posted),
        SkipWhenNoNotice,
        OrderNoticesConsumptionStarted(Vector(
          planId / A / noticeKey /*despite notice does not exist*/,
          planId / B / noticeKey /*despite notice does not exist*/,
          planId / C / noticeKey)))
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
    val testName = s"$boardExpr · $notices · $whenNotAnnounced"
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
  private val noticeKey = NoticeKey("🔸")
  private val plannedNoticeKey = planId / noticeKey
  private val orderId = OrderId(s"#$day#")

  /** Execute a ConsumeNotices instruction. */
  private def consumeNotices(
    boardExpr: BoardPathExpression,
    whenNotAnnounced: WhenNotAnnounced,
    boardToNoticeState: Map[BoardPath, NoticeState] = Map.empty)
  : Checked[List[OrderEvent.OrderActorEvent]] =
    val instr = ConsumeNotices(boardExpr, whenNotAnnounced = whenNotAnnounced, Workflow.empty)
    val workflow = Workflow.of(WorkflowPath("WORKFLOW"), instr)
    val order = Order(orderId, workflow.id /: Position(0), Order.Ready, maybePlanId = Some(planId),
      arguments = Map("ARG" -> StringValue("🔸")))
    val controllerState = ControllerTestStateView.of(
      workflows = Some(Seq(workflow)),
      orders = Some(Seq(order)),
      itemStates =
        boardPaths.map: boardPath =>
          BoardState(
            PlannableBoard(
              boardPath,
              postOrderToNoticeKey = expr("$ARG"),
              expectOrderToNoticeKey = expr("$ARG")))
        .appended:
          PlanSchemaState(
            dailyPlan,
            finishedPlanLifeTime = 0.s,
            namedValues = Map.empty,
            toPlan = Map(
              planId.planKey -> Plan(
                planId,
                Set(orderId),
                boardPaths.map: boardPath =>
                  PlannedBoard(
                    planId / boardPath,
                    boardToNoticeState.getOrElse(boardPath, Unknown) match
                      case Announced => Map(noticeKey -> NoticePlace(isAnnounced = true))
                      case Posted => Map(noticeKey -> NoticePlace(Some(Notice(boardPath / plannedNoticeKey))))
                      case Unknown => Map.empty),
                isClosed = false))))

    ConsumeNoticesExecutor:
      InstructionExecutorService(TestWallClock(ts"2024-11-25T12:00:00Z"))
    .toEvents(instr, order, controllerState)
    .map(_.map:
      case KeyedEvent(`orderId`, event) => event)


  private[instructions] enum NoticeState:
    case Announced
    case Posted
    /** Same as a missing NoticeState. */
    case Unknown
