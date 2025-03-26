package js7.data.plan

import io.circe.Codec
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.Problems.PlanIsDeletedProblem
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.board.{BoardPath, Notice, NoticeKey, PlannedBoard}
import js7.data.order.OrderId
import js7.data.plan.PlanEvent.{PlanClosed, PlanDeleted, PlanFinished, PlanOpened}
import js7.data.plan.PlanStatus.{Closed, Deleted, Open}
import js7.tester.CirceJsonTester.testJson

final class PlanTest extends OurTestSuite:

  "JSON" in:
    given Codec.AsObject[Plan.Snapshot] = TypedJsonCodec(Plan.subtype)
    testJson(
      Plan.Snapshot(
        PlanSchemaId("DailyPlan") / "2025-03-20",
        Open),
      json"""{
        "TYPE": "Plan",
        "planId": [ "DailyPlan", "2025-03-20" ],
        "status": "Open"
      }""")

  "PlanStatus changes" in:
    val now = ts"2025-03-20T12:00:00Z"

    val planId = PlanSchemaId("DailyPlan") / "2025-03-20"
    val orderId = OrderId("ORDER")
    val boardPath = BoardPath("BOARD")
    val noticeId = planId / boardPath / NoticeKey("NOTICE")

    var plan = Plan(planId, Deleted)
    assert(plan.addOrders(orderId :: Nil) == Left(PlanIsDeletedProblem(planId)))

    assert(plan.changePlanStatusEvents(Open, now, 1.h).map(_.toList) == Right:
      List(planId <-: PlanOpened))
    plan = plan.applyEvent(PlanOpened).orThrow

    assert(plan.changePlanStatusEvents(Open, now, 1.h) == Left:
      Problem(s"$planId is already Open"))

    assert(plan.changePlanStatusEvents(Closed, now, 1.h).map(_.toList) == Right:
      List(
        planId <-: PlanClosed,
        planId <-: PlanFinished(now)))

    assert(plan.changePlanStatusEvents(Closed, now, 0.h).map(_.toList) == Right:
      List(
        planId <-: PlanClosed,
        planId <-: PlanFinished(now),
        planId <-: PlanDeleted))

    assert(plan.status == Open)
    plan = plan.addOrders(orderId :: Nil).orThrow
    plan = plan.addBoard:
      PlannedBoard(planId / boardPath)
        .addNotice(Notice(noticeId)).orThrow

    assert(plan.changePlanStatusEvents(Closed, now, 1.h).map(_.toList) == Right:
      List(
        planId <-: PlanClosed))
    plan = plan.applyEvent(PlanClosed).orThrow

    plan = plan.removeOrders(orderId :: Nil)

    assert(plan.maybePlanFinished(now, 1.h).toList ==
      List(
        planId <-: PlanFinished(now)))

    assert(plan.maybePlanFinished(now, 0.h).toList ==
      List(
        planId <-: PlanFinished(now),
        boardPath <-: NoticeDeleted(noticeId.plannedNoticeKey),
        planId <-: PlanDeleted))
