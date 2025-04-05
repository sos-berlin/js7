package js7.data.plan

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{CompactPrinter, JsonStringInterpolator}
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.{BoardPath, PlannedBoard}
import js7.data.controller.ControllerState
import js7.data.item.ItemRevision
import js7.data.order.OrderId
import js7.data.plan.PlanEvent.PlanOpened
import js7.data.plan.PlanSchemaEvent.PlanSchemaChanged
import js7.data.plan.PlanStatus.Open
import js7.data.value.StringValue
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.exprFun

final class PlanSchemaStateTest extends OurTestSuite:

  "toSnapshotStream JSON" in:
    val planSchema = PlanSchema(
      PlanSchemaId("DailyPlan"),
      exprFun"day => $$day >= $$unknownPlansAreOpenFrom",
      namedValues = Map(
        "unknownPlansAreOpenFrom" -> StringValue("")),
      Some(ItemRevision(1)))

    val planSchemaState = PlanSchemaState(
      planSchema,
      finishedPlanRetentionPeriod = 3600.s,
      namedValues = Map(
        "unknownPlansAreOpenFrom" -> StringValue("2025-02-20")),
      toPlan = Map(
        PlanKey("2025-02-20") -> Plan(
          PlanSchemaId("DailyPlan") / "2025-02-20",
          PlanStatus.Closed,
          Set(OrderId("ORDER")),
          Map(
            BoardPath("BOARD") -> PlannedBoard(
              PlanSchemaId("DailyPlan") / "2025-02-20" / BoardPath("BOARD"),
              toNoticePlace = Map.empty)))))

    val snapshots = planSchemaState.toSnapshotStream
      .map(_
        .asJson(ControllerState.snapshotObjectJsonCodec)
        .printWith(CompactPrinter))
      .map(s => io.circe.parser.parse(s).orThrow)
      .compile.toVector

    assert(snapshots == List(
      json"""{
        "TYPE": "PlanSchema",
        "id": "DailyPlan",
        "unknownPlanIsOpenFunction": "day => $$day >= $$unknownPlansAreOpenFrom",
        "namedValues": {
          "unknownPlansAreOpenFrom": ""
        },
        "itemRevision": 1
      }""",
      json"""{
        "TYPE": "PlanSchemaState",
        "id": "DailyPlan",
        "finishedPlanRetentionPeriod": 3600,
        "namedValues": {
          "unknownPlansAreOpenFrom": "2025-02-20"
        }
      }""",
      json"""{
        "TYPE": "Plan",
        "planId": [ "DailyPlan", "2025-02-20" ],
        "status": "Closed"
      }"""))

  "isDiscardable" in:
    var planSchemaState = PlanSchema(
      PlanSchemaId("DailyPlan"),
      exprFun"day => $$day >= $$unknownPlansAreOpenFrom",
      namedValues = Map(
        "unknownPlansAreOpenFrom" -> StringValue("")),
      Some(ItemRevision(1))
    ).toInitialItemState

    planSchemaState =
      planSchemaState.applyEvent:
        PlanSchemaChanged(
          namedValues = Some(Map("unknownPlansAreOpenFrom" -> StringValue("2025-03-20"))))
      .orThrow

    planSchemaState = planSchemaState.applyPlanEvent(PlanKey("2025-03-19"), PlanOpened)
      .orThrow

    assert(planSchemaState.applyPlanEvent(PlanKey("2025-03-20"), PlanOpened) == Left:
      Problem("Plan:DailyPlan╱2025-03-20 is already Open"))

    assert(planSchemaState.applyPlanEvent(PlanKey("2025-03-21"), PlanOpened) == Left:
      Problem("Plan:DailyPlan╱2025-03-21 is already Open"))

    assert(!planSchemaState.isDiscardable(planSchemaState.plan(PlanKey("2025-03-19")).orThrow).orThrow)
    assert(planSchemaState.isDiscardable(planSchemaState.plan(PlanKey("2025-03-20")).orThrow).orThrow)
    assert(planSchemaState.isDiscardable(planSchemaState.plan(PlanKey("2025-03-21")).orThrow).orThrow)

  "Change PlanStatus" - {
    lazy val planSchemaId = PlanSchemaId("DailyPlan")
    lazy val planSchema = PlanSchema(
      planSchemaId,
      exprFun"day => $$day >= $$unknownPlansAreOpenFrom",
      namedValues = Map(
        "unknownPlansAreOpenFrom" -> StringValue("")))

    lazy val planSchemaState0 = planSchema.toInitialItemState.applyEvent:
      PlanSchemaChanged(
        finishedPlanRetentionPeriod = Some(0.s),
        namedValues = Some(Map(
          "unknownPlansAreOpenFrom" -> StringValue("2025-03-20"))))
    .orThrow

    "Open (reopen) a Plan before unknownPlansAreOpenFrom" in:
      // Open (reopen) a Plan before unknownPlansAreOpenFrom //
      val planSchemaState = planSchemaState0.applyPlanEvent(PlanKey("2025-03-01"), PlanOpened).orThrow

      assert(planSchemaState.toPlan == Map(
        PlanKey("2025-03-01") -> Plan(planSchemaId / PlanKey("2025-03-01"), Open)))
      assert(planSchemaState.toSnapshotStream.compile.toList == List(
        planSchema,
        PlanSchemaState.Snapshot(planSchemaId, 0.s, Map("unknownPlansAreOpenFrom" -> StringValue("2025-03-20"))),
        Plan.Snapshot(planSchemaId / "2025-03-01", Open)))

    "Open (reopen) a Plan after unknownPlansAreOpenFrom" in:
      assert(planSchemaState0.applyPlanEvent(PlanKey("2025-03-31"), PlanOpened) == Left:
        Problem("Plan:DailyPlan╱2025-03-31 is already Open"))
  }
