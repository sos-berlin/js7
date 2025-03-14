package js7.data.plan

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{CompactPrinter, JsonStringInterpolator}
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.{BoardPath, PlannedBoard}
import js7.data.controller.ControllerState
import js7.data.item.ItemRevision
import js7.data.order.OrderId
import js7.data.value.StringValue
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.exprFun

final class PlanSchemaStateTest extends OurTestSuite:

  "toSnapshotStream JSON" in:
    val planSchema = PlanSchema(
      PlanSchemaId("DailyPlan"),
      Some(exprFun"day => $$day < $$openingDay"),
      namedValues = Map(
        "openingDay" -> StringValue("")),
      Some(ItemRevision(1)))

    val planSchemaState = PlanSchemaState(
      planSchema,
      finishedPlanRetentionPeriod = 3600.s,
      namedValues = Map(
        "openingDay" -> StringValue("2025-02-20")),
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
        "TYPE" : "PlanSchema",
        "id" : "DailyPlan",
        "unknownPlanIsClosedFunction" : "day => $$day < $$openingDay",
        "namedValues" : {
          "openingDay" : ""
        },
        "itemRevision" : 1
      }""",
      json"""{
        "TYPE" : "PlanSchemaState",
        "id" : "DailyPlan",
        "finishedPlanRetentionPeriod": 3600,
        "namedValues" : {
          "openingDay" : "2025-02-20"
        }
      }""",
      json"""{
        "TYPE" : "Plan",
        "planId" : [ "DailyPlan", "2025-02-20" ],
        "status" : "Closed"
      }"""))
