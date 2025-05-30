package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.item.{InventoryItem, ItemRevision}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.exprFun
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class PlanSchemaTest extends OurTestSuite:

  "JSON" in:
    import js7.data.controller.ControllerState.inventoryItemJsonCodec

    testJson[InventoryItem](
      PlanSchema(
        PlanSchemaId("DailyPlan"),
        unknownPlanIsOpenFunction = exprFun"planKey => false",
        Map("NAME" -> StringValue("VALUE")),
        Some(ItemRevision(1))),
      json"""{
        "TYPE": "PlanSchema",
        "id": "DailyPlan",
        "unknownPlanIsOpenFunction": "planKey => false",
        "namedValues": {
          "NAME": "VALUE"
        },
        "itemRevision": 1
      }""")

    testJson[InventoryItem](
      PlanSchema(PlanSchemaId("DailyPlan"), PlanSchema.EachUnknownPlanIsClosed),
      json"""{
        "TYPE": "PlanSchema",
        "id": "DailyPlan",
        "unknownPlanIsOpenFunction": "planKey => false",
        "namedValues": {}
      }""")

    // COMPATIBLE with v2.7.4-SNAPSHOT
    testJsonDecoder[InventoryItem](
      PlanSchema(
        PlanSchemaId("DailyPlan"),
        unknownPlanIsOpenFunction = exprFun"day => !($$day < $$unknownPlansAreOpenFrom)"),
      json"""{
        "TYPE": "PlanSchema",
        "id": "DailyPlan",
        "unknownPlanIsClosedFunction": "day => $$day < $$unknownPlansAreOpenFrom"
      }""")
