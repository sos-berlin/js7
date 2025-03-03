package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.item.{InventoryItem, ItemRevision}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.exprFunction
import js7.tester.CirceJsonTester
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class PlanSchemaTest extends OurTestSuite:

  "JSON" in:
    import js7.data.controller.ControllerState.inventoryItemJsonCodec

    testJson[InventoryItem](
      PlanSchema(
        PlanSchemaId("DailyPlan"),
        planIsClosedFunction = Some(exprFunction("planKey => false")),
        Map("NAME" -> StringValue("VALUE")),
        Some(ItemRevision(1))),
      json"""{
        "TYPE": "PlanSchema",
        "id": "DailyPlan",
        "planIsClosedFunction": "planKey => false",
        "namedValues": {
          "NAME": "VALUE"
        },
        "itemRevision": 1
      }""")

    testJson[InventoryItem](
      PlanSchema(PlanSchemaId("DailyPlan")),
      json"""{
        "TYPE": "PlanSchema",
        "id": "DailyPlan",
        "namedValues": {}
      }""")

    // COMPATIBLE with v2.7.4-SNAPSHOT
    testJsonDecoder[InventoryItem](
      PlanSchema(PlanSchemaId("DailyPlan")),
      json"""{
        "TYPE": "PlanSchema",
        "id": "DailyPlan"
      }""")
