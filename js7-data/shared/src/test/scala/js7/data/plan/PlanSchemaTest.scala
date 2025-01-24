package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.controller.ControllerId
import js7.data.item.{InventoryItem, ItemRevision}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.value.{MissingValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester
import js7.tester.CirceJsonTester.testJson

final class PlanSchemaTest extends OurTestSuite:

  "JSON" in:
    import js7.data.controller.ControllerState.inventoryItemJsonCodec

    testJson[InventoryItem](
      PlanSchema(
        PlanSchemaId("DailyPlan"),
        planKeyExpr = expr("match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1') ?"),
        planIsClosedFunction = Some(exprFunction("(testPlanKey) => false")),
        Some(ItemRevision(1))),
      json"""{
        "TYPE": "PlanSchema",
        "id": "DailyPlan",
        "planKeyExpr": "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1')?",
        "planIsClosedFunction": "(testPlanKey) => false",
        "itemRevision": 1
      }""")

    testJson[InventoryItem](
      PlanSchema(
        PlanSchemaId("DailyPlan"),
        planKeyExpr = expr("match(orderId, '#(.+)#.*', '$1') ?")),
      json"""{
        "TYPE": "PlanSchema",
        "id": "DailyPlan",
        "planKeyExpr": "match(orderId, '#(.+)#.*', '$$1')?"
      }""")

  "planKeyExpr" in:
    val freshOrder = FreshOrder(OrderId("#2024-11-20#bla"), WorkflowPath("WORKFLOW"))
    val scope = freshOrder.minimumScope(ControllerId("CONTROLLER"))

    val dailyPlanSchema = PlanSchema.joc(PlanSchemaId("DailyPlan"))
    assert(dailyPlanSchema.planKeyExpr.eval(scope) == Right(StringValue("2024-11-20")))

    val weeklyPlanSchema = PlanSchema(
      PlanSchemaId("WeeklyPlan"),
      expr("match(orderId, '^#([0-9]{4}w[0-9]{2})#.*$', '$1') ?"))
    assert(weeklyPlanSchema.planKeyExpr.eval(scope) == Right(MissingValue))
