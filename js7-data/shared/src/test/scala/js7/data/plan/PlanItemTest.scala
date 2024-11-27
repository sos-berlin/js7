package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.controller.ControllerId
import js7.data.item.{InventoryItem, ItemRevision}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.scopes.OrderScopes
import js7.data.value.{MissingValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester

final class PlanItemTest extends OurTestSuite:

  "JSON" in:
    import js7.data.controller.ControllerState.inventoryItemJsonCodec
    CirceJsonTester.testJson[InventoryItem](
      PlanItem(
        PlanItemId("DailyPlan"),
        expr("match($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1') ?"),
        Some(ItemRevision(1))),
      json"""{
        "TYPE": "PlanItem",
        "id": "DailyPlan",
        "itemRevision": 1,
        "orderToPlanKey": "match($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1')?"
      }""")

  "orderToPlanKey" in:
    val freshOrder = FreshOrder(OrderId("#2024-11-20#bla"), WorkflowPath("WORKFLOW"))
    val scope = OrderScopes.minimumOrderScope(freshOrder, ControllerId("CONTROLLER"))

    val dailyPlanItem = PlanItem.joc(PlanItemId("DailyPlan"))
    assert(dailyPlanItem.orderToPlanKey.eval(scope) == Right(StringValue("2024-11-20")))

    val weeklyPlanItem = PlanItem(
      PlanItemId("WeeklyPlan"),
      expr("match($js7OrderId, '^#([0-9]{4}w[0-9]{2})#.*$', '$1') ?"))
    assert(weeklyPlanItem.orderToPlanKey.eval(scope) == Right(MissingValue))