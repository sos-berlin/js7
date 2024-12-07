package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.controller.ControllerId
import js7.data.item.{InventoryItem, ItemRevision}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.value.expression.scopes.OrderScopes
import js7.data.value.{MissingValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester
import js7.tester.CirceJsonTester.testJson

final class PlanTemplateTest extends OurTestSuite:

  "JSON" in:
    import js7.data.controller.ControllerState.inventoryItemJsonCodec

    testJson[InventoryItem](
      PlanTemplate(
        PlanTemplateId("DailyPlan"),
        orderToPlanKey = expr("match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1') ?"),
        planIsOpenFunction = Some(exprFunction("(testPlanKey) => true")),
        Some(ItemRevision(1))),
      json"""{
        "TYPE": "PlanTemplate",
        "id": "DailyPlan",
        "orderToPlanKey": "match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1')?",
        "planIsOpenFunction": "(testPlanKey) => true",
        "itemRevision": 1
      }""")

    testJson[InventoryItem](
      PlanTemplate(
        PlanTemplateId("DailyPlan"),
        orderToPlanKey = expr("match(orderId, '#(.+)#.*', '$1') ?")),
      json"""{
        "TYPE": "PlanTemplate",
        "id": "DailyPlan",
        "orderToPlanKey": "match(orderId, '#(.+)#.*', '$$1')?"
      }""")

  "orderToPlanKey" in:
    val freshOrder = FreshOrder(OrderId("#2024-11-20#bla"), WorkflowPath("WORKFLOW"))
    val scope = OrderScopes.minimumOrderScope(freshOrder, ControllerId("CONTROLLER"))

    val dailyPlanTemplate = PlanTemplate.joc(PlanTemplateId("DailyPlan"))
    assert(dailyPlanTemplate.orderToPlanKey.eval(scope) == Right(StringValue("2024-11-20")))

    val weeklyPlanTemplate = PlanTemplate(
      PlanTemplateId("WeeklyPlan"),
      expr("match(orderId, '^#([0-9]{4}w[0-9]{2})#.*$', '$1') ?"))
    assert(weeklyPlanTemplate.orderToPlanKey.eval(scope) == Right(MissingValue))
