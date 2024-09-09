package js7.data.subagent

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.data.controller.ControllerState.inventoryItemJsonCodec
import js7.data.item.InventoryItem
import js7.data.value.expression.Expression.NumericConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.tester.CirceJsonTester.testJson

final class SubagentSelectionTest extends OurTestSuite:

  "JSON" in:
    testJson[InventoryItem](
      SubagentSelection(
        SubagentSelectionId("SELECTION"),
        Map(
          SubagentId("A-SUBAGENT") -> NumericConstant(1),
          SubagentId("B-SUBAGENT") -> expr("min(100, (1 / $cpuLoad) ? 100)"))),
      json"""{
        "TYPE": "SubagentSelection",
        "id": "SELECTION",
        "subagentToPriority": {
          "A-SUBAGENT": 1,
          "B-SUBAGENT": "min(100, (1 / $$cpuLoad) ? 100)"
        }
      }""")
