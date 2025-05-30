package js7.data.subagent

import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.TypedJsonCodec
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.controller.ControllerState.{inventoryItemJsonCodec, inventoryItemKeyJsonCodec}
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.{BasicItemEvent, InventoryItem, InventoryItemKey}
import js7.data.value.expression.Expression.NumericConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class SubagentBundleTest extends OurTestSuite:

  "JSON" - {
    "SubagentBundle" in:
      testJson[InventoryItem](
        SubagentBundle(
          SubagentBundleId("BUNDLE"),
          Map(
            SubagentId("A-SUBAGENT") -> NumericConstant(1),
            SubagentId("B-SUBAGENT") -> expr("min(100, (1 / $cpuLoad) ? 100)"))),
        json"""{
          "TYPE": "SubagentBundle",
          "id": "BUNDLE",
          "subagentToPriority": {
            "A-SUBAGENT": 1,
            "B-SUBAGENT": "min(100, (1 / $$cpuLoad) ? 100)"
          }
        }""")

    "InventoryItemKey" in:
      testJson[InventoryItemKey](
        SubagentBundleId("BUNDLE"),
        json""" "SubagentBundle:BUNDLE" """)

    "ItemAttached" in:
      given TypedJsonCodec[BasicItemEvent] = BasicItemEvent.jsonCodec(using ControllerState)

      testJson[BasicItemEvent](
        ItemAttached(SubagentBundleId("BUNDLE"), None, AgentPath("AGENT")),
        json"""{
          "TYPE": "ItemAttached",
          "key": "SubagentBundle:BUNDLE",
          "delegateId": "Agent:AGENT"
        }""")
  }

  "JSON until v2.7.1" - {
    "InventoryItem" in:
      // COMPATIBLE with v2.7.1
      testJsonDecoder[InventoryItem](
        SubagentBundle(
          SubagentBundleId("BUNDLE"),
          Map(
            SubagentId("A-SUBAGENT") -> NumericConstant(1),
            SubagentId("B-SUBAGENT") -> expr("min(100, (1 / $cpuLoad) ? 100)"))),
        json"""{
          "TYPE": "SubagentSelection",
          "id": "BUNDLE",
          "subagentToPriority": {
            "A-SUBAGENT": 1,
            "B-SUBAGENT": "min(100, (1 / $$cpuLoad) ? 100)"
          }
        }""")

    "InventoryItemKey" in:
      testJsonDecoder[InventoryItemKey](
        SubagentBundleId("BUNDLE"),
        json""" "SubagentSelection:BUNDLE" """)

    "ItemAttached" in:
      given TypedJsonCodec[BasicItemEvent] = BasicItemEvent.jsonCodec(using ControllerState)

      testJsonDecoder[BasicItemEvent](
        ItemAttached(SubagentBundleId("BUNDLE"), None, AgentPath("AGENT")),
        json"""{
          "TYPE": "ItemAttached",
          "key": "SubagentSelection:BUNDLE",
          "delegateId": "Agent:AGENT"
        }""")
  }
