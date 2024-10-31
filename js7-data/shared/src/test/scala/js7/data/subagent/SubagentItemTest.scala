package js7.data.subagent

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState.inventoryItemJsonCodec
import js7.data.item.{InventoryItem, ItemRevision}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class SubagentItemTest extends OurTestSuite:

  "JSON" in:
    testJson[InventoryItem](
      SubagentItem(
        SubagentId("SUBAGENT"),
        AgentPath("AGENT"),
        Uri("https://example.com"),
        disabled = true,
        Some(ItemRevision(1))),
      json"""{
        "TYPE": "SubagentItem",
        "id": "SUBAGENT",
        "agentPath": "AGENT",
        "uri": "https://example.com",
        "disabled": true,
        "itemRevision": 1
      }""")

  "JSON until v2.2.2" in: // COMPATIBLE
    testJsonDecoder[InventoryItem](
      SubagentItem(
        SubagentId("SUBAGENT"),
        AgentPath("AGENT"),
        Uri("https://example.com"),
        itemRevision = Some(ItemRevision(1))),
      json"""{
        "TYPE": "SubagentItem",
        "id": "SUBAGENT",
        "agentPath": "AGENT",
        "uri": "https://example.com",
        "itemRevision": 1
      }""")

  "JSON, a long time ago" in: // COMPATIBLE
    testJsonDecoder[InventoryItem](
      SubagentItem(
        SubagentId("SUBAGENT"),
        AgentPath("AGENT"),
        Uri("https://example.com")),
      json"""{
        "TYPE": "SubagentRef",
        "id": "SUBAGENT",
        "agentPath": "AGENT",
        "uri": "https://example.com"
      }""")
