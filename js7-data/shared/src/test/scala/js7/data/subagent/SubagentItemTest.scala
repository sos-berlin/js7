package js7.data.subagent

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState.inventoryItemJsonCodec
import js7.data.item.{InventoryItem, ItemRevision}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class SubagentItemTest extends OurTestSuite
{
  "JSON" in {
    testJson[InventoryItem](
      SubagentItem(
        SubagentId("SUBAGENT"),
        AgentPath("AGENT"),
        Uri("https://example.com"),
        Some(Uri("https://backup.example.com")),
        disabled = true,
        Some(ItemRevision(1))),
      json"""{
        "TYPE": "SubagentItem",
        "id": "SUBAGENT",
        "agentPath": "AGENT",
        "uri": "https://example.com",
        "backupUri": "https://backup.example.com",
        "disabled": true,
        "itemRevision": 1
      }""")

    // COMPATIBLE with v2.2.2
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
  }
}
