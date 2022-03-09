package js7.data.subagent

import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState.inventoryItemJsonCodec
import js7.data.item.{InventoryItem, ItemRevision}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SubagentRefTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[InventoryItem](
      SubagentRef(
        SubagentId("SUBAGENT"),
        AgentPath("AGENT"),
        Uri("https://example.com"),
        disabled = true,
        Some(ItemRevision(1))),
      json"""{
        "TYPE": "SubagentRef",
        "id": "SUBAGENT",
        "agentPath": "AGENT",
        "uri": "https://example.com",
        "disabled": true,
        "itemRevision": 1
      }""")
  }
}
