package js7.data.item

import io.circe.Json
import js7.data.agent.AgentPath
import js7.data.board.BoardPath
import js7.data.calendar.CalendarPath
import js7.data.lock.LockPath
import js7.data.orderwatch.OrderWatchPath
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class InventoryItemPathTest extends AnyFreeSpec
{
  "JSON" in {
    implicit val x = InventoryItemPath.jsonCodec[InventoryItemPath](
      Seq(AgentPath, BoardPath, CalendarPath, LockPath, WorkflowPath, OrderWatchPath))
    testJson[InventoryItemPath](AgentPath("AGENT"), Json.fromString("Agent:AGENT"))
    testJson[InventoryItemPath](BoardPath("BOARD"), Json.fromString("Board:BOARD"))
    testJson[InventoryItemPath](CalendarPath("CALENDAR"), Json.fromString("Calendar:CALENDAR"))
    testJson[InventoryItemPath](LockPath("LOCK"), Json.fromString("Lock:LOCK"))
    testJson[InventoryItemPath](WorkflowPath("WORKFLOW"), Json.fromString("Workflow:WORKFLOW"))

    // Or FileWatch?
    testJson[InventoryItemPath](OrderWatchPath("ORDERWATCH"), Json.fromString("OrderWatch:ORDERWATCH"))
  }
}
