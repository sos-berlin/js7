package js7.data.orderwatch

import java.util.regex.Pattern
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.agent.AgentId
import js7.data.workflow.WorkflowPath
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class FileWatchTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[OrderWatch](
      FileWatch(
        OrderWatchId("ID"), WorkflowPath("WORKFLOW"), AgentId("AGENT"), "/DIRECTORY",
        Some(Pattern.compile("[a-z]+.csv"))
      ),
      json"""{
        "TYPE": "FileWatch",
        "id": "ID",
        "workflowPath": "WORKFLOW",
        "agentId": "AGENT",
        "directory": "/DIRECTORY",
        "pattern": "[a-z]+.csv",
        "itemRevision": 0
      }""")
  }
}
