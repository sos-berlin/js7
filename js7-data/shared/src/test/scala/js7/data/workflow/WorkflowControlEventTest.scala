package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.agent.AgentPath
import js7.data.item.ItemRevision
import js7.data.workflow.WorkflowControlEvent.{WorkflowControlAttached, WorkflowControlUpdated}
import js7.tester.CirceJsonTester
import org.scalatest.freespec.AnyFreeSpec

final class WorkflowControlEventTest extends AnyFreeSpec
{
  "WorkflowControlUpdated" in {
    CirceJsonTester.testJson[WorkflowControlEvent](
      WorkflowControlUpdated(suspended = true, ItemRevision(123)),
      json"""{
        "TYPE": "WorkflowControlUpdated",
        "suspended": true,
        "revision": 123
       }""")
  }

  "WorkflowControlAttached" in {
    CirceJsonTester.testJson[WorkflowControlEvent](
      WorkflowControlAttached(AgentPath("AGENT"), true, ItemRevision(123)),
      json"""{
        "TYPE": "WorkflowControlAttached",
        "agentPath": "AGENT",
        "suspended": true,
        "revision": 123
       }""")
  }
}
