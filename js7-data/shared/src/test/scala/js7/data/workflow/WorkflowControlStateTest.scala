package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.agent.AgentPath
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class WorkflowControlStateTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      WorkflowControlState(
        WorkflowControl(WorkflowPath("WORKFLOW")),
        Set(AgentPath("AGENT"))),
      json"""{
        "workflowControl": {
          "path": "WORKFLOW",
          "suspended": false,
          "revision": 0
        },
        "attachedToAgents": [ "AGENT" ]
      }""")
  }
}
