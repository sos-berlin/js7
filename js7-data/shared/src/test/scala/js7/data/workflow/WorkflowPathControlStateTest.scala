package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.agent.AgentPath
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class WorkflowPathControlStateTest extends AnyFreeSpec
{
  private implicit val typedJsonCodec = TypedJsonCodec[Any](
    WorkflowPathControlState.subtype)

  "JSON" in {
    testJson[Any](
      WorkflowPathControlState(
        WorkflowPathControl(WorkflowPath("WORKFLOW")),
        Set(AgentPath("AGENT"))),
      json"""{
        "TYPE": "WorkflowPathControlState",
        "workflowPathControl": {
          "path": "WORKFLOW",
          "suspended": false,
          "skip": [],
          "revision": 0
        },
        "attachedToAgents": [ "AGENT" ]
      }""")
  }

  "JSON 2.4.0-beta.20220621" in {
    assert(
      json"""{
        "TYPE": "WorkflowControlState",
        "workflowControl": {
          "path": "WORKFLOW",
          "suspended": false,
          "revision": 0
        },
        "attachedToAgents": [ "AGENT" ]
      }""".as[Any] == Right(
        WorkflowPathControlState(
          WorkflowPathControl(WorkflowPath("WORKFLOW")),
          Set(AgentPath("AGENT")))))
  }
}
