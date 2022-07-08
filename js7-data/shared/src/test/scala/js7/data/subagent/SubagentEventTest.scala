package js7.data.subagent

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.subagent.SubagentEvent.{SubagentItemAttached, SubagentShutdown}
import js7.data.workflow.Workflow
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SubagentEventTest extends AnyFreeSpec
{
  "SubagentItemAttached" in {
    testJson[SubagentEvent](
      SubagentItemAttached(Workflow.empty),
      json"""{
        "TYPE": "SubagentItemAttached",
        "item": {
          "TYPE": "Workflow",
          "instructions": []
        }
      }""")
  }

  "SubagentShutdown" in {
    testJson[SubagentEvent](
      SubagentShutdown,
      json"""{
        "TYPE": "SubagentShutdown"
      }""")
  }
}
