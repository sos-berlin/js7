package js7.data.subagent

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.subagent.SubagentEvent.{SubagentItemAttached, SubagentShutdown}
import js7.data.workflow.Workflow
import js7.tester.CirceJsonTester.testJson

final class SubagentEventTest extends OurTestSuite:
  "SubagentItemAttached" in:
    testJson[SubagentEvent](
      SubagentItemAttached(Workflow.empty),
      json"""{
        "TYPE": "SubagentItemAttached",
        "item": {
          "TYPE": "Workflow",
          "instructions": []
        }
      }""")

  "SubagentShutdown" in:
    testJson[SubagentEvent](
      SubagentShutdown,
      json"""{
        "TYPE": "SubagentShutdown"
      }""")
