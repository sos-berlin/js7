package js7.controller.data.agent

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class AgentEventsObservedTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(AgentEventsObserved(123L),json"""
      {
        "TYPE":  "AgentEventsObserved",
        "untilEventId": 123
      }""")
  }
}
