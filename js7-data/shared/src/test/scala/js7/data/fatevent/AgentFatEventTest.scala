package js7.data.fatevent

import js7.base.circeutils.CirceUtils._
import js7.data.agent.AgentRefPath
import js7.data.event.KeyedEvent
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentFatEventTest extends AnyFreeSpec
{
  "ControllerReadyFat" in {
    testJson[KeyedEvent[AgentFatEvent]](AgentRefPath("/AGENT") <-: AgentFatEvent.AgentReadyFat("Europe/Berlin"),
      json"""{
        "TYPE": "AgentReadyFat",
        "key": "/AGENT",
        "timezone": "Europe/Berlin"
      }""")
  }
}
