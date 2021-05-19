package js7.agent.data.event

import js7.base.circeutils.CirceUtils._
import js7.data.event.KeyedEvent
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AgentControllerEventTest extends AnyFreeSpec
{
  "AgentReady" in {
    testJson[KeyedEvent[AgentControllerEvent]](AgentControllerEvent.AgentReady("Europe/Berlin", 1.hour),
      json"""{
        "TYPE": "AgentReady",
        "timezone": "Europe/Berlin",
        "totalRunningTime": 3600
      }""")
  }
}
