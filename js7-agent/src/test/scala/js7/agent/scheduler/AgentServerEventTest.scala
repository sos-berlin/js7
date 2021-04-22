package js7.agent.scheduler

import java.util.UUID
import js7.base.circeutils.CirceUtils._
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.{JournalId, KeyedEvent}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentServerEventTest extends AnyFreeSpec {

  "JSON" - {
    "ControllerRegistered" in {
      testJson[KeyedEvent[AgentServerEvent]](
        AgentServerEvent.ControllerRegistered(
          ControllerId("CONTROLLER"),
          AgentPath("AGENT"),
          AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))),
        json"""{
          "TYPE": "ControllerRegistered",
          "agentPath": "AGENT",
          "controllerId": "CONTROLLER",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
        }""")
    }
  }
}
