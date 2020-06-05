package js7.agent.scheduler

import js7.base.circeutils.CirceUtils._
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.{JournalId, KeyedEvent}
import js7.data.master.MasterId
import js7.tester.CirceJsonTester.testJson
import java.util.UUID
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentEventTest extends AnyFreeSpec {

  "JSON" - {
    "MasterRegistered" in {
      testJson[KeyedEvent[AgentEvent]](
        AgentEvent.MasterRegistered(
          MasterId("MASTER"),
          AgentRefPath("/AGENT"),
          AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))),
        json""" {
          "TYPE": "MasterRegistered",
          "agentRefPath": "/AGENT",
          "masterId": "MASTER",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
        }""")
    }
  }
}
