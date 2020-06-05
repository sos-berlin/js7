package js7.agent.scheduler

import js7.base.circeutils.CirceUtils._
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.event.JournalId
import js7.data.master.MasterId
import js7.tester.CirceJsonTester.testJson
import java.util.UUID
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RegisteredMasterTest extends AnyFreeSpec
{
  "JSON" in {
    implicit val codec = AgentServerJsonCodecs.jsonCodec
    testJson[Any](
      RegisteredMaster(
        MasterId("MASTER"),
        AgentRefPath("/AGENT"),
        AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))),
      json""" {
        "TYPE": "RegisteredMaster",
        "masterId": "MASTER",
        "agentRefPath": "/AGENT",
        "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
      }""")
  }
}
