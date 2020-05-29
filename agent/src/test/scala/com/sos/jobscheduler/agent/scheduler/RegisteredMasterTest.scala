package com.sos.jobscheduler.agent.scheduler

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.agent.{AgentRefPath, AgentRunId}
import com.sos.jobscheduler.data.event.JournalId
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
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
