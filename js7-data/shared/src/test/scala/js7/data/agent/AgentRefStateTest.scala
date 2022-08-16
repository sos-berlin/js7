package js7.data.agent

import java.util.UUID
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.delegate.DelegateCouplingState
import js7.data.event.JournalId
import js7.data.item.ItemRevision
import js7.data.platform.PlatformInfo
import js7.data.subagent.SubagentId
import js7.tester.CirceJsonTester.testJson

final class AgentRefStateTest extends OurTestSuite
{
  "JSON" in {
    testJson(
      AgentRefState(
        AgentRef(
          AgentPath("AGENT"),
          directors = Seq(SubagentId("SUBAGENT-1")),
          itemRevision = Some(ItemRevision(0))),
        None,
        None,
        DelegateCouplingState.Coupled,
        123L,
        None,
        None),
      json"""{
        "agentRef": {
          "path": "AGENT",
          "directors": [ "SUBAGENT-1" ],
          "itemRevision": 0
        },
        "couplingState": {
          "TYPE": "Coupled"
        },
        "eventId": 123
      }""")

    val agentRunId = AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF")))

    testJson(
      AgentRefState(
        AgentRef(
          AgentPath("AGENT"),
          directors = Seq(SubagentId("SUBAGENT-1")),
          itemRevision = Some(ItemRevision(0))),
        Some(agentRunId),
        Some("UTC"),
        DelegateCouplingState.Resetting(force = true),
        123L,
        Some(Problem("PROBLEM")),
        Some(PlatformInfo.test)),
      json"""{
        "agentRef": {
          "path": "AGENT",
          "directors": [ "SUBAGENT-1" ],
          "itemRevision": 0
        },
        "agentRunId": "ABEiM0RVZneImaq7zN3u_w",
        "timezone": "UTC",
        "couplingState": {
          "TYPE": "Resetting",
          "force": true
        },
        "eventId": 123,
        "problem": {
          "message": "PROBLEM"
        },
        "platformInfo": {
          "timestamp": 1657281600000,
          "timezone": "Europe/Berlin",
          "js7Version": "2.4.0-TEST",
          "hostname": "HOST",
          "operatingSystemDistribution": "DISTRIBUTION",
          "cpuModel": "CPU",
          "java": {
            "version": "x.y.z",
            "availableProcessors": 8,
            "memory": {
              "maximum": 3,
              "total": 2,
              "free": 1
            },
            "systemProperties": {
              "test": "TEST"
            }
          }
        }
      }""")
  }
}
