package js7.agent.data.event

import java.util.UUID
import js7.agent.data.event.AgentEvent.{AgentDedicated, AgentReady, AgentShutDown}
import js7.base.circeutils.CirceUtils._
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.{JournalId, KeyedEvent}
import js7.data.platform.PlatformInfo
import js7.data.subagent.SubagentId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

final class AgentEventTest extends AnyFreeSpec
{
  "AgentDedicated" in {
    testJson[KeyedEvent[AgentEvent]](AgentDedicated(
      Some(SubagentId("SUBAGENT")),
      AgentPath("AGENT"),
      AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))),
      ControllerId("CONTROLLER")),
      json"""{
        "TYPE": "AgentDedicated",
        "subagentId": "SUBAGENT",
        "agentPath": "AGENT",
        "agentRunId": "ABEiM0RVZneImaq7zN3u_w",
        "controllerId": "CONTROLLER"
      }""")
  }

  "AgentReady" in {
    testJson[KeyedEvent[AgentEvent]](AgentReady("Europe/Berlin", 1.hour, Some(PlatformInfo.test)),
      json"""{
        "TYPE": "AgentReady",
        "timezone": "Europe/Berlin",
        "totalRunningTime": 3600,
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

  "AgentShutDown" in {
    testJson[KeyedEvent[AgentEvent]](AgentShutDown,
      json"""{
        "TYPE": "AgentShutDown"
      }""")
  }
}
