package js7.data.subagent

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.utils.Base64UUID
import js7.data.event.KeyedEvent
import js7.data.platform.PlatformInfo
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SubagentItemStateEventTest extends AnyFreeSpec
{
  "JSON" - {
    "SubagentCoupled" in {
      testJson[KeyedEvent[SubagentItemStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentItemStateEvent.SubagentCoupled,
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentCoupled"
        }""")
    }

    "SubagentCouplingFailed" in {
      testJson[KeyedEvent[SubagentItemStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentItemStateEvent.SubagentCouplingFailed(Problem("PROBLEM")),
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentCouplingFailed",
          "problem": {
            "message": "PROBLEM"
          }
        }""")
    }

    "SubagentDedicated" in {
      testJson[KeyedEvent[SubagentItemStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentItemStateEvent.SubagentDedicated(
          SubagentRunId(Base64UUID.zero),
          Some(PlatformInfo.test)),
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentDedicated",
          "subagentRunId": "AAAAAAAAAAAAAAAAAAAAAA",
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

    "SubagentEventsObserved" in {
      testJson[KeyedEvent[SubagentItemStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentItemStateEvent.SubagentEventsObserved(1001L),
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentEventsObserved",
          "untilEventId": 1001
        }""")
    }

    "SubagentRestarted" in {
      testJson[KeyedEvent[SubagentItemStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentItemStateEvent.SubagentRestarted,
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentRestarted"
        }""")
    }

    "SubagentShutdown" in {
      testJson[KeyedEvent[SubagentItemStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentItemStateEvent.SubagentShutdown,
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentShutdown"
        }""")
    }

    "SubagentResetStarted" in {
      testJson[KeyedEvent[SubagentItemStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentItemStateEvent.SubagentResetStarted(force = false),
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentResetStarted",
          "force": false
        }""")
    }

    "SubagentReset" in {
      testJson[KeyedEvent[SubagentItemStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentItemStateEvent.SubagentReset,
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentReset"
        }""")
    }
  }
}
