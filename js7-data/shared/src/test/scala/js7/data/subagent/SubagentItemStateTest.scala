package js7.data.subagent

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.Test
import js7.base.utils.Base64UUID
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.delegate.DelegateCouplingState
import js7.data.item.ItemRevision
import js7.data.platform.PlatformInfo
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class SubagentItemStateTest extends Test
{
  "JSON" in {
    testJson[SubagentItemState](
      SubagentItemState(
        SubagentItem(
          SubagentId("SUBAGENT"),
          AgentPath("AGENT"),
          Uri("https://example.com"),
          itemRevision = Some(ItemRevision(1))),
        Some(SubagentRunId(Base64UUID.zero)),
        DelegateCouplingState.Coupled,
        isDetaching = false,
        isResettingForcibly = None,
        eventId = 1001L,
        Some(Problem("PROBLEM")),
        Some(PlatformInfo.test)),
      json"""{
        "subagentItem": {
          "agentPath": "AGENT",
          "disabled": false,
          "id": "SUBAGENT",
          "itemRevision": 1,
          "uri": "https://example.com"
        },
        "subagentRunId": "AAAAAAAAAAAAAAAAAAAAAA",
        "couplingState": {
          "TYPE": "Coupled"
        },
        "eventId": 1001,
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

    testJson[SubagentItemState](
      SubagentItemState(
        SubagentItem(
          SubagentId("SUBAGENT"),
          AgentPath("AGENT"),
          Uri("https://example.com"),
          itemRevision = Some(ItemRevision(1))),
        None,
        DelegateCouplingState.Coupled,
        /*Agent only*/isDetaching = true,
        /*Controller only*/isResettingForcibly = Some(false),
        1001L,
        None,
        None),
      json"""{
        "subagentItem": {
          "agentPath": "AGENT",
          "disabled": false,
          "id": "SUBAGENT",
          "itemRevision": 1,
          "uri": "https://example.com"
        },
        "couplingState": {
          "TYPE": "Coupled"
        },
        "isDetaching": true,
        "isResettingForcibly": false,
        "eventId": 1001
      }""")

    // COMPATIBLE with v2.2.2
    testJsonDecoder[SubagentItemState](
      SubagentItemState(
        SubagentItem(
          SubagentId("SUBAGENT"),
          AgentPath("AGENT"),
          Uri("https://example.com"),
          itemRevision = Some(ItemRevision(1))),
        Some(SubagentRunId(Base64UUID.zero)),
        DelegateCouplingState.Coupled,
        eventId = 1001L,
        problem = Some(Problem("PROBLEM")),
        platformInfo = None),
      json"""{
        "subagentRef": {
          "agentPath": "AGENT",
          "id": "SUBAGENT",
          "itemRevision": 1,
          "uri": "https://example.com"
        },
        "subagentRunId": "AAAAAAAAAAAAAAAAAAAAAA",
        "couplingState": {
          "TYPE": "Coupled"
        },
        "eventId": 1001,
        "problem": {
          "message": "PROBLEM"
        }
      }""")
  }
}
