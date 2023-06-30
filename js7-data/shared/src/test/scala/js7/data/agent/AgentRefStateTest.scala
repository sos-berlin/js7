package js7.data.agent

import java.util.UUID
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.web.Uri
import js7.data.cluster.ClusterEvent.ClusterPassiveLost
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterTiming}
import js7.data.delegate.DelegateCouplingState
import js7.data.event.JournalId
import js7.data.item.ItemRevision
import js7.data.node.NodeId
import js7.data.platform.PlatformInfo
import js7.data.subagent.SubagentId
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class AgentRefStateTest extends OurTestSuite
{
  "JSON minimum" in {
    val agentRefState = AgentRefState(
      AgentRef(
        AgentPath("AGENT"),
        directors = Seq(SubagentId("SUBAGENT-1")),
        itemRevision = Some(ItemRevision(0))),
      None,
      None,
      DelegateCouplingState.Coupled,
      123L,
      None,
      ClusterState.Empty,
      Map.empty,
      None)

    testJson(agentRefState,
      json"""{
        "agentRef": {
          "path": "AGENT",
          "directors": [ "SUBAGENT-1" ],
          "itemRevision": 0
        },
        "couplingState": {
          "TYPE": "Coupled"
        },
        "eventId": 123,
        "clusterState": {
          "TYPE": "Empty"
        },
        "nodeToClusterWatchConfirmationRequired": {}
      }""")

    testJsonDecoder(agentRefState,
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
  }

  "JSON full" in {
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
        ClusterState.Coupled(ClusterSetting(
          Map(
            NodeId("Primary") -> Uri("https://PRIMARY"),
            NodeId("Backup") -> Uri("https://BACKUP")),
          NodeId("Primary"),
          ClusterTiming(3.s, 10.s),
          clusterWatchId = None)),
        Map(
          NodeId("Primary") -> ClusterPassiveLost(NodeId("Backup"))),
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
        "clusterState": {
          "TYPE": "Coupled",
          "setting": {
            "idToUri": {
              "Primary": "https://PRIMARY",
              "Backup": "https://BACKUP"
            },
            "activeId": "Primary",
            "timing": {
              "heartbeat": 3,
              "heartbeatTimeout": 10
            }
          }
        },
        "nodeToClusterWatchConfirmationRequired": {
          "Primary": {
            "TYPE": "ClusterPassiveLost",
            "id": "Backup"
          }
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
