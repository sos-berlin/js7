package js7.data.agent

import java.util.UUID
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.agent.AgentRefStateEvent.{AgentClusterWatchConfirmationRequired, AgentCoupled, AgentCouplingFailed, AgentDedicated, AgentEventsObserved, AgentMirroredEvent, AgentReady, AgentResetStarted}
import js7.data.cluster.ClusterEvent
import js7.data.cluster.ClusterWatchProblems.ClusterNodeLossNotConfirmedProblem
import js7.data.event.{JournalId, KeyedEvent}
import js7.data.node.NodeId
import js7.data.platform.PlatformInfo
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class AgentRefStateEventTest extends OurTestSuite:

  "JSON" - {
    "AgentCoupled" in:
      testJson[KeyedEvent[AgentRefStateEvent]](AgentPath("AGENT") <-: AgentCoupled,
        json"""{
          "TYPE": "AgentCoupled",
          "Key": "AGENT"
        }""")

    "AgentCouplingFailed" in:
      testJson[KeyedEvent[AgentRefStateEvent]](AgentPath("AGENT") <-: AgentCouplingFailed(Problem("ERROR")),
        json"""{
          "TYPE": "AgentCouplingFailed",
          "Key": "AGENT",
          "problem": {
            "message": "ERROR"
          }
        }""")

    "AgentReady" in:
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-:
          AgentReady("Europe/Berlin", Some(PlatformInfo.test)),
        json"""{
          "TYPE": "AgentReady",
          "Key": "AGENT",
          "timezone": "Europe/Berlin",
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

    "AgentDedicated" in:
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentDedicated(
          AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))),
          Some(1000L)),
        json"""{
          "TYPE": "AgentDedicated",
          "Key": "AGENT",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w",
          "agentEventId": 1000
        }""")

      // Compatible with 2.0.0-alpha.20210909
      testJsonDecoder[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentDedicated(
          AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))),
          Some(1000L)),
        json"""{
          "TYPE": "AgentCreated",
          "Key": "AGENT",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w",
          "agentEventId": 1000
        }""")

    "AgentDedicated, compatible" in:
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentDedicated(
          AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))),
          None),
        json"""{
          "TYPE": "AgentDedicated",
          "Key": "AGENT",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
        }""")

    "AgentEventsObserved" in:
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentEventsObserved(123L),json"""
        {
          "TYPE":  "AgentEventsObserved",
          "Key": "AGENT",
          "untilEventId": 123
        }""")

    "AgentResetStarted" in:
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentResetStarted(force = true),json"""
        {
          "TYPE": "AgentResetStarted",
          "Key": "AGENT",
          "force": true
        }""")

      testJsonDecoder[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentResetStarted(), json"""
        {
          "TYPE": "AgentResetStarted",
          "Key": "AGENT"
        }""")

    "AgentMirroredEvent" in:
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-:
          AgentMirroredEvent(ClusterEvent.ClusterPassiveLost(NodeId("Backup"))),
        json"""
        {
          "TYPE": "AgentMirroredEvent",
          "Key": "AGENT",
          "event": {
            "TYPE": "ClusterPassiveLost",
            "id": "Backup"
          }
        }""")

    "AgentClusterWatchConfirmationRequired" in:
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-:
          AgentClusterWatchConfirmationRequired(
            ClusterNodeLossNotConfirmedProblem(
            NodeId("Primary"),
            ClusterEvent.ClusterPassiveLost(NodeId("Backup")))),
        json"""
        {
          "TYPE": "AgentClusterWatchConfirmationRequired",
          "Key": "AGENT",
          "problem": {
            "TYPE": "ClusterNodeLossNotConfirmedProblem",
            "fromNodeId": "Primary",
            "event": {
              "TYPE": "ClusterPassiveLost",
              "id": "Backup"
            }
          }
        }""")
  }
