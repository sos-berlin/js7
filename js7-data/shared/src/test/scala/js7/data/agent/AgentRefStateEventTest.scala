package js7.data.agent

import java.util.UUID
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data.agent.AgentRefStateEvent.{AgentCoupled, AgentCouplingFailed, AgentDedicated, AgentEventsObserved, AgentReady, AgentResetStarted}
import js7.data.event.{JournalId, KeyedEvent}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}
import org.scalatest.freespec.AnyFreeSpec

final class AgentRefStateEventTest extends AnyFreeSpec
{
  "JSON" - {
    "AgentCoupled" in {
      testJson[KeyedEvent[AgentRefStateEvent]](AgentPath("AGENT") <-: AgentCoupled,
        json"""{
          "TYPE": "AgentCoupled",
          "Key": "AGENT"
        }""")
    }

    "AgentCouplingFailed" in {
      testJson[KeyedEvent[AgentRefStateEvent]](AgentPath("AGENT") <-: AgentCouplingFailed(Problem("ERROR")),
        json"""{
          "TYPE": "AgentCouplingFailed",
          "Key": "AGENT",
          "problem": {
            "message": "ERROR"
          }
        }""")
    }

    "AgentReady" in {
      testJson[KeyedEvent[AgentRefStateEvent]](AgentPath("AGENT") <-: AgentReady("Europe/Berlin"),
        json"""{
          "TYPE": "AgentReady",
          "Key": "AGENT",
          "timezone": "Europe/Berlin"
        }""")
    }

    "AgentDedicated" in {
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
    }

    "AgentDedicated, compatible" in {
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentDedicated(
          AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))),
          None),
        json"""{
          "TYPE": "AgentDedicated",
          "Key": "AGENT",
          "agentRunId": "ABEiM0RVZneImaq7zN3u_w"
        }""")
    }

    "AgentEventsObserved" in {
      testJson[KeyedEvent[AgentRefStateEvent]](
        AgentPath("AGENT") <-: AgentEventsObserved(123L),json"""
        {
          "TYPE":  "AgentEventsObserved",
          "Key": "AGENT",
          "untilEventId": 123
        }""")
    }

    "AgentResetStarted" in {
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
    }
  }
}
