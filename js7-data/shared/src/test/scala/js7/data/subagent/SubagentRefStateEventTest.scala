package js7.data.subagent

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.utils.Base64UUID
import js7.data.event.KeyedEvent
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SubagentRefStateEventTest extends AnyFreeSpec
{
  "JSON" - {
    "SubagentCoupled" in {
      testJson[KeyedEvent[SubagentRefStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentRefStateEvent.SubagentCoupled,
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentCoupled"
        }""")
    }

    "SubagentCouplingFailed" in {
      testJson[KeyedEvent[SubagentRefStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentRefStateEvent.SubagentCouplingFailed(Problem("PROBLEM")),
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentCouplingFailed",
          "problem": {
            "message": "PROBLEM"
          }
        }""")
    }

    "SubagentDedicated" in {
      testJson[KeyedEvent[SubagentRefStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentRefStateEvent.SubagentDedicated(SubagentRunId(Base64UUID.zero)),
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentDedicated",
          "subagentRunId": "AAAAAAAAAAAAAAAAAAAAAA"
        }""")
    }

    "SubagentEventsObserved" in {
      testJson[KeyedEvent[SubagentRefStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentRefStateEvent.SubagentEventsObserved(1001L),
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentEventsObserved",
          "untilEventId": 1001
        }""")
    }

    "SubagentRestarted" in {
      testJson[KeyedEvent[SubagentRefStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentRefStateEvent.SubagentRestarted,
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentRestarted"
        }""")
    }

    "SubagentShutdown" in {
      testJson[KeyedEvent[SubagentRefStateEvent]](
        SubagentId("SUBAGENT") <-: SubagentRefStateEvent.SubagentShutdown,
        json"""{
          "Key": "SUBAGENT",
          "TYPE": "SubagentShutdown"
        }""")
    }
  }
}
