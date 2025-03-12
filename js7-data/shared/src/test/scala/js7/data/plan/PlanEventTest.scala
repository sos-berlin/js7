package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.data.event.KeyedEvent
import js7.data.plan.PlanEvent.{PlanClosed, PlanDeleted, PlanFinished, PlanOpened}
import js7.tester.CirceJsonTester.testJson

final class PlanEventTest extends OurTestSuite:

  "JSON" - {
    "PlanOpened" in:
      testJson[KeyedEvent[PlanEvent]](
        PlanSchemaId("DailyPlan") / "2025-02-11" <-: PlanOpened,
        json"""{
          "TYPE": "PlanOpened",
          "Key": [ "DailyPlan", "2025-02-11" ]
        }""")

    "PlanClosed" in:
      testJson[KeyedEvent[PlanEvent]](
        PlanSchemaId("DailyPlan") / "2025-02-11" <-: PlanClosed,
        json"""{
          "TYPE": "PlanClosed",
          "Key": [ "DailyPlan", "2025-02-11" ]
        }""")

    "PlanFinished" in:
      testJson[KeyedEvent[PlanEvent]](
        PlanSchemaId("DailyPlan") / "2025-02-11" <-: PlanFinished(ts"2025-03-10T12:00:00Z"),
        json"""{
          "TYPE": "PlanFinished",
          "Key": [ "DailyPlan", "2025-02-11" ],
          "at": 1741608000000
        }""")

    "PlanDeleted" in:
      testJson[KeyedEvent[PlanEvent]](
        PlanSchemaId("DailyPlan") / "2025-02-11" <-: PlanDeleted,
        json"""{
          "TYPE": "PlanDeleted",
          "Key": [ "DailyPlan", "2025-02-11" ]
        }""")
  }
