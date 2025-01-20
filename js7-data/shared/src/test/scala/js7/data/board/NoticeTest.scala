package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.data.plan.PlanId
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class NoticeTest extends OurTestSuite:

  "JSON" in:
    testJson(
      Notice(PlanId.Global / BoardPath("BOARD") / NoticeKey("NOTICE"),
        endOfLife = Some(ts"2025-01-17T12:00:00Z")),
      json"""{
        "id": [ "BOARD", "NOTICE" ],
        "endOfLife": 1737115200000
      }""")

    testJson(
      Notice(PlanId.Global / BoardPath("BOARD") / NoticeKey("NOTICE"), endOfLife = None),
      json"""{
        "id": [ "BOARD", "NOTICE" ]
      }""")

    // COMPATIBLE with v2.7.2
    testJsonDecoder(
      Notice(PlanId.Global / BoardPath("BOARD") / NoticeKey("NOTICE"),
        endOfLife = Some(ts"2025-01-17T12:00:00Z")),
      json"""{
        "boardPath": "BOARD",
        "id": "NOTICE",
        "endOfLife": 1737115200000
      }""")

    // COMPATIBLE with v2.7.2
    testJsonDecoder(
      Notice(PlanId.Global / BoardPath("BOARD") / NoticeKey("NOTICE"), endOfLife = None),
      json"""{
        "boardPath": "BOARD",
        "id": "NOTICE"
      }""")
