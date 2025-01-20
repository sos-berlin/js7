package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.plan.{PlanId, PlanSchemaId}
import js7.tester.CirceJsonTester.{testJson, testJsonEncoder}

final class NoticeIdTest extends OurTestSuite:

  "JSON" in:
    testJson[NoticeId](
      PlanSchemaId("DailyPlan") / "2025-01-17" / BoardPath("BOARD") / NoticeKey("NOTICE"),
      json"""[
        "DailyPlan", "2025-01-17", "BOARD", "NOTICE"
      ]""")

    testJson[NoticeId](
      PlanSchemaId("DailyPlan") / "2025-01-17" / BoardPath("BOARD") / NoticeKey.empty,
      json"""[
        "DailyPlan", "2025-01-17", "BOARD"
      ]""")

    testJson[NoticeId](
      PlanId.Global / BoardPath("BOARD") / NoticeKey("NOTICE"),
      json"""[
        "BOARD", "NOTICE"
      ]""")

    // Invalid NoticeId because NoticeKey must not be empty
    testJsonEncoder[NoticeId](
      PlanId.Global / BoardPath("BOARD") / NoticeKey.empty,
      json"""[
        "BOARD"
      ]""")
