package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.plan.{PlanKey, PlanSchemaId}
import js7.tester.CirceJsonTester.testJson

final class PlannedNoticeKeyTest extends OurTestSuite:

  "JSON" - {
    "PlannedNoticeKey for GlobalBoard" in:
      testJson[PlannedNoticeKey](GlobalNoticeKey("NOTICE"), json"""
        "NOTICE"
       """)

    "PlannedNoticeKey with all fields" in:
      testJson[PlannedNoticeKey](
        PlanSchemaId("DALIY-PLAN") / PlanKey("2024-11-08") / NoticeKey("NOTICE"),
        json"""
          [ "DALIY-PLAN", "2024-11-08", "NOTICE" ]
        """)

    "PlannedNoticeKey with empty NoticeKey" in:
      testJson[PlannedNoticeKey](
        PlanSchemaId("DALIY-PLAN") / PlanKey("2024-11-08") / NoticeKey.empty,
        json"""
          [ "DALIY-PLAN", "2024-11-08"]
        """)

  }
