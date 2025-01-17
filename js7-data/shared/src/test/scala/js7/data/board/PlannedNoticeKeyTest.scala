package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.plan.{PlanId, PlanKey, PlanSchemaId}
import js7.tester.CirceJsonTester
import js7.tester.CirceJsonTester.testJson

final class PlannedNoticeKeyTest extends OurTestSuite:

  "JSON" - {
    "PlannedNoticeKey for GlobalBoard" in:
      testJson[PlannedNoticeKey](PlannedNoticeKey("NOTICE"), json"""
        "NOTICE"
       """)

    "PlannedNoticeKey for PlannedBoard" in:
      testJson[PlannedNoticeKey](
        PlannedNoticeKey.planned:
          PlanId(PlanSchemaId("DALIY-PLAN"), PlanKey("2024-11-08")),
        json"""
          [ "DALIY-PLAN", "2024-11-08"]
        """)

    "PlannedNoticeKey with all fields" in:
      testJson[PlannedNoticeKey](
        PlannedNoticeKey(
          PlanId(PlanSchemaId("DALIY-PLAN"), PlanKey("2024-11-08")), 
          NoticeKey("NOTICE")),
        json"""
          [ "DALIY-PLAN", "2024-11-08", "NOTICE" ]
        """)
  }
