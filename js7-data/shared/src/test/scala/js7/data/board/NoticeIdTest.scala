package js7.data.board

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.plan.{PlanId, PlanItemId, PlanKey}
import js7.tester.CirceJsonTester
import js7.tester.CirceJsonTester.testJson

final class NoticeIdTest extends OurTestSuite:

  "JSON" - {
    "NoticeId for GlobalBoard" in:
      testJson[NoticeId](NoticeId("NOTICE"), json"""
        "NOTICE"
       """)

    "NoticeId for PlannedBoard" in:
      testJson[NoticeId](
        NoticeId.planned:
          PlanId(PlanItemId("DALIY-PLAN"), PlanKey("2024-11-08")),
        json"""
          [ "DALIY-PLAN", "2024-11-08"]
        """)

    "NoticeId with all fields" in:
      testJson[NoticeId](
        NoticeId(
          NoticeKey("NOTICE"),
          PlanId(PlanItemId("DALIY-PLAN"), PlanKey("2024-11-08"))),
        json"""
          [ "DALIY-PLAN", "2024-11-08", "NOTICE" ]
        """)
  }
