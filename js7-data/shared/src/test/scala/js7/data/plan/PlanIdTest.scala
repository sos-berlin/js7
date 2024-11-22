package js7.data.plan

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester
import js7.tester.CirceJsonTester.testJson

final class PlanIdTest extends OurTestSuite:

  "JSON" - {
    "standard" in:
      testJson(
        PlanId(PlanItemId("DailyPlan"), PlanKey("2024-11-20")),
        json""" [ "DailyPlan", "2024-11-20" ] """)

    "PlanId.Global must not be encoded like any other PlanId" in:
      intercept[IllegalArgumentException]:
        PlanId.Global.asJson

      assert(json""" [ "Global", "" ] """.as[PlanId].swap.toOption.get.toString ==
        "DecodingFailure at [0]: PlanItemId:Global is a reserved name")

      assert(json""" [ "Global", "X" ] """.as[PlanId].swap.toOption.get.toString ==
        "DecodingFailure at [0]: PlanItemId:Global is a reserved name")
  }

  "toString" in:
    assert(PlanId.Global.toString == "Plan:Global")
    assert(PlanId(PlanItemId("DailyPlan"), PlanKey("2024-11-22")).toString ==
      "Plan:DailyPlan/2024-11-22")
