package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.tester.CirceJsonTester
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class PlanIdTest extends AnyFreeSpec:

  "JSON" - {
    "standard" in:
      testJson(
        PlanId(PlanItemId("DailyPlan"), PlanKey("2024-11-20")),
        json""" [ "DailyPlan", "2024-11-20" ] """)

    "PlanId.Global" in:
      testJson(PlanId.Global, json""" null """)

    "PlanId.Global must not be encoded like any other PlanId" in:
      assert(json""" [ "Global", "" ] """.as[PlanId].swap.toOption.get.toString ==
        "DecodingFailure at [1]: Invalid PlanKey")

      assert(json""" [ "Global", "X" ] """.as[PlanId].swap.toOption.get.toString ==
        "DecodingFailure at : Invalid Global PlanId")
  }