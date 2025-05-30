package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class PlanIdTest extends OurTestSuite:

  "JSON" - {
    "standard" in:
      testJson(
        PlanId(PlanSchemaId("DailyPlan"), PlanKey("2024-11-20")),
        json""" [ "DailyPlan", "2024-11-20" ] """)

    "Global" in:
      testJson(PlanId.Global, json""" [] """)

    //"PlanId.Global must not be encoded like any other PlanId" in:
    //  intercept[IllegalArgumentException]:
    //    PlanId.Global.asJson
    //
    //  assert(json""" [ "Global", "" ] """.as[PlanId].swap.toOption.get.toString ==
    //    "DecodingFailure at [0]: PlanSchemaId:Global is a reserved name")
    //
    //  assert(json""" [ "Global", "X" ] """.as[PlanId].swap.toOption.get.toString ==
    //    "DecodingFailure at [0]: PlanSchemaId:Global is a reserved name")
  }

  "Global" in:
    assert(PlanId.Global == PlanSchemaId("Global") / "Global")
    assert(PlanId.Global.toString == "Plan:Global")

  "toString" in:
    assert(PlanId(PlanSchemaId("DailyPlan"), PlanKey("2024-11-22")).toString ==
      "Plan:DailyPlan╱2024-11-22")
