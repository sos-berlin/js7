package js7.data.plan

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.time.ScalaTime.*
import js7.data.plan.PlanSchemaEvent.PlanSchemaChanged
import js7.data.value.StringValue
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class PlanSchemaEventTest extends AnyFreeSpec:

  "JSON" in:
    testJson[PlanSchemaEvent](
      PlanSchemaChanged(
        finishedPlanRetentionPeriod = Some(24.h),
        Some(Map("NAME" -> StringValue("VALUE")))),
      json"""{
        "TYPE": "PlanSchemaChanged",
        "finishedPlanRetentionPeriod": 86400,
        "namedValues": {
          "NAME": "VALUE"
        }
      }""")
