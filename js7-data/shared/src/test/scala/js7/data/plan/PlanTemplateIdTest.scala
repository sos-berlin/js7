package js7.data.plan

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.problem.{Problem, ProblemException}
import js7.base.test.OurTestSuite

final class PlanTemplateIdTest extends OurTestSuite:

  "Global is a reserved name" in:
    assert(PlanTemplateId.checked("Global") == Left(Problem("PlanTemplateId:Global is a reserved name")))

  "Global is not JSON-serializable" in:
    // Use None instead of "Global"

    intercept[IllegalArgumentException]:
      PlanTemplateId.Global.asJson

    assert(json""" "Global" """.as[PlanTemplateId].toChecked ==
      Left(Problem("JSON DecodingFailure at : PlanTemplateId:Global is a reserved name")))

  "Global is a reserved word" in:
    assert(PlanTemplateId.checked("Global") ==
      Left(Problem("PlanTemplateId:Global is a reserved name")))

    intercept[ProblemException]:
      PlanTemplateId("Global")
