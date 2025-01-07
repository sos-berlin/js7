package js7.data.plan

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.problem.{Problem, ProblemException}
import js7.base.test.OurTestSuite

final class PlanSchemaIdTest extends OurTestSuite:

  "Global is a reserved name" in:
    assert(PlanSchemaId.checked("Global") == Left(Problem("PlanSchemaId:Global is a reserved name")))

  "Global is not JSON-serializable" in:
    // Use None instead of "Global"

    intercept[IllegalArgumentException]:
      PlanSchemaId.Global.asJson

    assert(json""" "Global" """.as[PlanSchemaId].toChecked ==
      Left(Problem("JSON DecodingFailure at : PlanSchemaId:Global is a reserved name")))

  "Global is a reserved word" in:
    assert(PlanSchemaId.checked("Global") ==
      Left(Problem("PlanSchemaId:Global is a reserved name")))

    intercept[ProblemException]:
      PlanSchemaId("Global")
