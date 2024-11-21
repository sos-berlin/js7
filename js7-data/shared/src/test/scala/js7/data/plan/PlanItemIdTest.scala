package js7.data.plan

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.problem.Problem
import js7.base.test.OurTestSuite

final class PlanItemIdTest extends OurTestSuite:

  "Global is a reserved name" in:
    assert(PlanItemId.checked("Global") == Left(Problem("PlanItemId:Global is a reserved name")))

  "Global is not JSON-serializable" in:
    // Use None instead of "Global"

    intercept[IllegalArgumentException]:
      PlanItemId.Global.asJson

    assert(json""" "Global" """.as[PlanItemId].toChecked ==
      Left(Problem("JSON DecodingFailure at : PlanItemId:Global is a reserved name")))
