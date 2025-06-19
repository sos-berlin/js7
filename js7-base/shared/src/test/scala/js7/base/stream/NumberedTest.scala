package js7.base.stream

import io.circe.DecodingFailure
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

final class NumberedTest extends OurTestSuite:

  "JSON" in:
    testJson(Numbered(0L, "NULL"), json""" "NULL" """)
    testJson(Numbered(1L, "ONE"), json""" [ 1, "ONE" ]""")

    assert(json"""[ 1, "ONE", "garbage" ]""".as[Numbered[String]] ==
      Left(DecodingFailure(
        "For Numbered[String], a JSON array with exactly two elements is expected",
        Nil)))
