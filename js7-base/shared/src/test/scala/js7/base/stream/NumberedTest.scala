package js7.base.stream

import io.circe.DecodingFailure
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class NumberedTest extends OurTestSuite:
  "JSON" in:
    testJson(Numbered(1L, "ONE"),json""" [ 1, "ONE" ]""")

    // A only yields Numbered[A](0, a)
    testJsonDecoder(Numbered(0, "ONE"),json""" "ONE"""")

    assert(json"""[ 1, "ONE", "garbage" ]""".as[Numbered[String]] ==
      Left(DecodingFailure("For Numbered, a JSON array with exactly two elements expected", Nil)))
