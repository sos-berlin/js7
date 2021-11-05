package js7.base.stream

import io.circe.DecodingFailure
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class NumberedTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(Numbered(1L, "ONE"),
      json"""
         [ 1, "ONE" ]
        """)

    assert(json"""[ 1, "ONE", "garbage" ]""".as[Numbered[String]] ==
      Left(DecodingFailure("For Numbered, a JSON array with exactly two elements expected", Nil)))
  }
}
