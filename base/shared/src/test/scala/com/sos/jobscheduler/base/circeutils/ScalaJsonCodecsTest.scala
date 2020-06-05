package js7.base.circeutils

import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.ScalaJsonCodecs._
import js7.tester.CirceJsonTester.testJson
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ScalaJsonCodecsTest extends AnyFreeSpec
{
  "FiniteDuration" - {
    "1s" in {
      testJson(1.second, json"1")
    }

    "1ms" in {
      testJson(1.millisecond, json"0.001")
    }

    "1ns" in {
      testJson(1.nanosecond, json"0.000000001")
    }

    "negative" in {
      testJson(-1234.milliseconds, json"-1.234")
    }

    "Long.MaxValue" in {
      testJson(Duration(Long.MaxValue, NANOSECONDS), json"9223372036.854775807")
    }

    "Invalid" in {
      assert(""" "1x" """.parseJsonOrThrow.as[FiniteDuration].isLeft)
    }

    //"ISO-8601" in {
    //  assert(""" "PT1m2.123S" """.parseJsonOrThrow.as[Duration].orThrow == Duration.ofSeconds(62, 123456789))
    //}
  }
}
