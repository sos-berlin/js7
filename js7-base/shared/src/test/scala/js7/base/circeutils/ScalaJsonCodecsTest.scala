package js7.base.circeutils

import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.test.Test
import js7.base.time.ScalaTime.*
import js7.tester.CirceJsonTester.testJson
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class ScalaJsonCodecsTest extends Test
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
      testJson(-1234.ms, json"-1.234")
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
