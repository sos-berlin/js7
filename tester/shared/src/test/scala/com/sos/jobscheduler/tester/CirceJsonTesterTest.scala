package com.sos.jobscheduler.tester

import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import com.sos.jobscheduler.tester.CirceJsonTesterTest._
import io.circe.generic.JsonCodec
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CirceJsonTesterTest extends AnyFreeSpec
{
  "Case class" in {
    testJson(a, JsonString)
  }
}

object CirceJsonTesterTest
{
  private val JsonString = """{
      "boole": true,
      "int": 2147483647,
      "long": -9223372036854775808,
      "double": 1.23456789E33,
      "string": "STRING",
      "some": 1,
      "seq": [1, 2, 3],
      "set": [ "a", "b" ]
    }"""

  private val a = A(true, Int.MaxValue, Long.MinValue, 1.23456789e33, "STRING", Some(1), None, List(1, 2, 3), Set("a", "b"))

  @JsonCodec
  final case class A(
    boole: Boolean,
    int: Int,
    long: Long,
    double: Double,
    string: String,
    some: Option[Int],
    none: Option[Int],
    seq: Seq[Int],
    set: Set[String])
}
