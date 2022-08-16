package js7.base.log

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.log.CorrelIdWrappedTest.*
import js7.base.test.Test
import js7.tester.CirceJsonTester.*

final class CorrelIdWrappedTest extends Test
{
  "JSON" in {
    testJson(
      CorrelIdWrapped(CorrelId("_CORREL_"), A(7)),
      json"""{
        "correlId": "_CORREL_",
        "number": 7
      }""")
  }
}

object CorrelIdWrappedTest
{
  final case class A(number: Int)
  object A {
    implicit val jsonCode: Codec.AsObject[A] = deriveCodec[A]
  }
}
