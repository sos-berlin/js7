package com.sos.jobscheduler.base.circeutils.typed

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodec.{UnknownClassForJsonException, UnknownJsonTypeException}
import com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodecTest._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class TypedJsonCodecTest extends FreeSpec {

  "encode and decode" in {
    testJson[A](A0     , """{ "TYPE": "A0" }""")
    testJson[A](A1(7)  , """{ "TYPE": "A1", "int": 7 }""")
    testJson[A](A2("X"), """{ "TYPE": "A2", "string": "X" }""")
  }

  "encode unknown subclass" in {
    intercept[UnknownClassForJsonException] {
      (NotRegistered(1): A).asJson(JsonCodec)
    }.getMessage should include (
      "Class com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodecTest$NotRegistered is not registered with JsonTypeCodec[com.sos.jobscheduler.base.circeutils.typed.TypedJsonCodecTest$A]")
  }

  "decode unknown subclass" in {
    intercept[UnknownJsonTypeException] {
      """{ "TYPE": "UNKNOWN" }""".parseJson.as[A].force
    }.getMessage should include ("""Unexpected JSON { "TYPE": "UNKNOWN" } for class 'A'""")
  }

  "Nested TypedJsonCodec" in {
    testJson[A](AA1(7)  , """{ "TYPE": "AA1", "int": 7 }""")
  }
}

object TypedJsonCodecTest {
  sealed trait A
  case object A0 extends A
  @JsonCodec final case class A1(int: Int) extends A
  @JsonCodec final case class A2(string: String) extends A
  @JsonCodec final case class NotRegistered(int: Int) extends A

  sealed trait AA extends A
  @JsonCodec final case class AA1(int: Int) extends AA

  private implicit val AAJsonCodec: TypedJsonCodec[AA] = TypedJsonCodec[AA](
    Subtype[AA1])

  private implicit val JsonCodec: TypedJsonCodec[A] = TypedJsonCodec[A](
    Subtype(A0),
    Subtype[A1],
    Subtype[A2],
    Subtype[AA])
}
