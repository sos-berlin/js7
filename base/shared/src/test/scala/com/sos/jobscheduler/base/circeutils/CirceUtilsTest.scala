package com.sos.jobscheduler.base.circeutils

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import io.circe.{Json, JsonObject}
import org.scalatest.FreeSpec
import scala.collection.immutable._

/**
  * @author Joacim Zschimmer
  */
final class CirceUtilsTest extends FreeSpec {

  private case class A(a: Int, b: B)
  private case class B(string: String, array: Seq[Int], empty: Seq[Int])

  private implicit val BCodec = deriveCodec[B]
  private implicit val ACodec = deriveCodec[A]

  "PrettyPrinter" in {
    assert(A(1, B("STRING", 1 :: 2 :: Nil, Nil)).asJson.toPrettyString ==
      """{
      |  "a": 1,
      |  "b": {
      |    "string": "STRING",
      |    "array": [
      |      1,
      |      2
      |    ],
      |    "empty": []
      |  }
      |}""".stripMargin)
  }

  "listMapCodec" in {
    final case class A(a: Int, listMap: ListMap[Int, String])
    implicit val myListMapCodec = listMapCodec[Int, String]()
    implicit val aCodec = deriveCodec[A]
    testJson(A(0, ListMap(1 → "eins", 2 → "zwei", 3 → "drei", 4 → "vier")),
      """{
        "a": 0,
        "listMap": [
          { "key": 1, "value": "eins" },
          { "key": 2, "value": "zwei" },
          { "key": 3, "value": "drei" },
          { "key": 4, "value": "vier" }
        ]
      }""")
  }

  "json string interpolator" - {
    "Simple string" in {
      assert(json"""{ "A": "STRING" }""" == Json.obj("A" → Json.fromString("STRING")))
      assert(json"""{ "A": "STRING\"\u007f." }""" == Json.obj("A" → Json.fromString("STRING\"\u007f.")))
    }

    "Interpolating String value" in {
      for (string ← "STRING" :: "STRING\"" :: "STRING\"\u007f." :: Nil)
        assert(json"""{ "A": "!$string" }""" == Json.obj("A" → Json.fromString("!" + string)))
    }

    "Interpolating Int value" in {
      val i = 7
      assert(json"""{ "A": $i }""" ==  Json.obj("A" → Json.fromInt(7)))
    }

    "Interpolating Array value" in {
      val array = List(1, 2, 3)
      assert(json"""{ "A": $array }""" ==  Json.obj("A" → Json.fromValues(array map Json.fromInt)))
    }
  }

  "jsonString string interpolator" in {
    assert(jsonString"""{ "A": 7 }""" == """{ "A": 7 }""")  // Only to let IDE highlight JSON error
  }

  "parseJsonChecked" in {
    assert("7".parseJsonChecked == Valid(Json.fromInt(7)))
    assert("x".parseJsonChecked == Invalid(Problem("expected json value got x (line 1, column 1)")))
  }

  "parseJsonOrThrow" in {
    assert("7".parseJsonOrThrow == Json.fromInt(7))
    intercept[io.circe.ParsingFailure] {
      "x".parseJsonOrThrow
    }
  }

  "JsonObject ++ JsonObject" in {
    assert(JsonObject("a" → 1.asJson) ++ JsonObject("b" → 2.asJson) == JsonObject("a" → 1.asJson, "b" → 2.asJson))
  }
}
