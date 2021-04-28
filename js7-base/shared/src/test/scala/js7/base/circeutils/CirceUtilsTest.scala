package js7.base.circeutils

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString
import js7.base.problem.{Problem, ProblemException}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.SeqMap

/**
  * @author Joacim Zschimmer
  */
final class CirceUtilsTest extends AnyFreeSpec
{
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
    final case class A(a: Int, listMap: SeqMap[Int, String])
    implicit val mySeqMapCodec = listMapCodec[Int, String]()
    implicit val aCodec = deriveCodec[A]
    testJson(A(0, SeqMap(1 -> "eins", 2 -> "zwei", 3 -> "drei", 4 -> "vier")),
      json"""{
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
      assert(json"""{ "A": "STRING" }""" == Json.obj("A" -> Json.fromString("STRING")))
      assert(json"""{ "A": "STRING\"\u007f." }""" == Json.obj("A" -> Json.fromString("STRING\"\u007f.")))
    }

    "Interpolating String value" in {
      for (string <- "STRING" :: "STRING\"" :: "STRING\"\u007f." :: Nil)
        assert(json"""{ "A": "!$string" }""" == Json.obj("A" -> Json.fromString("!" + string)))
    }

    "Interpolating GenericString value" in {
      case class MyGenericString(string: String) extends GenericString
      for (string <- MyGenericString("STRING") :: MyGenericString("STRING\")") :: MyGenericString("STRING\"\u007f.") :: Nil)
        assert(json"""{ "A": "!$string" }""" == Json.obj("A" -> Json.fromString("!" + string)))
    }

    "Interpolating Int value" in {
      val i = 7
      assert(json"""{ "A": $i }""" == Json.obj("A" -> Json.fromInt(7)))
    }

    "Interpolating Array value" in {
      val array = List(1, 2, 3)
      assert(json"""{ "A": $array }""" == Json.obj("A" -> Json.fromValues(array map Json.fromInt)))
    }
  }

  "jsonString string interpolator" in {
    assert(jsonString"""{ "A": 7 }""" == """{ "A": 7 }""")  // Only to let IDE highlight JSON error
  }

  "parseJson" in {
    assert("7".parseJson == Right(Json.fromInt(7)))
    assert("x".parseJson == Left(Problem("JSON ParsingFailure: expected json value got 'x' (line 1, column 1)")))
  }

  "parseJsonOrThrow" in {
    assert("7".parseJsonOrThrow == Json.fromInt(7))
    intercept[ProblemException] {
      "x".parseJsonOrThrow
    }
  }

  "JsonObject ++ JsonObject" in {
    val a = JsonObject("a" -> 1.asJson)
    assert(a ++ JsonObject("b" -> 2.asJson) == JsonObject("a" -> 1.asJson, "b" -> 2.asJson))

    // empty is optimized:
    assert((a ++ JsonObject.empty) eq a)
    assert((JsonObject.empty ++ a) eq a)
  }

  "toProblem decoding error" in {
    case class A(number: Int)
    val decoder: Decoder[A] = _.get[Int]("number") map A.apply
    assert(decoder.decodeJson(json"""{ "number": true }""").left.map(_.toProblem) == Left(Problem("JSON DecodingFailure at .number: Int")))
  }

  "toChecked decoding error" in {
    case class A(number: Int)
    val decoder: Decoder[A] = _.get[Int]("number") map A.apply
    assert(decoder.decodeJson(json"""{ "number": 7 }""").toChecked == Right(A(7)))
    assert(decoder.decodeJson(json"""{ "number": true }""").toChecked == Left(Problem("JSON DecodingFailure at .number: Int")))
    assert(decoder.decodeJson(json"""{ "x": true }""").toChecked == Left(Problem("JSON DecodingFailure at .number: Attempt to decode value on failed cursor")))
  }

  "stringDecoder catches exceptions" in {
    val decoder = stringDecoder(_.toInt)
    assert(decoder.decodeJson(json""""7"""").toChecked == Right(7))
    assert(decoder.decodeJson(json""""X"""").toChecked == Left(Problem(
      """JSON DecodingFailure at : NumberFormatException: For input string: "X"""")))
  }
}
