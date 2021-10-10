package js7.tester

import cats.implicits.toShow
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json, ParsingFailure, Printer}
import org.scalactic.source
import org.scalatest.Assertion
import org.scalatest.Assertions._

/**
  * @author Joacim Zschimmer
  */
object CirceJsonTester
{
  private val printer = Printer.noSpaces.copy(dropNullValues = true/*drops None*/)
  private val prettyPrinter = Printer.spaces2.copy(colonLeft = "", lrbracketsEmpty = "")

  def testJsonString[A: Encoder: Decoder](a: A, jsonString: String): Assertion =
    testJson(a, parseJson(jsonString))

  def testJson[A: Encoder: Decoder](a: A, json: => Json)(implicit pos: source.Position): Assertion = {
    // Do a.asJson first to get the JSON string, then evaluate lazy json (which may have syntax errors during development).
    val asJson: Json = removeJNull(a.asJson)  // Circe converts None to JNull which we remove here (like Printer dropNullValues = true)
    if (asJson != json) fail(s"${prettyPrinter.print(normalizeJson(asJson))} did not equal ${prettyPrinter.print(normalizeJson(json))}")
    val reencoded = rightOrThrow(json.as[A])
    if (a != reencoded) {
      assert(a == reencoded)
    }
    val reparsed = parseJson(printer.print(asJson))
    assert(reparsed == json)
    assert(reparsed == asJson)
  }

  def testJsonDecoder[A: Decoder](a: A, json: => Json): Assertion = {
    // Do a.asJson first to get the JSON string, then evaluate lazy json (which may have syntax errors during development).
    assert(a == rightOrThrow(json.as[A]))
  }

  private def parseJson(string: String): Json =
    rightOrThrow(parse(string))

  private def rightOrThrow[R](either: Either[io.circe.Error, R]): R =
    either match {
      case Left(t: DecodingFailure) =>
        throw new RuntimeException(t.show, t)
      case Left(t: ParsingFailure) =>
        throw new RuntimeException(t.show, t)   // Add stacktrace
      case Right(o) => o
    }

  def normalizeJson(json: Json): Json =
    removeJNull(
      json.asObject match {
        case Some(o) =>
          Json.fromFields(o.toVector.sortBy(_._1).map { case (k, v) => k -> normalizeJson(v) })

        case None =>
          json.asArray match {
            case Some(o) => Json.fromValues(o map normalizeJson)
            case None => json
          }
      })

  def removeJNull(json: Json): Json =
    json.asObject match {
      case Some(o) => Json.fromFields(o.toIterable collect {
        case (k, v) if !v.isNull => k -> removeJNull(v)
      })
      case None =>
        json.asArray match {
          case Some(elements) => Json.fromValues(elements map removeJNull)
          case None => json
        }
    }
}
