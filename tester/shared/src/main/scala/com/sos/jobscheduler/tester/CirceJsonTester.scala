package com.sos.jobscheduler.tester

import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, Printer}
import java.lang.Character.isWhitespace
import org.scalatest.Assertions._

/**
  * @author Joacim Zschimmer
  */
object CirceJsonTester {
  private val printer = Printer.noSpaces.copy(dropNullKeys = true/*drops None*/)

  def testJson[A: Encoder: Decoder](a: A, jsonString: String): Unit = {
    val asJson: Json = removeJNull(a.asJson)  // Circe converts None to JNull which we remove here (like Printer dropNullKeys = true)
    val compressedJsonString = jsonString filter (o ⇒ !isWhitespace(o))
    val parsed = parseJson(jsonString)
    assert(asJson == parsed)
    assert(forceLeft(parsed.as[A]) == a)
    assert(printer.pretty(asJson).length == compressedJsonString.length, s", expected: $compressedJsonString")
    assert(parsed == parseJson(printer.pretty(asJson)))
    assert(parseJson(printer.pretty(asJson)) == asJson)
  }

  private def parseJson(string: String): Json =
    forceLeft(parse(string)
    )

  private def forceLeft[R](either: Either[Throwable, R]): R =
    either match {
      case Right(o) ⇒ o
      case Left(t) ⇒ throw new RuntimeException(t.toString, t)   // Add stacktrace
    }

  def removeJNull(json: Json): Json =
    json.asObject match {
      case Some(o) ⇒ Json.fromFields(o.toMap collect {
        case (k, v) if !v.isNull ⇒ k → removeJNull(v)
      })
      case None ⇒ json
    }
}
