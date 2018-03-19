package com.sos.jobscheduler.tester

import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, Printer}
import org.scalatest.Assertions._

/**
  * @author Joacim Zschimmer
  */
object CirceJsonTester {
  private val printer = Printer.noSpaces.copy(dropNullValues = true/*drops None*/)
  private val prettyPrinter = Printer.spaces2.copy(preserveOrder = true, colonLeft = "", lrbracketsEmpty = "")

  def testJson[A: Encoder: Decoder](a: A, jsonString: String): Unit =
    testJson(a, parseJson(jsonString))

  def testJson[A: Encoder: Decoder](a: A, json: ⇒ Json): Unit = {
    // Do a.asJson first to get the JSON string, then evaluate lazy json (which may have syntax errors during development).
    val asJson: Json = removeJNull(a.asJson)  // Circe converts None to JNull which we remove here (like Printer dropNullValues = true)
    if (asJson != json) fail(s"${prettyPrinter.pretty(normalize(asJson))} did not equal ${prettyPrinter.pretty(normalize(json))}")
    assert(rightOrThrow(json.as[A]) == a)
    assert(json == parseJson(printer.pretty(asJson)))
    assert(parseJson(printer.pretty(asJson)) == asJson)
  }

  private def parseJson(string: String): Json =
    rightOrThrow(parse(string))

  private def rightOrThrow[R](either: Either[Throwable, R]): R =
    either match {
      case Right(o) ⇒ o
      case Left(t) ⇒ throw new RuntimeException(t.toString, t)   // Add stacktrace
    }

  private def normalize(json: Json): Json =
    json.asObject match {
      case Some(o) ⇒ Json.fromFields(o.toVector.sortBy(_._1).map { case (k, v) ⇒ k → normalize(v) })
      case None ⇒
        json.asArray match {
          case Some(o) ⇒ Json.fromValues(o map normalize)
          case None ⇒ json
        }
    }

  private def removeJNull(json: Json): Json =
    json.asObject match {
      case Some(o) ⇒ Json.fromFields(o.toMap collect {
        case (k, v) if !v.isNull ⇒ k → removeJNull(v)
      })
      case None ⇒
        json.asArray match {
          case Some(elements) ⇒ Json.fromValues(elements map removeJNull)
          case None ⇒ json
        }
    }
}
