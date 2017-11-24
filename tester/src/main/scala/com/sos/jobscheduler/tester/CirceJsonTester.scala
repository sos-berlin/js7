package com.sos.jobscheduler.tester

import io.circe
import io.circe.Json
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedObjectEncoder
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import java.lang.Character.isWhitespace
import org.scalatest.Assertions._
import shapeless.Lazy

/**
  * @author Joacim Zschimmer
  */
object CirceJsonTester {
  private val printer = circe.Printer.noSpaces.copy(dropNullKeys = true/*drops None*/)

  private[tester] def testCirceJson[A](a: A, jsonString: String)(implicit lazyEncoder: Lazy[DerivedObjectEncoder[A]], lazyDecoder: Lazy[DerivedDecoder[A]]): Unit = {
    implicit val decoder = lazyDecoder.value
    implicit val encoder = lazyEncoder.value

    val asJson: Json = removeJNull(a.asJson)  // Circe converts None to JNull which we remove here (like Printer dropNullKeys = true)

    assert(Right(asJson) == parse(jsonString))
    assert(parse(jsonString).flatMap(_.as[A]) == Right(a))
    assert(printer.pretty(asJson).length == jsonString.count(o ⇒ !isWhitespace(o)))
    assert(parse(jsonString) == parse(printer.pretty(asJson)))
    assert(parse(printer.pretty(asJson)) == Right(asJson))
  }

  def removeJNull(json: Json): Json =
    json.asObject match {
      case Some(o) ⇒ Json.fromFields(o.toMap collect {
        case (k, v) if !v.isNull ⇒ k → removeJNull(v)
      })
      case None ⇒ json
    }
}
