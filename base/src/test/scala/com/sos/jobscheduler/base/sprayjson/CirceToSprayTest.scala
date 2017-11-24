package com.sos.jobscheduler.base.sprayjson

import com.sos.jobscheduler.base.sprayjson.CirceToSpray.circeToSpray
import com.sos.jobscheduler.base.sprayjson.CirceToSprayTest._
import com.sos.jobscheduler.tester.JsonTester.testCirceAndSprayJson
import io.circe
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedObjectEncoder
import io.circe.parser.{parse ⇒ circeParse}
import java.lang.Character.isWhitespace
import org.scalatest.FreeSpec
import shapeless.Lazy
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class CirceToSprayTest extends FreeSpec {

  "Case class" in {
    val a = A(true, Int.MaxValue, Long.MinValue, 1.23456789e33, "STRING", Some(1), None, List(1, 2, 3), Set("a", "b"))
    implicit val jsonFormat = jsonFormat9(A)
    val jsonString = """{
        "boole": true,
        "int": 2147483647,
        "long": -9223372036854775808,
        "double": 1.23456789E33,
        "string": "STRING",
        "some": 1,
        "seq": [1, 2, 3],
        "set": [ "a", "b" ]
      }"""
    checkCirceAndSpray(a, jsonString)
    testCirceAndSprayJson(a, jsonString)
  }
}
object CirceToSprayTest {
  private val CircePrinter = circe.Printer.noSpaces.copy(dropNullKeys = true)

  private case class A(
    boole: Boolean,
    int: Int,
    long: Long,
    double: Double,
    string: String,
    some: Option[Int],
    none: Option[Int],
    seq: Seq[Int],
    set: Set[String])

  private def checkCirceAndSpray[A: JsonFormat](a: A, jsonString: String)(implicit lazyEncoder: Lazy[DerivedObjectEncoder[A]], lazyDecoder: Lazy[DerivedDecoder[A]]): Unit = {
    implicit val decoder = lazyDecoder.value

    val circeJson: circe.Json = lazyEncoder.value(a)
    val sprayJsValue: JsValue = a.toJson

    // A -> AST
    assert(circeToSpray(circeJson) == sprayJsValue)

    // A -> AST -> JSON
    val expectedJsonLength = jsonString.count(o ⇒ !isWhitespace(o))
    assert(CircePrinter.pretty(circeJson).length == expectedJsonLength)   // Method pretty prints compact JSON
    assert(sprayJsValue.compactPrint.replace("E+", "E")/*remove Spray's exponent sign*/.length == expectedJsonLength)
    assert(CircePrinter.pretty(circeJson).parseJson == sprayJsValue)
    //assert(circeParse(sprayJsValue.compactPrint) == Right(circeJson))  contains "none": null
    assert(circeParse(sprayJsValue.compactPrint).map(circeToSpray) == Right(sprayJsValue))
    assert(circeParse(CircePrinter.pretty(circeJson)) == circeParse(jsonString))

    // JSON -> AST
    assert(jsonString.parseJson == sprayJsValue)
    assert(circeParse(jsonString).map(circeToSpray) == Right(sprayJsValue))
    assert(circeParse(jsonString) == circeParse(sprayJsValue.compactPrint))
    assert(sprayJsValue.compactPrint.parseJson == sprayJsValue)

    // JSON -> AST -> A
    assert(jsonString.parseJson.convertTo[A] == a)
    assert(circeParse(jsonString).flatMap(_.as[A]) == Right(a))
  }
}
