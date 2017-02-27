package com.sos.jobscheduler.base.sprayjson.typed

import com.sos.jobscheduler.base.sprayjson.typed.NamedJsonFormat._
import com.sos.jobscheduler.base.sprayjson.typed.NamedJsonFormatTest._
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class NamedJsonFormatTest extends FreeSpec {

  private val aJsonFormat = jsonFormat2(A) withTypeName "Test-A"
  private val typedJsonFormat = TypedJsonFormat[A](Subtype(aJsonFormat))
  private val a = A(7, "STRING")

  "NamedJsonFormat" in {
    check(a, AJson)(aJsonFormat)
    check(a, ATypedJson)(typedJsonFormat)
  }

  private def check(a: A, jsValue: JsValue)(implicit jsonFormat: RootJsonFormat[A]): Unit = {
    assert(a.toJson == jsValue)
    assert(jsValue.convertTo[A] == a)
  }
}

private object NamedJsonFormatTest {
  private final case class A(i: Int, string: String)

  private val AJson = JsObject(
    "i" → JsNumber(7),
    "string" → JsString("STRING"))

  private val ATypedJson = JsObject(
    "TYPE" → JsString("Test-A"),
    "i" → JsNumber(7),
    "string" → JsString("STRING"))
}
