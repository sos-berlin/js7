package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.base.sprayjson.SprayJson.JsonFormats._
import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits._
import com.sos.scheduler.engine.common.sprayutils.SprayJsonTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class SprayJsonTest extends FreeSpec {

  "Map JSON" in {
    implicit val jsonWriter = jsonFormat2(A.apply)
    val obj = A(
      111,
      Map(
        "string" → "STRING",
        "int" → 333,
        "boolean" → true,
        "null" → null,
        "array" → Vector(1, "two")))
    val json =
      """{
        "int": 111,
        "map": {
          "string": "STRING",
          "int": 333,
          "boolean": true,
          "null": null,
          "array": [1, "two"]
        }
      }""".parseJson
    assert(obj.toJson == json)
    assert(json.convertTo[A] == obj)
  }

  "map" in {
    assert((JsArray(JsNumber(1), JsNumber(2)) map { o ⇒ JsNumber(o.asJsNumber.value * 11) }) ==
            JsArray(JsNumber(11), JsNumber(22)))
  }

  "mapValues" in {
    assert((JsObject("a" → JsString("A"), "b" → JsString("B")) mapValues { o ⇒ JsString(o.asJsString.value + "-") }) ==
            JsObject("a" → JsString("A-"), "b" → JsString("B-")))
  }

  "deepMapJsObjects" in {
    def f(o: JsObject) = o mapValues {
      case v: JsString ⇒ JsString(s"/${v.value}/")
      case v ⇒ v
    }
    assert((JsObject("a" → JsObject("b" → JsString("B"  )), "c" → JsArray(JsObject("d" → JsString("D"  )))) deepMapJsObjects f) ==
            JsObject("a" → JsObject("b" → JsString("/B/")), "c" → JsArray(JsObject("d" → JsString("/D/")))))
    assert((JsArray(JsObject("d" → JsString("D"  ))) deepMapJsObjects f) ==
            JsArray(JsObject("d" → JsString("/D/"))))
  }
}

object SprayJsonTest {
  private case class A(int: Int, map: Map[String, Any])
}
