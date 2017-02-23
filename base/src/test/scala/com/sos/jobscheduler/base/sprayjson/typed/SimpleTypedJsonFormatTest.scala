package com.sos.scheduler.engine.base.sprayjson.typed

import com.sos.scheduler.engine.base.sprayjson.typed.SimpleTypedJsonFormatTest._
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class SimpleTypedJsonFormatTest extends FreeSpec {

  private implicit val aJsonFormat = AJsonFormat
  private val xJson = """{
      "TYPE": "TEST-A",
      "x": 7
    }"""
  private val yJson = """{
      "TYPE": "TEST-A",
      "y": true
    }"""
  private val zJson = """{
      "TYPE": "TEST-A",
      "y": true
    }"""
  private val otherJson = """{
      "TYPE": "OTHER"
    }"""


  "SimpleTypedJsonFormat" in {
    check(X(7), xJson)
    check(Y(true), yJson)
  }

  "canSerialize" in {
    assert(AJsonFormat canSerialize X(7))
    assert(AJsonFormat canSerialize Y(true))
    assert(!(AJsonFormat canSerialize Z("")))
  }

  "canDeserialize" in {
    assert(AJsonFormat canDeserialize xJson.parseJson.asJsObject)
    assert(AJsonFormat canDeserialize yJson.parseJson.asJsObject)
    assert(AJsonFormat canDeserialize zJson.parseJson.asJsObject) // Not really !!!
    assert(!(AJsonFormat canDeserialize otherJson.parseJson.asJsObject))
  }

  private def check(a: A, json: String): Unit = {
    assert(a.toJson == json.parseJson)
    assert(json.parseJson.convertTo[A] == a)
  }
}

object SimpleTypedJsonFormatTest {
  private sealed trait A
  private case class X(x: Int) extends A
  private case class Y(y: Boolean) extends A
  private case class Z(z: String) extends A
  private implicit val xJsonFormat = jsonFormat1(X)
  private implicit val yJsonFormat = jsonFormat1(Y)

  private object AJsonFormat extends SimpleTypedJsonFormat[A] {
    protected def typeName = "TEST-A"

    protected def typelessWrite(a: A) = a match {
      case x: X ⇒ x.toJson.asJsObject
      case y: Y ⇒ y.toJson.asJsObject
    }

    def read(jsValue: JsValue) =
      if (jsValue.asJsObject.fields contains "x")
        jsValue.convertTo[X]
      else
        jsValue.convertTo[Y]

    protected def subclasses = Set(classOf[X], classOf[Y])
  }
}
