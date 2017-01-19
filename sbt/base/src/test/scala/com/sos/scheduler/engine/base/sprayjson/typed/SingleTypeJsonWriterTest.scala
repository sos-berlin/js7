package com.sos.scheduler.engine.base.sprayjson.typed

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SingleTypeJsonWriterTest extends FreeSpec {

  "type field added" in {
    final case class A(int: Int)
    assertResult(JsObject("type" → JsString("A"), "int" → JsNumber(100))) {
      new SingleTypeJsonWriter("type" → JsString("A"), jsonFormat1(A)).write(A(100))
    }
  }

  "Existing duplicte type is rejected" in {
    final case class A(int: Int, `type`: String)
    intercept[RuntimeException] {
      new SingleTypeJsonWriter("type" → JsString("A"), jsonFormat2(A)).write(A(100, "X"))
    }
  }
}
