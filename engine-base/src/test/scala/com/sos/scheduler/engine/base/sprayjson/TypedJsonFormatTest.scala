package com.sos.scheduler.engine.base.sprayjson

import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormat._
import com.sos.scheduler.engine.base.sprayjson.TypedJsonFormatTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TypedJsonFormatTest extends FreeSpec {

  private implicit val typedJsonFormat = TypedJsonFormat[A](
    Subtype(jsonFormat0(() ⇒ A0)),
    Subtype(jsonFormat1(A1)),
    Subtype(jsonFormat2(A2)))

  "case object" in {
    val a: A = A0
    val json = """"A0"""".parseJson
    assert(a.toJson == json)
    assert(a == json.convertTo[A])
  }

  "case class 1" in {
    val a: A = A1("one")
    val json = """{ "TYPE": "A1", "string": "one" } """.parseJson
    assert(a.toJson == json)
    assert(a == json.convertTo[A])
  }

  "case class 2" in {
    val a: A = A2(2, "two")
    val json = """{ "TYPE": "A2", "int": 2, "string": "two" }""".parseJson
    assert(a.toJson == json)
    assert(a == json.convertTo[A])
  }
}

object TypedJsonFormatTest {
  private sealed trait A
  private case object A0 extends A
  private case class A1(string: String) extends A
  private case class A2(int: Int, string: String) extends A
}
