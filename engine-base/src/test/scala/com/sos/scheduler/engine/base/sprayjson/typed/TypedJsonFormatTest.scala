package com.sos.scheduler.engine.base.sprayjson.typed

import com.sos.scheduler.engine.base.sprayjson.SprayJson.lazyRootFormat
import com.sos.scheduler.engine.base.sprayjson.typed.TypedJsonFormatTest._
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

  private implicit val aJsonFormat = TypedJsonFormat[A](
    Subtype(jsonFormat0(() ⇒ A0)),
    Subtype(jsonFormat1(A1)),
    //Subtype(jsonFormat0(Alien)),  // Must not not compile
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

  "Nested TypedJsonFormat" - {
    implicit lazy val xJsonFormat: RootJsonFormat[X] =
      lazyRootFormat(
        TypedJsonFormat[X](
          Subtype[A],
          Subtype(jsonFormat2(B1))))

    "case object" in {
      val a: X = A0
      val json = """"A0"""".parseJson
      assert(a.toJson == json)
      assert(a == json.convertTo[X])
    }

    "case class 1" in {
      val a: X = A1("one")
      val json = """{ "TYPE": "A1", "string": "one" } """.parseJson
      assert(a.toJson == json)
      assert(a == json.convertTo[X])
    }

    "case class 2" in {
      val a: X = A2(2, "two")
      val json = """{ "TYPE": "A2", "int": 2, "string": "two" }""".parseJson
      assert(a.toJson == json)
      assert(a == json.convertTo[X])
    }
    "case class 1b" in {
      val b: X = B1("b one", Some(A0))
      val json = """{ "TYPE": "B1", "b1String": "b one", "recursive": "A0" }""".parseJson
      assert(b.toJson == json)
      assert(b == json.convertTo[X])
    }
  }

  "fallbackJsonWriter and recursive data structure" - {
    implicit lazy val xJsonFormat: TypedJsonFormat.AsLazy[X] =
      TypedJsonFormat.asLazy(
        TypedJsonFormat[X](
            Subtype[A],
            Subtype(jsonFormat2(B1))))

    "case object A0" in {
      val x: X = A0
      val json = """"A0"""".parseJson
      assert(x.toJson == json)
      assert(x == json.convertTo[X])
    }

    "case class 1" in {
      val a: X = A1("one")
      val json = """{ "TYPE": "A1", "string": "one" } """.parseJson
      assert(a.toJson == json)
      assert(a == json.convertTo[X])
    }

    "case class B1" in {
      val x: X = B1("b one", Some(A0))
      val json = """{ "TYPE": "B1", "b1String": "b one", "recursive": "A0" }""".parseJson
      assert(x.toJson == json)
      assert(x == json.convertTo[X])
    }

    "canSerialize" in {
      assert(!xJsonFormat.canSerialize(B0))
      assert(xJsonFormat.canSerialize(B1("", None)))
    }
  }
}

object TypedJsonFormatTest {
  private trait X

  private trait A extends X
  private case object A0 extends A
  private case class A1(string: String) extends A
  private case class A2(int: Int, string: String) extends A
  private case class Alien()

  private trait B extends X
  private case object B0 extends B
  private case class B1(b1String: String, recursive: Option[X]) extends B
}
