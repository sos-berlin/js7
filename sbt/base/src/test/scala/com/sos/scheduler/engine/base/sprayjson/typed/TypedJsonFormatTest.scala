package com.sos.scheduler.engine.base.sprayjson.typed

import com.sos.scheduler.engine.base.sprayjson.SprayJson.lazyRootFormat
import com.sos.scheduler.engine.base.sprayjson.typed.TypedJsonFormatTest._
import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final class TypedJsonFormatTest extends FreeSpec {

  private implicit val aJsonFormat = TypedJsonFormat[A](shortenTypeOnlyValue = true)(
    Subtype(jsonFormat0(() ⇒ A0)),
    Subtype(jsonFormat1(A1)),
    //Subtype(jsonFormat0(Alien)),  // Must not not compile
    Subtype(jsonFormat2(A2)))

  "typeNameToClass" in {
    assert(aJsonFormat.typeNameToClass == Map(
      "A" → classOf[A],
      "A0" → A0.getClass,
      "A1" → classOf[A1],
      "A2" → classOf[A2]))
  }

  "subtypeNames in original order" in {
    assert(aJsonFormat.subtypeNames == List("A0", "A1", "A2"))
  }

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

  "classJsonFormat" - {
    type C = Class[_ <: A]
    implicit val jsonFormat: JsonFormat[C] = aJsonFormat.classJsonFormat

    "case object" in {
      val a: C = A0.getClass.asInstanceOf[C]
      val json = JsString("A0")
      assert(jsonFormat.write(a) == json)
      // Cannot find JsonWriter or JsonFormat type class for Class[_$1]: assert(a.toJson == json)
      assert(json.convertTo[C] == a)
    }

    "case class" in {
      val a: C = classOf[A1]
      val json = JsString("A1")
      assert(jsonFormat.write(a) == json)
      // Cannot find JsonWriter or JsonFormat type class for Class[_$1]: assert(a.toJson == json)
      assert(json.convertTo[C] == a)
    }
  }

  "Nested TypedJsonFormat" - {
    implicit lazy val xJsonFormat: RootJsonFormat[X] =
      lazyRootFormat(
        TypedJsonFormat[X](shortenTypeOnlyValue = true)(
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
        TypedJsonFormat[X](shortenTypeOnlyValue = true)(
          Subtype[A],
          Subtype(jsonFormat2(B1))))

    "typeNameToClass" in {
      assert(xJsonFormat.typeNameToClass == Map(
        "X" → classOf[X],
        "A" → classOf[A],
        "A0" → A0.getClass,
        "A1" → classOf[A1],
        "A2" → classOf[A2],
        "B1" → classOf[B1]))
    }

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
