package js7.base.circeutils.typed

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.TypedJsonCodec.*
import js7.base.circeutils.typed.TypedJsonCodecTest.*
import js7.base.problem.Problem
import js7.base.test.Test
import js7.tester.CirceJsonTester.testJson
import org.scalatest.matchers.should.Matchers.*

/**
  * @author Joacim Zschimmer
  */
final class TypedJsonCodecTest extends Test
{
  "encode and decode" in {
    testJson[A](A0     , json"""{ "TYPE": "A0" }""")
    testJson[A](A1(7)  , json"""{ "TYPE": "A1", "int": 7 }""")
    testJson[A](A2("X"), json"""{ "TYPE": "A2", "string": "X" }""")
  }

  "encode unknown subclass" in {
    intercept[UnknownClassForJsonException] {
      (NotRegistered(1): A).asJson(AJsonCodec)
    }.getMessage should include (
      "Class 'TypedJsonCodecTest.NotRegistered' is not registered in " +
        "js7.base.circeutils.typed.TypedJsonCodecTest.AJsonCodec: " +
        "TypedJsonCodec[TypedJsonCodecTest.A]")
  }

  "decode unknown subclass" in {
    assert(json"""{ "TYPE": "UNKNOWN" }""".as[A].toChecked == Left(Problem(
      """JSON DecodingFailure at : Unexpected JSON {"TYPE": "UNKNOWN", ...} for """ +
        "js7.base.circeutils.typed.TypedJsonCodecTest.AJsonCodec: " +
        "TypedJsonCodec[TypedJsonCodecTest.A]")))
  }

  "Nested TypedJsonCodec" in {
    testJson[A](AA1(7), json"""{ "TYPE": "AA1", "int": 7 }""")
  }

  "Union" in {
    implicit val ab: TypedJsonCodec[Any] = AJsonCodec | BJsonCodec
    testJson[Any](A0, json"""{ "TYPE": "A0" }""")
    testJson[Any](B0, json"""{ "TYPE": "B0" }""")
  }

  "classes" in {
    assert(AJsonCodec.classes[A] == Set(A0.getClass, classOf[A1], classOf[A2], classOf[AA1]))
    assert(AJsonCodec.classes[AA] == Set(classOf[AA1]))
  }

  "classToName" in {
    assert(AJsonCodec.classToName(A0.getClass) == "A0")
    intercept[NoSuchElementException] {
      assert(AJsonCodec.classToName(classOf[A]) == "A")
    }
  }

  "typeName" in {
    assert(AJsonCodec.typeName(A0) == "A0")
    assert(AJsonCodec.typeName(A1(1)) == "A1")
    intercept[NoSuchElementException] {
      AJsonCodec.typeName(Other)
    }
  }

  "isOfType, isOfType" in {
    val a1Json = json"""{ "TYPE":  "A1"}"""
    val xJson = json"""{ "TYPE":  "X"}"""
    assert(a1Json.isOfType[A, A1])
    assert(!xJson.isOfType[A, A1])
    assert(AJsonCodec.isOfType[A1](a1Json))
    assert(!AJsonCodec.isOfType[A1](xJson))
  }

  "jsonToClass" in {
    val a1Json = json"""{ "TYPE":  "A1"}"""
    val xJson = json"""{ "TYPE":  "X"}"""
    assert(a1Json.toClass[A] == Some(classOf[A1]))
    assert(xJson.toClass[A] == None)
  }
}

object TypedJsonCodecTest {
  sealed trait A
  case object A0 extends A
  final case class A1(int: Int) extends A
  final case class A2(string: String) extends A
  final case class NotRegistered(int: Int) extends A

  sealed trait AA extends A
  final case class AA1(int: Int) extends AA

  object Other extends A

  private implicit val AAJsonCodec: TypedJsonCodec[AA] = TypedJsonCodec[AA](
    Subtype(deriveCodec[AA1]))

  private implicit val AJsonCodec: TypedJsonCodec[A] = TypedJsonCodec[A](
    Subtype(A0),
    Subtype(deriveCodec[A1]),
    Subtype(deriveCodec[A2]),
    Subtype[AA])

  sealed trait B
  case object B0 extends B

  private implicit val BJsonCodec: TypedJsonCodec[B] = TypedJsonCodec[B](
    Subtype(B0))
}
