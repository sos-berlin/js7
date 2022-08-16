package js7.common.guice

import com.google.inject.TypeLiteral
import com.google.inject.util.Types
import izumi.reflect.Tag
import js7.base.test.OurTestSuite
import js7.common.guice.Guices.typeLiteral

final class GuicesTest extends OurTestSuite
{
  "Izumi reflect" - {
    "Array" in {
      val tag = implicitly[Tag[Array[Int]]].tag
      assert(tag.shortName == "Array")
      assert(tag.longName == "scala.Array")
      assert(tag.repr == "scala.Array[=scala.Int]")
      assert(tag.decompose.map(_.repr) == Set("scala.Array[=scala.Int]"))
      assert(tag.leftSide.repr == "scala.Array[=scala.Int]")
      assert(tag.ref.repr == "scala.Array[=scala.Int]")
      assert(tag.typeArgs.map(_.repr) == List("scala.Int"))
    }

    "GuicesTest" in {
      val tag = implicitly[Tag[GuicesTest]].tag
      assert(tag.shortName == "GuicesTest")
      assert(tag.longName == "js7.common.guice.GuicesTest")
      assert(tag.repr == "js7.common.guice.GuicesTest")
      assert(tag.decompose.map(_.repr) == Set("js7.common.guice.GuicesTest"))
      assert(tag.leftSide.repr == "js7.common.guice.GuicesTest")
      assert(tag.ref.repr == "js7.common.guice.GuicesTest")
      assert(tag.typeArgs.isEmpty)
    }

    "GuicesTest object" in {
      val tag = implicitly[Tag[GuicesTest.type]].tag
      assert(tag.shortName == "GuicesTest")
      assert(tag.longName == "js7.common.guice.GuicesTest")
      assert(tag.repr == "js7.common.guice.GuicesTest")
    }
  }

  "typeLiteral[GuicesTest]" in {
    assert(typeLiteral[GuicesTest] == TypeLiteral.get(classOf[GuicesTest]))
  }

  "typeLiteral[TypeLiteral[Int]]" in {
    assert(typeLiteral[TypeLiteral[Int]] ==
      TypeLiteral.get(
        Types.newParameterizedType(
          classOf[TypeLiteral[_]],
          classOf[java.lang.Integer])))
  }

  "typeLiteral[TypeLiteral[java.lang.Integer]]" in {
    assert(typeLiteral[TypeLiteral[java.lang.Integer]] ==
      TypeLiteral.get(
        Types.newParameterizedType(
          classOf[TypeLiteral[_]],
          classOf[java.lang.Integer])))
  }

  "typeLiteral[GuicesTest.NestedClass]" in {
    assert(typeLiteral[TypeLiteral[GuicesTest.NestedClass]] ==
      TypeLiteral.get(
        Types.newParameterizedType(
          classOf[TypeLiteral[_]],
          classOf[GuicesTest.NestedClass])))
  }
}

object GuicesTest {
  final class NestedClass
}
