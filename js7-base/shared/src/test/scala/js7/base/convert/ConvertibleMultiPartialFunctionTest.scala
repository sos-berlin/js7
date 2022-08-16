package js7.base.convert

import js7.base.test.OurTestSuite
import org.scalatest.matchers.should.Matchers.*

/**
  * @author Joacim Zschimmer
  */
final class ConvertibleMultiPartialFunctionTest extends OurTestSuite {

  "as with default" in {
    assert(convertible("KEY" -> List("111")).as[Int]("KEY", default = 999) == 111)
    assert(convertible[String, String]().as[Int]("KEY", default = 999) == 999)
    intercept[IllegalArgumentException] { convertible("KEY" -> List("111", "222")).as[Int]("KEY", default = 999) }
      .getMessage shouldEqual "Only one value is allowed for key 'KEY'"
  }

  "as" in {
    assert(convertible("KEY" -> List("111")).as[Int]("KEY") == 111)
    intercept[NoSuchElementException] { convertible[String, String]().as[Int]("KEY") }
      .getMessage shouldEqual "key not found: KEY"
    intercept[IllegalArgumentException] { convertible("KEY" -> List("111", "222")).as[Int]("KEY") }
      .getMessage shouldEqual "Only one value is allowed for key 'KEY'"
  }

  "as String" in {
    assert(convertible("KEY" -> List("111")).as[String]("KEY") == "111")
  }

  "as X" in {
    case class X(s: String)
    assert(convertible("KEY" -> List("111")).as[X]("KEY")(As(X(_))) == X("111"))
  }

  "optionAs" in {
    assert(convertible[String, String]().optionAs[Int]("KEY") == None)
    assert(convertible[String, String]().optionAs[Int]("KEY", None) == None)
    assert(convertible[String, String]().optionAs[Int]("KEY", Some(333)) == Some(333))
    assert(convertible("KEY" -> List("111")).optionAs[Int]("KEY") == Some(111))
    assert(convertible("KEY" -> List("111")).optionAs[Int]("KEY", None) == Some(111))
    assert(convertible("KEY" -> List("111")).optionAs[Int]("KEY", Some(333)) == Some(111))
    intercept[IllegalArgumentException] { convertible("KEY" -> List("111", "222")).optionAs[Int]("KEY") }
      .getMessage shouldEqual "Only one value is allowed for key 'KEY'"
  }

  "seqAs" in {
    assert(convertible[String, String]().seqAs[Int]("KEY") == Nil)
    assert(convertible("KEY" -> List("111")).seqAs[Int]("KEY") == List(111))
    assert(convertible("KEY" -> List("111", "222")).seqAs[Int]("KEY") == List(111, 222))
  }

  private def convertible[K, V](kvs: (K, Seq[V])*) =
    new PartialFunction[K, Seq[V]] with ConvertibleMultiPartialFunction[K, V] {
      private val m = Map(kvs*)
      def isDefinedAt(key: K) = m isDefinedAt key
      def apply(key: K) = m(key)
    }
}
