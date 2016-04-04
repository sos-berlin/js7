package com.sos.scheduler.engine.common.convert

import com.sos.scheduler.engine.common.convert.Converters.To
import java.nio.file.{Path, Paths}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ConvertibleMultiPartialFunctionTest extends FreeSpec {

  "as with default" in {
    assert(convertible("KEY" → List("111")).as[Int]("KEY", default = 999) == 111)
    assert(convertible[String, String]().as[Int]("KEY", default = 999) == 999)
    intercept[IllegalArgumentException] { convertible("KEY" → List("111", "222")).as[Int]("KEY", default = 999) }
      .getMessage shouldEqual "For key 'KEY', only one value is possible"
  }

  "as" in {
    assert(convertible("KEY" → List("111")).as[Int]("KEY") == 111)
    intercept[NoSuchElementException] { convertible[String, String]().as[Int]("KEY") }
      .getMessage shouldEqual "key not found: KEY"
    intercept[IllegalArgumentException] { convertible("KEY" → List("111", "222")).as[Int]("KEY") }
      .getMessage shouldEqual "For key 'KEY', only one value is possible"
  }

  "as String" in {
    assert(convertible("KEY" → List("111")).as[String]("KEY") == "111")
  }

  "as Path" in {
    assert(convertible("KEY" → List("111")).as[Path]("KEY") == Paths.get("111"))
  }

  "as X" in {
    case class X(s: String)
    assert(convertible("KEY" → List("111")).as[X]("KEY")(To(X)) == X("111"))
  }

  "optionAs" in {
    assert(convertible[String, String]().optionAs[Int]("KEY") == None)
    assert(convertible("KEY" → List("111")).optionAs[Int]("KEY") == Some(111))
    intercept[IllegalArgumentException] { convertible("KEY" → List("111", "222")).optionAs[Int]("KEY") }
      .getMessage shouldEqual "For key 'KEY', only one value is possible"
  }

  "seqAs" in {
    assert(convertible[String, String]().seqAs[Int]("KEY") == Nil)
    assert(convertible("KEY" → List("111")).seqAs[Int]("KEY") == List(111))
    assert(convertible("KEY" → List("111", "222")).seqAs[Int]("KEY") == List(111, 222))
  }

  private def convertible[K, V](kvs: (K, Seq[V])*) =
    new PartialFunction[K, Seq[V]] with ConvertibleMultiPartialFunction[K, V] {
      private val m = Map(kvs: _*)
      def isDefinedAt(key: K) = m isDefinedAt key
      def apply(key: K) = m(key)
    }
}
