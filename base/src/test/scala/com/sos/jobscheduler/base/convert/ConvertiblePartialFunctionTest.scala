package com.sos.scheduler.engine.base.convert

import java.nio.file.{Path, Paths}
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class ConvertiblePartialFunctionTest extends FreeSpec {

  "as with default" in {
    assert(convertible("KEY" → "111").as[Int]("KEY", default = 999) == 111)
    assert(convertible[String, String]().as[Int]("KEY", default = 999) == 999)
  }

  "as" in {
    assert(convertible("KEY" → "111").as[Int]("KEY") == 111)
    intercept[NoSuchElementException] { convertible[String, String]().as[Int]("KEY") }
      .getMessage shouldEqual "key not found: KEY"
  }

  "as String" in {
    assert(convertible("KEY" → "111").as[String]("KEY") == "111")
  }

  "as Path" in {
    assert(convertible("KEY" → "111").as[Path]("KEY") == Paths.get("111"))
  }

  "as X" in {
    case class X(s: String)
    assert(convertible("KEY" → "111").as[X]("KEY")(As(X)) == X("111"))
  }

  "optionAs" in {
    assert(convertible[String, String]().optionAs[Int]("KEY") == None)
    assert(convertible[String, String]().optionAs[Int]("KEY", None) == None)
    assert(convertible[String, String]().optionAs[Int]("KEY", Some(333)) == Some(333))
    assert(convertible("KEY" → "111").optionAs[Int]("KEY") == Some(111))
    assert(convertible("KEY" → "111").optionAs[Int]("KEY", Some(333)) == Some(111))
    assert(convertible("KEY" → "111").optionAs[Int]("KEY", None) == Some(111))
  }

  private def convertible[K, V](kvs: (K, V)*) =
    new ConvertiblePartialFunction[K, V] {
      private val m = Map(kvs: _*)
      def isDefinedAt(key: K) = m isDefinedAt key
      def apply(key: K) = m(key)
    }
}
