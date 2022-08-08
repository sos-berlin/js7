package js7.base.convert

import js7.base.convert.ConvertiblePartialFunction.MissingConfigurationKeyProblem
import js7.base.problem.Problem
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.*

/**
  * @author Joacim Zschimmer
  */
final class ConvertiblePartialFunctionTest extends AnyFreeSpec {

  "as with default" in {
    assert(convertible("KEY" -> "111").as[Int]("KEY", default = 999) == 111)
    assert(convertible[String, String]().as[Int]("KEY", default = 999) == 999)
  }

  "as" in {
    assert(convertible("KEY" -> "111").as[Int]("KEY") == 111)
    intercept[NoSuchElementException] { convertible[String, String]().as[Int]("KEY") }
      .getMessage shouldEqual "key not found: KEY"
  }

  "as String" in {
    assert(convertible("KEY" -> "111").as[String]("KEY") == "111")
  }

  "as X" in {
    case class X(s: String)
    assert(convertible("KEY" -> "111").as[X]("KEY")(As(X(_))) == X("111"))
  }

  "checkedAs" in {
    assert(convertible[String, String]().checkedAs[Int]("KEY") == Left(MissingConfigurationKeyProblem("KEY")))
    convertible("KEY" -> "111").checkedAs[Int]("KEY")
    intercept[IllegalArgumentException] {
      // TODO checked[Option]As methods should not throw
      assert(convertible("KEY" -> "TEXT").checkedAs[Int]("KEY") == Left(Problem("???")))
    }
    assert(convertible[String, String]().checkedAs[Int]("KEY", Some(222)) == Right(222))
  }

  "optionAs" in {
    assert(convertible[String, String]().optionAs[Int]("KEY") == None)
    assert(convertible[String, String]().optionAs[Int]("KEY", None) == None)
    assert(convertible[String, String]().optionAs[Int]("KEY", Some(333)) == Some(333))
    assert(convertible("KEY" -> "111").optionAs[Int]("KEY") == Some(111))
    assert(convertible("KEY" -> "111").optionAs[Int]("KEY", Some(333)) == Some(111))
    assert(convertible("KEY" -> "111").optionAs[Int]("KEY", None) == Some(111))
  }

  private def convertible[K, V](kvs: (K, V)*) =
    new ConvertiblePartialFunction[K, V] {
      private val m = Map(kvs*)
      def isDefinedAt(key: K) = m isDefinedAt key
      def apply(key: K) = m(key)
    }
}
