package js7.base.convert

import js7.base.convert.ConvertiblePartialFunctions.ImplicitConvertablePF
import js7.base.test.OurTestSuite
import org.scalatest.matchers.should.Matchers.*

/**
  * @author Joacim Zschimmer
  */
final class ConvertiblePartialFunctionsTest extends OurTestSuite:

  private val pf: PartialFunction[String, String] = Map("KEY" -> "111", "other" -> "000")

  "as" in:
    assert(pf.as[Int]("KEY") == 111)
    intercept[NoSuchElementException] { pf.as[Int]("unknown") }
      .getMessage shouldEqual "key not found: unknown"
    intercept[IllegalArgumentException] { pf.as[Boolean]("KEY") }
      .getMessage shouldEqual """Invalid key 'KEY': java.lang.IllegalArgumentException: Boolean value true or false expected, not: 111"""

  "as with default" in:
    assert(pf.as[Int]("KEY") == 111)
    assert(pf.as[Int]("unknown", default = 222) == 222)

  "optionAs" in:
    assert(pf.optionAs[Int]("KEY") == Some(111))
    assert(pf.optionAs[Int]("unknown") == None)
