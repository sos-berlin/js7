package js7.core.api

import js7.core.api.Api.quoteString
import js7.data.workflow.parser.{ExpressionParser, Parsers}
import fastparse.NoWhitespace._
import fastparse.{End, P}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ApiTest extends AnyFreeSpec
{
  "quoteString" - {
    "explicit cases" in {
      assert(quoteString("") == """""""")
      assert(quoteString("A") == """"A"""")
      assert(quoteString("\"string\"") == """"\"string\""""")
      assert(quoteString("line one\nline two\n$\n\\") == """"line one\nline two\n\$\n\\"""")
    }

    "random strings" in {
      forAll { string: String =>
        assert(parse(quoteString(string)) == Right(string))
      }
    }

    "Enumerated characters" in {
      for (i <- 0 to 0xFFFF) {
        val string = "" + i.toChar
        assert(parse(quoteString(string)) == Right(string))
      }
    }

    def parser[_: P] = ExpressionParser.quotedString ~ End

    def parse(quotedString: String) = Parsers.checkedParse(quotedString, parser(_))
  }
}
