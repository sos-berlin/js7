package js7.core.api

import fastparse.NoWhitespace._
import fastparse.{End, P}
import js7.core.api.Api.quoteString
import js7.data.parser.Parsers
import js7.data.value.expression.ExpressionParser
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks._

/**
  * @author Joacim Zschimmer
  */
final class ApiTest extends AnyFreeSpec
{
  "quoteString" - {
    "explicit cases" in {
      assert(quoteString("") == """""""")
      assert(quoteString("A") == "'A'")
      assert(quoteString("\"string\"") == "'\"string\"'")
      assert(quoteString("line one\nline two\n$\n\\") == "'line one\nline two\n$\n\\'")
    }

    "random strings" in {
      forAll { string: String =>
        val cleansedString = string.map(c => if ((c & 0xffff) >= 0x20 || c == '\t' || c == '\r'|| c == '\n') c else '¿')
        assert(parse(quoteString(cleansedString)) == Right(cleansedString))
      }
    }

    "Enumerated characters" in {
      for (i <- (0x20 to 0xFFFF) ++ Seq('\t'.toInt, '\r'.toInt, '\n'.toInt)) {
        val string = "" + i.toChar
        assert(parse(quoteString(string)) == Right(string), "0x" + i.toHexString)
      }
    }

    def parser[_: P] = ExpressionParser.quotedString ~ End

    def parse(quotedString: String) = Parsers.checkedParse(quotedString, parser(_))
  }
}
