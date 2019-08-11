package com.sos.jobscheduler.core.api

import com.sos.jobscheduler.core.api.Api.quoteString
import com.sos.jobscheduler.data.workflow.parser.{ExpressionParser, Parsers}
import fastparse.NoWhitespace._
import fastparse.{End, P}
import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
  * @author Joacim Zschimmer
  */
final class ApiTest extends FreeSpec with GeneratorDrivenPropertyChecks
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
