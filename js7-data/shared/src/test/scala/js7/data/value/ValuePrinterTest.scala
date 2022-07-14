package js7.data.value

import fastparse.NoWhitespace.*
import fastparse.{End, P}
import js7.data.parser.Parsers
import js7.data.value.ValuePrinter.quoteString
import js7.data.value.expression.ExpressionParser
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks.forAll

final class ValuePrinterTest extends AnyFreeSpec
{
  "quoteString" in {
    assert(q("") == """»""«""")
    assert(q("A") == "»'A'«")
    assert(q("\"") == """»'"'«""")
    assert(q("\r") == """»"\r"«""")
    assert(q("\n") == """»'
        |'«""".stripMargin)
    assert(q("'") == """»"'"«""")
    assert(q("''") == """»"''"«""")
  }

  "quoteString - random values" in {
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

  private def q(string: String) =
    "»" + quoteString(string) + "«"

  def parser[x: P] =
    ExpressionParser.quotedString ~ End

  def parse(quotedString: String) =
    Parsers.checkedParse(quotedString, parser(_))
}
