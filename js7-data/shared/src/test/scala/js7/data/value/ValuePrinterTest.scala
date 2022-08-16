package js7.data.value

import js7.base.test.Test
import js7.data.value.ValuePrinter.quoteString
import js7.data.value.expression.ExpressionParser.parseQuotedString
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks.forAll

final class ValuePrinterTest extends Test
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
    forAll { (string: String) =>
      val cleansedString = string.map(c => if ((c & 0xffff) >= 0x20 || c == '\t' || c == '\r'|| c == '\n') c else '¿')
      assert(parseQuotedString(quoteString(cleansedString)) == Right(cleansedString))
    }
  }

  "Enumerated characters" in {
    for (i <- (0x20 to 0xFFFF) ++ Seq('\t'.toInt, '\r'.toInt, '\n'.toInt)) {
      val string = "" + i.toChar
      assert(parseQuotedString(quoteString(string)) == Right(string), "0x" + i.toHexString)
    }
  }

  private def q(string: String) =
    "»" + quoteString(string) + "«"
}
