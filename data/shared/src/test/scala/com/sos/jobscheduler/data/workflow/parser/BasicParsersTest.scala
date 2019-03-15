package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.workflow.parser.BasicParsers._
import com.sos.jobscheduler.data.workflow.parser.Parsers.checkedParse
import fastparse.NoWhitespace._
import fastparse._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class BasicParsersTest extends FreeSpec
{
  "checkedParse" - {
    "End" in {
      assert(checkedParse("", End(_)) == Valid(()))
    }

    "String" in {
      def p[_: P]: P[Unit] = P("x")
      assert(checkedParse("x", p(_)) == Valid(()))
      assert(checkedParse("y", p(_)) == Invalid(Problem("""Expected "x":1:1, found "y"""")))
    }
  }

  "int" in {
    assert(checkedParse("123", int(_)) == Valid(123))
  }

  "identifier" in {
    assert(checkedParse("name", identifier(_)) == Valid("name"))
    assert(checkedParse("name123", identifier(_)) == Valid("name123"))
    assert(checkedParse("1name", identifier(_)).isInvalid/*TODO error message?*/)
  }

  "String in single-quotes" - {
    "Empty single-quote string is not allowed" in {
      assert(checkedParse("''", quotedString(_)) ==
        Invalid(Problem("Expected properly terminated ''-quoted string without non-printable characters (except \\r or \\n):1:3, found \"\"")))
    }

    "Backquote + single-quote cannot be used" in {
      assert(checkedParse("""'\''""", quotedString(_)) == Invalid(Problem("""Expected end-of-input:1:4, found "'"""")))
    }

    "Newline \\n (multi-line) is allowed" in {
      assert(checkedParse("'\n'", quotedString(_)) == Valid("\n"))
    }

    "Multi-line" in {
      val s =
        """test '
          |  first line
          |    indented line
          |  third line
          |fourth line
          |'""".stripMargin
      def p[_: P] = keyword("test") ~ w ~ quotedString
      assert(checkedParse(s, p(_)) == Valid(
         """
           |  first line
           |    indented line
           |  third line
           |fourth line
           |""".stripMargin))
    }

    "Multiple quoted" in {
      assert(checkedParse("''abc''", quotedString(_)) == Valid("abc"))
      assert(checkedParse("''ab'c''", quotedString(_)) == Valid("ab'c"))
      assert(checkedParse("''ab'c'd''", quotedString(_)) == Valid("ab'c'd"))
      assert(checkedParse("'''ab''c'd'''", quotedString(_)) == Valid("ab''c'd"))
      assert(checkedParse("''''ab'''c'''d''''", quotedString(_)) == Valid("ab'''c'''d"))
      assert(checkedParse("'''''abc'''''", quotedString(_)) == Valid("abc"))
      assert(checkedParse("''''''abc''''''", quotedString(_)).isInvalid) // == Invalid(Problem("More than 5 '-quotes are not supported")))
    }

    "Delimiter at start or end" in {
      // FIXME quotes at string start or end does not work!
      pending
    }

    "CF+LF is normalized to LF" in {
      assert(checkedParse("'AAA\r\nBBB\r\n'", quotedString(_)) == Valid("AAA\nBBB\n"))
    }

    "Control character is not allowed" in {
      assert(checkedParse("'\t'", quotedString(_)) == Invalid(Problem(
        """Expected properly terminated '-quoted string without non-printable characters (except \r or \n):1:2, found "\t'"""")))
    }

    "Valid" in {
      assert(checkedParse("'abc'", quotedString(_)) == Valid("abc"))
      assert(checkedParse("'ö'", quotedString(_)) == Valid("ö"))
      assert(checkedParse("""'x\n'""", quotedString(_)) == Valid("""x\n"""))
      assert(checkedParse("""'x$y'""", quotedString(_)) == Valid("x$y"))
    }
  }

  "String in double-quotes" - {
    "Unknown backslash combination" in {
      assert(checkedParse(""""x\x"""", quotedString(_)) == Invalid(Problem(
        """Expected blackslash (\) and one of the following characters: [\"nt]:1:5, found "\""""")))
    }

    "String interpolation is not supported" in {
      // TODO String interpolation
      assert(checkedParse(""""x$variable"""", quotedString(_)) == Invalid(Problem(
        """Expected "-quoted string without variable interpolation via '$' (reserved syntax, consider using single quotes (')):1:13, found """"")))
    }

    "Valid" in {
      assert(checkedParse("""""""", quotedString(_)) == Valid(""))
      assert(checkedParse(""""x"""", quotedString(_)) == Valid("x"))
      assert(checkedParse(""""ö"""", quotedString(_)) == Valid("ö"))
      assert(checkedParse(""""abc"""", quotedString(_)) == Valid("abc"))
      assert(checkedParse(""""ab\"c"""", quotedString(_)) == Valid("""ab"c"""))
      assert(checkedParse(""""ab\\c"""", quotedString(_)) == Valid("""ab\c"""))
      assert(checkedParse(""""ab\nc"""", quotedString(_)) == Valid("ab\nc"))
      assert(checkedParse(""""ab\tc"""", quotedString(_)) == Valid("ab\tc"))
      assert(checkedParse(""""ab\n\"\\c"""", quotedString(_)) == Valid("ab\n\"\\c"))
    }
  }

  "specificKeyValue" in {
    def parser[_: P] = specificKeyValue("key", int)
    assert(checkedParse("key=123", parser(_)) == Valid(123))
  }

  "specificKeyValue 2" in {
    def myKeyInt(name: String)(implicit ctx: P[_]): P[Int] =
      P(name ~ w ~ "=" ~ w ~/ int)
    def parser[_: P] = myKeyInt("key")
    assert(checkedParse("key=123", parser(_)) == Valid(123))
  }

  "commaSequence" - {
    def parser[_: P] = commaSequence(identifier)

    "empty" in {
      assert(checkedParse("", parser(_)) == Valid(Nil))
    }

    "single" in {
      assert(checkedParse("a", parser(_)) == Valid("a" :: Nil))
    }

    "multiple" in {
      assert(checkedParse("a, bc , def ,  ghij", parser(_)) == Valid("a" :: "bc" :: "def" :: "ghij" :: Nil))
    }

    "with specificKeyValue" in {
      def parser[_: P] = commaSequence(specificKeyValue("key", int))
      assert(checkedParse("", parser(_)) == Valid(Nil))
      assert(checkedParse("key=1", parser(_)) == Valid(1 :: Nil))
      assert(checkedParse("key=1, key=2", parser(_)) == Valid(1 :: 2 :: Nil))
    }
  }

  "keyValues" - {
    def kvP[_: P] = keyValues(keyValue("number", int) | keyValue("string", quotedString))

    "empty" in {
      assert(checkedParse("", kvP(_)) == Valid(KeyToValue(Map.empty)))
    }

    "single" in {
      assert(checkedParse("number=123", kvP(_)) == Valid(KeyToValue(Map("number" -> 123))))
    }

    "Duplicate keys (1)" in {
      assert(checkedParse("""number=1, number=2""", kvP(_))
        == Invalid(Problem("""Expected unique keywords (duplicates: number):1:19, found """"")))
    }

    "Duplicate keys (2)" in {
      assert(checkedParse("""string="A", string="B"""", kvP(_))
        == Invalid(Problem("""Expected unique keywords (duplicates: string):1:23, found """"")))
    }

    "Valid" in {
      assert(checkedParse("""string="STRING", number=7""", kvP(_))
        == Valid(KeyToValue(Map("string" -> "STRING", "number" -> 7))))
    }

    "apply" in {
      def p[_: P]: P[(Int, String)] = kvP.flatMap(keyToValue =>
        for {
          number <- keyToValue[java.lang.Integer]("number")
          string <- keyToValue[String]("string")
        } yield (number, string))
      assert(checkedParse("""string="STRING", number=7""", p(_)) == Valid((7, "STRING")))
      assert(checkedParse("""string="STRING", number="7"""", p(_)).isInvalid)
    }
  }

  "KeyToValue" - {
    val keyToValue = KeyToValue(Map("string" -> "STRING", "string2" -> "STRING2", "number" -> 7))

    // checkedParse("") in following tests due to Parser.flatMap

    "apply failed" in {
      def p[_: P] = keyToValue[String]("MISSING")
      assert(checkedParse("", p(_)) == Invalid(Problem("""Expected keyword MISSING=:1:1, found """"")))
    }

    "apply" in {
      def p[_: P] = keyToValue[String]("string")
      assert(checkedParse("", p(_)) == Valid("STRING"))
    }

    "apply with default" in {
      assert(checkedParse("", keyToValue("string", "DEFAULT")(_)) == Valid("STRING"))
      assert(checkedParse("", keyToValue("MISSING", "DEFAULT")(_)) == Valid("DEFAULT"))
    }

    "oneOf" in {
      assert(checkedParse("", keyToValue.oneOf[String](Set("string", "string2"))(_))
        == Invalid(Problem("""Expected non-contradicting keywords: string; string2:1:1, found """"")))
      assert(checkedParse("", keyToValue.oneOf[String](Set("string", "x"))(_)) == Valid("string" -> "STRING"))
      assert(checkedParse("", keyToValue.oneOf[String](Set("string2", "x"))(_)) == Valid("string2" -> "STRING2"))
      assert(checkedParse("", keyToValue.oneOf[String](Set("y", "x"))(_)) == Invalid(Problem("""Expected keywords y=, x=:1:1, found """"")))
    }

    "oneOfOr" in {
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("string", "string2"), "DEFAULT")(_))
        == Invalid(Problem("""Expected non-contradicting keywords: string; string2:1:1, found """"")))
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("string", "x"), "DEFAULT")(_)) == Valid("STRING"))
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("string2", "x"), "DEFAULT")(_)) == Valid("STRING2"))
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("y", "x"), "DEFAULT")(_)) == Valid("DEFAULT"))
    }

    "noneOrOneOf" in {
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("string", "string2"))(_)) ==
        Invalid(Problem("""Expected non-contradicting keywords: string; string2:1:1, found """"")))
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("string", "x"))(_)) == Valid(Some("string" -> "STRING")))
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("string2", "x"))(_)) == Valid(Some("string2" -> "STRING2")))
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("y", "x"))(_)) == Valid(None))
    }
  }
}
