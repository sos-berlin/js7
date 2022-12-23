package js7.data.parser

import fastparse.*
import fastparse.NoWhitespace.*
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.withStringBuilder
import js7.data.parser.BasicPrinter.appendIdentifier
import js7.data.parser.FastparseBasicParsers.*
import js7.data.parser.FastparseParsers.checkedParse

/**
  * @author Joacim Zschimmer
  */
final class FastparseBasicParsersTest extends OurTestSuite
{
  "checkedParse" - {
    "End" in {
      assert(checkedParse("", End(_)) == Right(()))
    }

    "String" in {
      def p[x: P]: P[Unit] = P("x")
      assert(checkedParse("x", p(_)) == Right(()))
      assert(checkedParse("y", p(_)) == Left(Problem("""Expected "x":1:1, found "y"""")))
    }
  }

  "int" in {
    assert(checkedParse("123", int(_)) == Right(123))
  }

  "identifier" in {
    assertIdentifier("name", Right("name"))
    assertIdentifier("name123", Right("name123"))
    assertIdentifier("a.b", Left(Problem("Expected end-of-input:1:2, found \".b\"")))
    assertIdentifier("`a.b`", Right("a.b"))
    assertIdentifier("`a``b`", Right("a`b"))
    assertIdentifier("`a````b`", Right("a``b"))
    assertIdentifier("```a````b```", Right("`a``b`"))
    assertIdentifier("1name", Left(Problem("Expected (simpleIdentifier | backtickIdentifier):1:1, found \"1name\"")))

    def assertIdentifier[T, V, R](expr: String, expected: Checked[String]) = {
      val result = checkedParse(expr, identifier(_))
      assert(result == expected)
      for (r <- result) {
        assert(withStringBuilder(appendIdentifier(_, r)) == expr)
      }
    }
  }

  "String in single-quotes" - {
    "Empty single-quote string is not allowed" in {
      assert(checkedParse("''", quotedString(_)) ==
        Left(Problem("""Expected properly terminated ''…''-quoted string without non-printable characters (except \t, \r and \n) — or use "" (not '') for the empty string:1:3, found """"")))
    }

    "Backquote + single-quote cannot be used" in {
      assert(checkedParse("""'\''""", quotedString(_)) == Left(Problem("""Expected end-of-input:1:4, found "'"""")))
    }

    "Newline \\n (multi-line) is allowed" in {
      assert(checkedParse("'\n'", quotedString(_)) == Right("\n"))
    }

    "Multi-line" in {
      val s =
        """test '
          |  first line
          |    indented line
          |  third line
          |fourth line
          |'""".stripMargin
      def p[x: P] = keyword("test") ~ w ~ quotedString
      assert(checkedParse(s, p(_)) == Right(
         """
           |  first line
           |    indented line
           |  third line
           |fourth line
           |""".stripMargin))
    }

    "Multiple quoted" in {
      assert(checkedParse("''abc''", quotedString(_)) == Right("abc"))
      assert(checkedParse("''ab'c''", quotedString(_)) == Right("ab'c"))
      assert(checkedParse("''ab'c'd''", quotedString(_)) == Right("ab'c'd"))
      assert(checkedParse("'''ab''c'd'''", quotedString(_)) == Right("ab''c'd"))
      assert(checkedParse("''''ab'''c'''d''''", quotedString(_)) == Right("ab'''c'''d"))
      assert(checkedParse("'''''abc'''''", quotedString(_)) == Right("abc"))
      assert(checkedParse("''''''abc''''''", quotedString(_)).isLeft) // == Left(Problem("More than 5 '-quotes are not supported")))
    }

    "Delimiter at start or end" in {
      // FIXME quotes at string start or end does not work!
      pending
    }

    "CF+LF is normalized to LF" in {
      assert(checkedParse("'AAA\r\nBBB\r\n'", quotedString(_)) == Right("AAA\nBBB\n"))
    }

    //"Control character is not allowed" in {
    //  assert(checkedParse("'\t'", quotedString(_)) == Left(Problem(
    //    """Expected properly terminated '…'-quoted string without non-printable characters (except \r or \n):1:2, found "\t'"""")))
    //}

    "Valid" in {
      assert(checkedParse("'abc'", quotedString(_)) == Right("abc"))
      assert(checkedParse("'ö'", quotedString(_)) == Right("ö"))
      assert(checkedParse("""'x\n'""", quotedString(_)) == Right("""x\n"""))
      assert(checkedParse("""'x$y'""", quotedString(_)) == Right("x$y"))
    }
  }

  "String in double-quotes" - {
    "Unknown backslash combination" in {
      assert(checkedParse(""""x\x"""", quotedString(_)) == Left(Problem(
        """Expected blackslash (\) and one of the following characters: [\"trn$]:1:5, found "\""""")))
    }

    "String interpolation is not supported" in {
      // TODO String interpolation
      assert(checkedParse(""""$variable"""", quotedString(_)) == Left(Problem(
        """Expected properly terminated "…"-quoted string:1:2, found "$variable\""""")))
      assert(checkedParse(""""x$variable"""", quotedString(_)) == Left(Problem(
        """Expected properly terminated "…"-quoted string:1:3, found "$variable\""""")))
    }

    "Valid" in {
      assert(checkedParse("""""""", quotedString(_)) == Right(""))
      assert(checkedParse(""""x"""", quotedString(_)) == Right("x"))
      assert(checkedParse(""""ö"""", quotedString(_)) == Right("ö"))
      assert(checkedParse(""""abc"""", quotedString(_)) == Right("abc"))
      assert(checkedParse(""""ab\"c"""", quotedString(_)) == Right("""ab"c"""))
      assert(checkedParse(""""ab\\c"""", quotedString(_)) == Right("""ab\c"""))
      assert(checkedParse(""""ab\nc"""", quotedString(_)) == Right("ab\nc"))
      assert(checkedParse(""""ab\tc"""", quotedString(_)) == Right("ab\tc"))
      assert(checkedParse(""""ab\n\"\\c"""", quotedString(_)) == Right("ab\n\"\\c"))
    }
  }

  "specificKeyValue" in {
    def parser[x: P] = specificKeyValue("key", int)
    assert(checkedParse("key=123", parser(_)) == Right(123))
  }

  "specificKeyValue 2" in {
    def myKeyInt(name: String)(implicit ctx: P[?]): P[Int] =
      P(name ~ w ~ "=" ~ w ~/ int)
    def parser[x: P] = myKeyInt("key")
    assert(checkedParse("key=123", parser(_)) == Right(123))
  }

  "commaSequence" - {
    def parser[x: P] = commaSequence(identifier)

    "empty" in {
      assert(checkedParse("", parser(_)) == Right(Nil))
    }

    "single" in {
      assert(checkedParse("a", parser(_)) == Right("a" :: Nil))
    }

    "multiple" in {
      assert(checkedParse("a, bc , def ,  ghij", parser(_)) == Right("a" :: "bc" :: "def" :: "ghij" :: Nil))
    }

    "with specificKeyValue" in {
      def parser[x: P] = commaSequence(specificKeyValue("key", int))
      assert(checkedParse("", parser(_)) == Right(Nil))
      assert(checkedParse("key=1", parser(_)) == Right(1 :: Nil))
      assert(checkedParse("key=1, key=2", parser(_)) == Right(1 :: 2 :: Nil))
    }
  }

  "keyValues" - {
    def kvP[x: P] = keyValues(keyValue("number", int) | keyValue("string", quotedString))

    "empty" in {
      assert(checkedParse("", kvP(_)) == Right(KeyToValue(Map.empty)))
    }

    "single" in {
      assert(checkedParse("number=123", kvP(_)) == Right(KeyToValue(Map("number" -> 123))))
    }

    "Duplicate keys (1)" in {
      assert(checkedParse("""number=1, number=2""", kvP(_))
        == Left(Problem("""Expected unique keywords (duplicates: number):1:19, found """"")))
    }

    "Duplicate keys (2)" in {
      assert(checkedParse("""string="A", string="B"""", kvP(_))
        == Left(Problem("""Expected unique keywords (duplicates: string):1:23, found """"")))
    }

    "Valid" in {
      assert(checkedParse("""string="STRING", number=7""", kvP(_))
        == Right(KeyToValue(Map[String, Any]("string" -> "STRING", "number" -> 7))))
    }

    "apply" in {
      def p[x: P]: P[(Int, String)] = kvP.flatMap(keyToValue =>
        for {
          number <- keyToValue[java.lang.Integer]("number")
          string <- keyToValue[String]("string")
        } yield (number, string))
      assert(checkedParse("""string="STRING", number=7""", p(_)) == Right((7, "STRING")))
      assert(checkedParse("""string="STRING", number="7"""", p(_)).isLeft)
    }
  }

  "KeyToValue" - {
    val keyToValue = KeyToValue(Map[String, Any](
      "string" -> "STRING",
      "string2" -> "STRING2",
      "number" -> 7))

    // checkedParse("") in following tests due to Parser.flatMap

    "apply failed" in {
      def p[x: P] = keyToValue[String]("MISSING")
      assert(checkedParse("", p(_)) == Left(Problem("""Expected keyword MISSING=:1:1, found """"")))
    }

    "apply" in {
      def p[x: P] = keyToValue[String]("string")
      assert(checkedParse("", p(_)) == Right("STRING"))
    }

    "apply with default" in {
      assert(checkedParse("", keyToValue("string", "DEFAULT")(_)) == Right("STRING"))
      assert(checkedParse("", keyToValue("MISSING", "DEFAULT")(_)) == Right("DEFAULT"))
    }

    "oneOf" in {
      assert(checkedParse("", keyToValue.oneOf[String](Set("string", "string2"))(_))
        == Left(Problem("""Expected non-contradicting keywords: string; string2:1:1, found """"")))
      assert(checkedParse("", keyToValue.oneOf[String](Set("string", "x"))(_)) == Right("string" -> "STRING"))
      assert(checkedParse("", keyToValue.oneOf[String](Set("string2", "x"))(_)) == Right("string2" -> "STRING2"))
      assert(checkedParse("", keyToValue.oneOf[String](Set("y", "x"))(_)) == Left(Problem("""Expected keywords y=, x=:1:1, found """"")))
    }

    "oneOfOr" in {
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("string", "string2"), "DEFAULT")(_))
        == Left(Problem("""Expected non-contradicting keywords: string; string2:1:1, found """"")))
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("string", "x"), "DEFAULT")(_)) == Right("STRING"))
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("string2", "x"), "DEFAULT")(_)) == Right("STRING2"))
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("y", "x"), "DEFAULT")(_)) == Right("DEFAULT"))
    }

    "noneOrOneOf" in {
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("string", "string2"))(_)) ==
        Left(Problem("""Expected non-contradicting keywords: string; string2:1:1, found """"")))
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("string", "x"))(_)) == Right(Some("string" -> "STRING")))
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("string2", "x"))(_)) == Right(Some("string2" -> "STRING2")))
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("y", "x"))(_)) == Right(None))
    }
  }
}
