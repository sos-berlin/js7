package js7.data.parser

import cats.data.NonEmptyList
import cats.parse.Parser.Expectation.InRange
import cats.parse.Parser.{end, string}
import cats.parse.{Parser, Parser0}
import js7.base.parser.BasicPrinter.appendIdentifier
import js7.base.parser.BasicParsers.*
import js7.base.parser.Parsers.{ParsingProblem, checkedParse}
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.withStringBuilder

final class BasicParsersTest extends OurTestSuite:

  "checkedParse" - {
    "End" in:
      assert(checkedParse("", end) == Right(()))

    "String" in:
      assert(checkedParse("x", string("x")) == Right(()))
      assert(checkedParse("y", string("x")) == Left(ParsingProblem(
        "y", Parser.Error(0, NonEmptyList.one(InRange(0, 'x', 'x'))))))
  }

  "int" in:
    assert(checkedParse("123", int) == Right(123))

  "identifier" in:
    assertIdentifier("name", Right("name"))
    assertIdentifier("name123", Right("name123"))
    assertIdentifier("a.b", Left(Problem(
      "Parsing failed at position 2 “a❓.b” · Expected end of input")))
    assertIdentifier("``", Left(Problem("" +
      "Parsing failed at position 3 “``❓” · Identifier in backticks (`) must not be empty")))
    assertIdentifier("`a.b`", Right("a.b"))
    assertIdentifier("`a``b`", Right("a`b"))
    assertIdentifier("`a````b`", Right("a``b"))
    assertIdentifier("```a````b```", Right("`a``b`"))
    assertIdentifier("1name", Left(Problem(
      "Parsing failed at position 1 “❓1name” · Expected identifer · Expected character '`'")))

    def assertIdentifier(expr: String, expected: Checked[String]) =
      val result = checkedParse(expr, identifier)
      assert(result == expected)
      for r <- result do
        assert(withStringBuilder(appendIdentifier(_, r)) == expr)

  "String in single-quotes" - {
    "Empty single-quote string is not allowed" in:
      assert(checkedParse("''", quotedString) ==
        Left(Problem("Parsing failed at position 3 “''❓”" +
          " · Expected properly terminated ''…''-quoted string without non-printable characters " +
          "(except \\t, \\r and \\n) — or use \"\" (but not '') for the empty string" +
          " · Expected character != '\\''")))

    "Backquote + single-quote cannot be used" in:
      assert(checkedParse("""'\''""", quotedString) == Left(Problem(
        """Parsing failed at position 4 “'\'❓'” · Expected end of input""")))

    "Newline \\n (multi-line) is allowed" in:
      assert(checkedParse("'\n'", quotedString) == Right("\n"))

    "Multi-line" in:
      val s =
        """test '
          |  first line
          |    indented line
          |  third line
          |fourth line
          |'""".stripMargin
      assert(checkedParse(s, keyword("test") *> w *> quotedString) == Right(
         """
           |  first line
           |    indented line
           |  third line
           |fourth line
           |""".stripMargin))

    "Multiple quoted" in:
      assert(checkedParse("''abc''", quotedString) == Right("abc"))
      assert(checkedParse("''ab'c''", quotedString) == Right("ab'c"))
      assert(checkedParse("''ab'c'd''", quotedString) == Right("ab'c'd"))
      assert(checkedParse("'''ab''c'd'''", quotedString) == Right("ab''c'd"))
      assert(checkedParse("''''ab'''c'''d''''", quotedString) == Right("ab'''c'''d"))
      assert(checkedParse("'''''abc'''''", quotedString) == Right("abc"))
      assert(checkedParse("''''''abc''''''", quotedString) == Right("abc"))
      //  "Parsing failed at position 7 “''''''❓abc''''''” · More than 5 '-quotes are not supported")))

    "Delimiter at start or end" in:
      // FIXME quotes at string start or end does not work!
      pending

    "CF+LF is normalized to LF" in:
      assert(checkedParse("'AAA\r\nBBB\r\n'", quotedString) == Right("AAA\nBBB\n"))

    //"Control character is not allowed" in {
    //  assert(checkedParse("'\t'", quotedString) == Left(Problem(
    //    """Expected properly terminated '…'-quoted string without non-printable characters (except \r and \n):1:2, found "\t'"""")))
    //}

    "Valid" in:
      assert(checkedParse("'abc'", quotedString) == Right("abc"))
      assert(checkedParse("'ö'", quotedString) == Right("ö"))
      assert(checkedParse("""'x\n'""", quotedString) == Right("""x\n"""))
      assert(checkedParse("""'x$y'""", quotedString) == Right("x$y"))
  }

  "String in double-quotes" - {
    "Unknown backslash combination" in:
      assert(checkedParse(""""x\x"""", quotedString) == Left(Problem(
        """Parsing failed at position 4 “"x\❓x"”""" +
          " · Expected blackslash (\\) and one of the following characters: [\\\"trn$]")))

    "String interpolation is not supported at this level" in:
      assert(checkedParse(""""$variable"""", quotedString) == Left(Problem(
        """Parsing failed at position 2 “"❓$variable"” · Expected a character out of ["\\]""")))
      assert(checkedParse(""""x$variable"""", quotedString) == Left(Problem(
        """Parsing failed at position 3 “"x❓$variable"” · Expected a character out of ["\\]""")))

    "Valid" in:
      assert(checkedParse("""""""", quotedString) == Right(""))
      assert(checkedParse(""""x"""", quotedString) == Right("x"))
      assert(checkedParse(""""ö"""", quotedString) == Right("ö"))
      assert(checkedParse(""""abc"""", quotedString) == Right("abc"))
      assert(checkedParse(""""ab\"c"""", quotedString) == Right("""ab"c"""))
      assert(checkedParse(""""ab\\c"""", quotedString) == Right("""ab\c"""))
      assert(checkedParse(""""ab\nc"""", quotedString) == Right("ab\nc"))
      assert(checkedParse(""""ab\tc"""", quotedString) == Right("ab\tc"))
      assert(checkedParse(""""ab\n\"\\c"""", quotedString) == Right("ab\n\"\\c"))
  }

  "specificKeyValue" in:
    def parser = specificKeyValue("key", int)
    assert(checkedParse("key=123", parser) == Right(123))

  "specificKeyValue 2" in:
    def myKeyInt(name: String): Parser[Int] =
      string(name) *> w *> string("=") *> w *> int
    assert(checkedParse("key=123", myKeyInt("key")) == Right(123))

  "commaSequence" - {
    val parser = commaSequence(identifier)

    "empty" in:
      assert(checkedParse("", parser) == Right(Nil))

    "single" in:
      assert(checkedParse("a", parser) == Right("a" :: Nil))

    "multiple" in:
      assert(checkedParse("a, bc , def ,  ghij", parser) == Right("a" :: "bc" :: "def" :: "ghij" :: Nil))

    "with specificKeyValue" in:
      def parser = commaSequence(specificKeyValue("key", int))
      assert(checkedParse("", parser) == Right(Nil))
      assert(checkedParse("key=1", parser) == Right(1 :: Nil))
      assert(checkedParse("key=1, key=2", parser) == Right(1 :: 2 :: Nil))
  }

  "keyValues" - {
    def kvP = keyValues(keyValue("number", int) | keyValue("string", quotedString))

    "empty" in:
      assert(checkedParse("", kvP) == Right(KeyToValue(Map.empty)))

    "single" in:
      assert(checkedParse("number=123", kvP) == Right(KeyToValue(Map("number" -> 123))))

    "Duplicate keys (1)" in:
      assert(checkedParse("""number=1, number=2""", kvP)
        == Left(Problem("""Parsing failed at position 19 “…, number=2❓” · Expected unique keywords (duplicates: number)""")))

    "Duplicate keys (2)" in:
      assert(checkedParse("""string="A", string="B"""", kvP)
        == Left(Problem("""Parsing failed at position 23 “…string="B"❓” · Expected unique keywords (duplicates: string)""")))

    "Valid" in:
      assert(checkedParse("""string="STRING", number=7""", kvP)
        == Right(KeyToValue(Map[String, Any]("string" -> "STRING", "number" -> 7))))

    "apply" in:
      def p: Parser0[(Int, String)] =
        kvP.flatMap(keyToValue =>
          for
            number <- keyToValue[Int]("number")
            string <- keyToValue[String]("string")
          yield (number, string))
      assert(checkedParse("""string="STRING", number=7""", p) == Right((7, "STRING")))
      assert(checkedParse("""string="STRING", number="7"""", p).isLeft)
  }

  "KeyToValue" - {
    val keyToValue = KeyToValue(Map[String, Any](
      "string" -> "STRING",
      "string2" -> "STRING2",
      "number" -> 7))

    // checkedParse("") in following tests due to Parser.flatMap

    "apply failed" in:
      def p = keyToValue[String]("MISSING")
      assert(checkedParse("", p) == Left(Problem(
        """Parsing failed at position 1 “❓” · Expected keyword MISSING=""")))

    "apply" in:
      def p = keyToValue[String]("string")
      assert(checkedParse("", p) == Right("STRING"))

    "getOrElse" in:
      assert(checkedParse("", keyToValue.getOrElse("string", "DEFAULT")) == Right("STRING"))
      assert(checkedParse("", keyToValue.getOrElse("MISSING", "DEFAULT")) == Right("DEFAULT"))

    "oneOf" in:
      assert(checkedParse("", keyToValue.oneOf[String]("string", "string2")) == Left(Problem(
        """Parsing failed at position 1 “❓” · Expected non-contradicting keywords: string; string2""")))
      assert(checkedParse("", keyToValue.oneOf[String]("string", "x")) == Right("string" -> "STRING"))
      assert(checkedParse("", keyToValue.oneOf[String]("string2", "x")) == Right("string2" -> "STRING2"))
      assert(checkedParse("", keyToValue.oneOf[String]("y", "x")) == Left(Problem(
        "Parsing failed at position 1 “❓” · Expected one of keywords y=, x=")))

    "oneOfOr" in:
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("string", "string2"), "DEFAULT"))
        == Left(Problem("""Parsing failed at position 1 “❓” · Expected non-contradicting keywords: string; string2""")))
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("string", "x"), "DEFAULT")) == Right("STRING"))
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("string2", "x"), "DEFAULT")) == Right("STRING2"))
      assert(checkedParse("", keyToValue.oneOfOr[String](Set("y", "x"), "DEFAULT")) == Right("DEFAULT"))

    "noneOrOneOf" in:
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("string", "string2"))) ==
        Left(Problem("""Parsing failed at position 1 “❓” · Expected non-contradicting keywords: string; string2""")))
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("string", "x"))) == Right(Some("string" -> "STRING")))
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("string2", "x"))) == Right(Some("string2" -> "STRING2")))
      assert(checkedParse("", keyToValue.noneOrOneOf[String](Set("y", "x"))) == Right(None))
  }
