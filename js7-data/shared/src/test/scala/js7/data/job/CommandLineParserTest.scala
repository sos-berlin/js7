package js7.data.job

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.value.expression.Expression.{ListExpr, MkString, NamedValue, StringConstant}

final class CommandLineParserTest extends OurTestSuite:
  "Empty commandline is rejected" in:
    assert(CommandLineParser.parse("") ==
      Left(Problem(
        """Parsing failed at position 1 “❓” · Unexpected “” · The command line must not be empty""")))
    assert(CommandLineParser.parse("  ") ==
      Left(Problem(
        """Parsing failed at position 3 “  ❓” · Unexpected “” · The command line must not be empty""")))

  "Constant" in:
    assert(CommandLineParser.parse("ABC") ==
      Right(CommandLineExpression("ABC", List(StringConstant("ABC")))))

  "Reference" in:
    assert(CommandLineParser.parse("XX $NAME YY $END") ==
      Right(CommandLineExpression("XX $NAME YY $END",
        List(
          StringConstant("XX"),
          NamedValue("NAME"),
          StringConstant("YY"),
          NamedValue("END")))))

  "Constant in quotes" in:
    assert(CommandLineParser.parse(""""CONSTANT"""") ==
      Right(CommandLineExpression(""""CONSTANT"""", List(StringConstant("CONSTANT")))))

  "Reference in quotes" in:
    assert(CommandLineParser.parse(""">> "$NAME" <<""") ==
      Right(CommandLineExpression(
        """>> "$NAME" <<""",
        List(
          StringConstant(">>"),
          NamedValue("NAME"),
          StringConstant("<<")))))

  "Reference and escaped characters in quotes" in:
    assert(CommandLineParser.parse("""XX "$NAME-\"QUOTED\"\\\$"""") ==
      Right(CommandLineExpression(
        """XX "$NAME-\"QUOTED\"\\\$"""",
        List(
          StringConstant("XX"),
          MkString(ListExpr(List(
            NamedValue("NAME"),
            StringConstant("""-"QUOTED"\$"""))))))))
