package js7.data.job

import js7.base.problem.Problem
import js7.data.value.expression.Expression.{ListExpression, MkString, NamedValue, StringConstant}
import org.scalatest.freespec.AnyFreeSpec

final class CommandLineParserTest extends AnyFreeSpec
{
  "Empty commandline is rejected" in {
    assert(FastparseCommandLineParser.parse("") ==
      Left(Problem("""Expected The command line must not be empty:1:1, found """"")))
    assert(FastparseCommandLineParser.parse("  ") ==
      Left(Problem("""Expected The command line must not be empty:1:3, found """"")))
  }

  "Constant" in {
    assert(FastparseCommandLineParser.parse("ABC") ==
      Right(CommandLineExpression("ABC", List(StringConstant("ABC")))))
  }

  "Reference" in {
    assert(FastparseCommandLineParser.parse("XX $NAME YY $END") ==
      Right(CommandLineExpression("XX $NAME YY $END",
        List(
          StringConstant("XX"),
          NamedValue("NAME"),
          StringConstant("YY"),
          NamedValue("END")))))
  }

  "Constant in quotes" in {
    assert(FastparseCommandLineParser.parse(""""CONSTANT"""") ==
      Right(CommandLineExpression(""""CONSTANT"""", List(StringConstant("CONSTANT")))))
  }

  "Reference in quotes" in {
    assert(FastparseCommandLineParser.parse(""">> "$NAME" <<""") ==
      Right(CommandLineExpression(
        """>> "$NAME" <<""",
        List(
          StringConstant(">>"),
          NamedValue("NAME"),
          StringConstant("<<")))))
  }

  "Reference and escaped characters in quotes" in {
    assert(FastparseCommandLineParser.parse("""XX "$NAME-\"QUOTED\"\\\$"""") ==
      Right(CommandLineExpression(
        """XX "$NAME-\"QUOTED\"\\\$"""",
        List(
          StringConstant("XX"),
          MkString(ListExpression(List(
            NamedValue("NAME"),
            StringConstant("""-"QUOTED"\$"""))))))))
  }
}
