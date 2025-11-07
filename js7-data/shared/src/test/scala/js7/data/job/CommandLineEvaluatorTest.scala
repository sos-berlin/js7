package js7.data.job

import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.data.value.expression.Scope
import js7.data.value.{NumberValue, StringValue}
import scala.collection.MapView

final class CommandLineEvaluatorTest extends OurTestSuite:
  "Constant" in:
    assert(eval("ÄBC") ==
      Right(CommandLine(Seq("ÄBC"))))

  "Reference" in:
    assert(eval("XX $NAME YY $NUMERIC") ==
      Right(CommandLine(Seq("XX", "MY NAME", "YY", "7"))))

  "Spaces" in:
    assert(eval(" XX   $NAME   YY \t  $NUMERIC  ") ==
      Right(CommandLine(Seq("XX", "MY NAME", "YY", "7"))))

  "Escaped characters" in:
    assert(eval(""" \\ \" \' \$ """) ==
      Right(CommandLine(Seq("\\", "\"", "'", "$"))))

  "Constant in quotes" in:
    assert(eval("""" CONSTANT WITH SPACES """") ==
      Right(CommandLine(Seq(" CONSTANT WITH SPACES "))))

  "Reference in quotes" in:
    assert(eval(""">> "$NAME" <<""") ==
      Right(CommandLine(Seq(">>", "MY NAME", "<<"))))

  "Quotes without space" in {
    assert(eval(""">>"Hi $NAME"<< TWO""") ==
      Right(CommandLine(Seq(">>Hi MY NAME<<", "TWO"))))}

  "Concatenated quotes" in:
    assert(eval(""""$NAME""/APPENDED"""") ==
      Right(CommandLine(Seq("MY NAME/APPENDED"))))

  "Reference and escaped characters in quotes" in:
    assert(eval("""XX "$NAME \"QUOTED\"\\\$"""") ==
      Right(CommandLine(Seq("XX", """MY NAME "QUOTED"\$"""))))

  private val commandLineEvaluator =
    new CommandLineEvaluator()(
      using new Scope:
        override val nameToCheckedValue =
          MapView(
            "NAME" -> Right(StringValue("MY NAME")),
            "NUMERIC" -> Right(NumberValue(7))))

  private def eval(commandLine: String): Checked[CommandLine] =
    commandLineEvaluator.eval:
      CommandLineParser.parse(commandLine).orThrow
