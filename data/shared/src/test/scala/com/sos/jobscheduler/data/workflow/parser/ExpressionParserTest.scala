package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.expression.Expression._
import com.sos.jobscheduler.data.workflow.Label
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.parser.ExpressionParser._
import com.sos.jobscheduler.data.workflow.parser.Parsers.checkedParse
import fastparse.NoWhitespace._
import fastparse.{Parsed, _}
import org.scalactic.source
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExpressionParserTest extends FreeSpec
{
  // See also EvaluatorTest

  "NamedValue" - {
    "$ with impossible names" in {
      assert(parse("$var/1", dollarValueName(_)) == Parsed.Success(NamedValue(NamedValue.LastOccurred, StringConstant("var")), 4))
      assert(parse("$var.1", dollarValueName(_)) == Parsed.Success(NamedValue(NamedValue.LastOccurred, StringConstant("var")), 4))
      assert(parse("$var-1", dollarValueName(_)) == Parsed.Success(NamedValue(NamedValue.LastOccurred, StringConstant("var")), 4))
      assert(parse("$var_1", dollarValueName(_)) == Parsed.Success(NamedValue(NamedValue.LastOccurred, StringConstant("var_1")), 6))
    }

    testStringExpression("""$key""", NamedValue(NamedValue.LastOccurred, StringConstant("key")))
    testStringExpression("""$Schlüssel""", NamedValue(NamedValue.LastOccurred, StringConstant("Schlüssel")))
    testStringExpression("""$clé""", NamedValue(NamedValue.LastOccurred, StringConstant("clé")))
    testStringExpression("""$A""", NamedValue(NamedValue.LastOccurred, StringConstant("A")))
    testStringExpression("""${SOME-KEY}""", NamedValue(NamedValue.LastOccurred, StringConstant("SOME-KEY")))
    testStringExpression("""${arg:SOME-KEY}""", NamedValue(NamedValue.Argument, StringConstant("SOME-KEY")))
    testStringExpression("""${label:LABEL:SOME-KEY}""", NamedValue(NamedValue.ByLabel(Label("LABEL")), StringConstant("SOME-KEY")))
    testStringExpression("""${job:JOB:SOME-KEY}""", NamedValue(NamedValue.ByWorkflowJob(WorkflowJob.Name("JOB")), StringConstant("SOME-KEY")))

    "variable()" in {
      assert(checkedParse("""variable("clé")""", variableFunctionCall(_)) == Valid(NamedValue(NamedValue.LastOccurred, StringConstant("clé"))))
      assert(checkedParse("""variable ( "clé" )""", variableFunctionCall(_)) == Valid(NamedValue(NamedValue.LastOccurred, StringConstant("clé"))))
    }
  }

  "Boolean" - {
    testBooleanExpression("true", BooleanConstant(true))
    testBooleanExpression("false", BooleanConstant(false))
    testBooleanExpression("(false)", BooleanConstant(false))
  }

  "String" - {
    testStringExpression("'x'", StringConstant("x"))
    testStringExpression("'ö'", StringConstant("ö"))
    testStringExpression("""'a\x'""", StringConstant("""a\x"""))
    testStringExpression("""'a\\x'""", StringConstant("""a\\x"""))
    testStringExpression(""" "" """.trim, StringConstant(""))
    testStringExpression(""" "x" """.trim, StringConstant("x"))
    testStringExpression(""" "ö" """.trim, StringConstant("ö"))

    "Invalid strings" in {
      assert(checkedParse("''", stringExpression(_)).isInvalid)
      assert(checkedParse(""" "\" """.trim, stringExpression(_)).isInvalid)
      assert(checkedParse(" \"\t\" ".trim, stringExpression(_)).isInvalid)
    }
  }

  testError(""""1" < 1""",
    """Expected comparable types: '1' < 1:1:8, found """"")

  "Comparison" - {
    testBooleanExpression("returnCode != 7", NotEqual(OrderReturnCode, NumericConstant(7)))
    testBooleanExpression("returnCode > 7", GreaterThan(OrderReturnCode, NumericConstant(7)))
    testBooleanExpression("""variable("A") == "X"""", Equal(NamedValue(NamedValue.LastOccurred, StringConstant("A")), StringConstant("X")))
    testBooleanExpression("""$A == "X"""", Equal(NamedValue(NamedValue.LastOccurred, StringConstant("A")), StringConstant("X")))

    testBooleanExpression("returnCode > 0 && returnCode < 9",
      And(
        GreaterThan(OrderReturnCode, NumericConstant(0)),
        LessThan(OrderReturnCode, NumericConstant(9))))

    testBooleanExpression("returnCode >= 0 && returnCode <= 9",
      And(
        GreaterOrEqual(OrderReturnCode, NumericConstant(0)),
        LessOrEqual(OrderReturnCode, NumericConstant(9))))

    testBooleanExpression("returnCode == 1 || returnCode == 2 || returnCode == 3",
      Or(
        Or(
          Equal(OrderReturnCode, NumericConstant(1)),
          Equal(OrderReturnCode, NumericConstant(2))),
        Equal(OrderReturnCode, NumericConstant(3))))

    testBooleanExpression("""returnCode >= 0 && returnCode <= 9 && $result == "OK"""",
      And(
        And(
          GreaterOrEqual(OrderReturnCode, NumericConstant(0)),
          LessOrEqual(OrderReturnCode, NumericConstant(9))),
        Equal(NamedValue(NamedValue.LastOccurred, StringConstant("result")), StringConstant("OK"))))

    testBooleanExpression("""returnCode in [0, 3, 50]""",
      In(
        OrderReturnCode,
        ListExpression(List(NumericConstant(0), NumericConstant(3), NumericConstant(50)))))

    testError("""returnCode in [0, 3, 50] || $result == "1"""",
      """Expected boolean operarands for operator ||: [0, 3, 50] || $result == '1':1:43, found """"")

    testBooleanExpression("""(returnCode in [0, 3, 50]) || $result == "1"""",
      Or(
        In(
          OrderReturnCode,
          ListExpression(List(NumericConstant(0), NumericConstant(3), NumericConstant(50)))),
        Equal(
          NamedValue(NamedValue.LastOccurred, StringConstant("result")),
          StringConstant("1"))))

    testBooleanExpression("""returnCode==$expected.toNumber||$result=="1"||true&&returnCode>0""",
      Or(
        Or(
          Equal(
            OrderReturnCode,
            ToNumber(NamedValue(NamedValue.LastOccurred, StringConstant("expected")))),
          Equal(
            NamedValue(NamedValue.LastOccurred, StringConstant("result")),
            StringConstant("1"))),
        And(
          BooleanConstant(true),
          GreaterThan(
            OrderReturnCode,
            NumericConstant(0)))))
  }

  testStringExpression("'STRING'.stripMargin",
    StripMargin(StringConstant("STRING")))

  testBooleanExpression("""$result matches 'A.*'""",
    Matches(
      NamedValue(NamedValue.LastOccurred, StringConstant("result")),
      StringConstant("A.*")))

  "Unknown numeric function" in {
    def parser[_: P] = numericExpression ~ End
    assert(checkedParse(""""123".toNumber""", parser(_)) == Valid(ToNumber(StringConstant("123"))))
    assert(checkedParse(""""123".UNKNOWN""", parser(_)) == Invalid(Problem("""Expected known function: .UNKNOWN:1:14, found """"")))
  }

  "Unknown boolean function" in {
    def parser[_: P] = booleanExpression ~ End
    assert(checkedParse(""""true".toBoolean""", parser(_)) == Valid(ToBoolean(StringConstant("true"))))
    assert(checkedParse(""""true".UNKNOWN""", parser(_)) == Invalid(Problem("""Expected known function: .UNKNOWN:1:15, found """"")))
  }

  private def testBooleanExpression(exprString: String, expr: BooleanExpression)(implicit pos: source.Position) =
    registerTest(exprString) {
      def parser[_: P] = booleanExpression ~ End
      assert(checkedParse(exprString, parser(_)) == Valid(expr))
      assert(checkedParse(expr.toString, parser(_)) == Valid(expr), " - toString")
    }

  private def testStringExpression(exprString: String, expr: StringExpression)(implicit pos: source.Position) =
    registerTest(exprString) {
      def parser[_: P] = stringExpression ~ End
      assert(checkedParse(exprString, parser(_)) == Valid(expr))
      assert(checkedParse(expr.toString, parser(_)) == Valid(expr))
    }

  private def testError(exprString: String, errorMessage: String)(implicit pos: source.Position) =
    registerTest(exprString + " - should fail") {
      def parser[_: P] = stringExpression ~ End
      assert(checkedParse(exprString, parser(_)) == Invalid(Problem(errorMessage)))
    }
}
