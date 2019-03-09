package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
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

  "Variable" - {
    "$ with impossible names" in {
      assert(parse("$var/1", dollarVariable(_)) == Parsed.Success(Variable(StringConstant("var")), 4))
      assert(parse("$var.1", dollarVariable(_)) == Parsed.Success(Variable(StringConstant("var")), 4))
      assert(parse("$var-1", dollarVariable(_)) == Parsed.Success(Variable(StringConstant("var")), 4))
      assert(parse("$var_1", dollarVariable(_)) == Parsed.Success(Variable(StringConstant("var_1")), 6))
    }

    "$ with possible names" in {
      assert(checkedParse("$var", dollarVariable(_)) == Valid(Variable(StringConstant("var"))))
      assert(checkedParse("$gå", dollarVariable(_)) == Valid(Variable(StringConstant("gå"))))
      assert(checkedParse("$été", dollarVariable(_)) == Valid(Variable(StringConstant("été"))))
    }

    "variable()" in {
      assert(checkedParse("""variable("gå")""", variableFunctionCall(_)) == Valid(Variable(StringConstant("gå"))))
      assert(checkedParse("""variable ( "gå" )""", variableFunctionCall(_)) == Valid(Variable(StringConstant("gå"))))
    }
  }

  testBooleanExpression("true", BooleanConstant(true))
  testBooleanExpression("false", BooleanConstant(false))
  testBooleanExpression("(false)", BooleanConstant(false))

  testStringExpression("''", StringConstant(""))
  testStringExpression("'x'", StringConstant("x"))
  testStringExpression("'ö'", StringConstant("ö"))
  testStringExpression("""'a\x'""", StringConstant("""a\x"""))
  testStringExpression("""'a\\x'""", StringConstant("""a\\x"""))
  testStringExpression(""" "" """.trim, StringConstant(""))
  testStringExpression(""" "x" """.trim, StringConstant("x"))
  testStringExpression(""" "ö" """.trim, StringConstant("ö"))

  "Invalid strings" in {
    assert(checkedParse(""" "\" """.trim, stringExpression(_)).isInvalid)
    assert(checkedParse(""" "\\" """.trim, stringExpression(_)).isInvalid)
  }

  testError(""""1" < 1""",
    """Expected comparable types: '1' < 1:1:8, found """"")

  testBooleanExpression("returnCode != 7", NotEqual(OrderReturnCode, NumericConstant(7)))
  testBooleanExpression("returnCode > 7", GreaterThan(OrderReturnCode, NumericConstant(7)))
  testBooleanExpression("""variable("A") == "X"""", Equal(Variable(StringConstant("A")), StringConstant("X")))
  testBooleanExpression("""$A == "X"""", Equal(Variable(StringConstant("A")), StringConstant("X")))

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
      Equal(Variable(StringConstant("result")), StringConstant("OK"))))

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
        Variable(StringConstant("result")),
        StringConstant("1"))))

  testBooleanExpression("""returnCode==$expected.toNumber||$result=="1"||true&&returnCode>0""",
    Or(
      Or(
        Equal(
          OrderReturnCode,
          ToNumber(Variable(StringConstant("expected")))),
        Equal(
          Variable(StringConstant("result")),
          StringConstant("1"))),
      And(
        BooleanConstant(true),
        GreaterThan(
          OrderReturnCode,
          NumericConstant(0)))))

  testBooleanExpression("""$result matches 'A.*'""",
    Matches(
      Variable(StringConstant("result")),
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
