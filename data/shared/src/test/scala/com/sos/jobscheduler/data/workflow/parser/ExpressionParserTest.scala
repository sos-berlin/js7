package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
import com.sos.jobscheduler.data.workflow.parser.ExpressionParser._
import com.sos.jobscheduler.data.workflow.parser.Parsers.ops._
import fastparse.all.Parsed
import org.scalactic.source
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExpressionParserTest extends FreeSpec {

  // See also EvaluatorTest

  "Variable" - {
    "$ with impossible names" in {
      assert(dollarVariable.parse("$var/1") == Parsed.Success(Variable(StringConstant("var")), 4))
      assert(dollarVariable.parse("$var.1") == Parsed.Success(Variable(StringConstant("var")), 4))
      assert(dollarVariable.parse("$var-1") == Parsed.Success(Variable(StringConstant("var")), 4))
      assert(dollarVariable.parse("$var_1") == Parsed.Success(Variable(StringConstant("var_1")), 6))
    }

    "$ with possible names" in {
      assert(dollarVariable.checkedParse("$var") == Valid(Variable(StringConstant("var"))))
      assert(dollarVariable.checkedParse("$gå") == Valid(Variable(StringConstant("gå"))))
      assert(dollarVariable.checkedParse("$été") == Valid(Variable(StringConstant("été"))))
    }

    "variable()" in {
      assert(functionCall.checkedParse("""variable("gå")""") == Valid(Variable(StringConstant("gå"))))
      assert(functionCall.checkedParse("""variable ( "gå" )""") == Valid(Variable(StringConstant("gå"))))
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
    assert(stringExpression.checkedParse(""" "\" """.trim).isInvalid)
    assert(stringExpression.checkedParse(""" "\\" """.trim).isInvalid)
  }

  testError(""""1" < 1""",
    """Types are not comparable: '1' < 1:1:8 ...""""")

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
    """Operator || requires Boolean operands: [0, 3, 50] || $result == '1':1:43 ...""""")

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
    import fastparse.all._
    val parser = numericExpression ~ End
    assert(parser.checkedParse(""""123".toNumber""") == Valid(ToNumber(StringConstant("123"))))
    assert(parser.checkedParse(""""123".UNKNOWN""") == Invalid(Problem("""Unknown function: .UNKNOWN:1:14 ...""""")))
  }

  "Unknown boolean function" in {
    import fastparse.all._
    val parser = booleanExpression ~ End
    assert(parser.checkedParse(""""true".toBoolean""") == Valid(ToBoolean(StringConstant("true"))))
    assert(parser.checkedParse(""""true".UNKNOWN""") == Invalid(Problem("""Unknown function: .UNKNOWN:1:15 ...""""")))
  }

  private def testBooleanExpression(exprString: String, expr: BooleanExpression)(implicit pos: source.Position) =
    registerTest(exprString) {
      import fastparse.all._
      val parser = booleanExpression ~ End
      assert(parser.checkedParse(exprString) == Valid(expr))
      assert(parser.checkedParse(expr.toString) == Valid(expr), " - toString")
    }

  private def testStringExpression(exprString: String, expr: StringExpression)(implicit pos: source.Position) =
    registerTest(exprString) {
      import fastparse.all._
      val parser = stringExpression ~ End
      assert(parser.checkedParse(exprString) == Valid(expr))
      assert(parser.checkedParse(expr.toString) == Valid(expr))
    }

  private def testError(exprString: String, errorMessage: String)(implicit pos: source.Position) =
    registerTest(exprString + " - should fail") {
      import fastparse.all._
      val parser = stringExpression ~ End
      assert(parser.checkedParse(exprString) == Invalid(Problem(errorMessage)))
    }
}
