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

  "Variable" - {
    "$ with impossible names" in {
      assert(variable.parse("$var/1") == Parsed.Success(Variable(StringConstant("var")), 4))
      assert(variable.parse("$var.1") == Parsed.Success(Variable(StringConstant("var")), 4))
      assert(variable.parse("$var-1") == Parsed.Success(Variable(StringConstant("var")), 4))
      assert(variable.parse("$var_1") == Parsed.Success(Variable(StringConstant("var_1")), 6))
    }

    "$ with possible names" in {
      assert(variable.checkedParse("$var") == Valid(Variable(StringConstant("var"))))
      assert(variable.checkedParse("$gå") == Valid(Variable(StringConstant("gå"))))
      assert(variable.checkedParse("$été") == Valid(Variable(StringConstant("été"))))
    }

    "variable()" in {
      assert(variable.checkedParse("""variable("gå")""") == Valid(Variable(StringConstant("gå"))))
      assert(variable.checkedParse("""variable ( "gå" )""") == Valid(Variable(StringConstant("gå"))))
    }
  }

  testBooleanExpression("true", BooleanConstant(true))
  testBooleanExpression("false", BooleanConstant(false))
  testBooleanExpression("(false)", BooleanConstant(false))

  """"1" < 1 fails""" in {
    booleanExpression.checkedParse(""""1" < 1""") match {
      case Valid(_) ⇒ fail()
      case Invalid(problem) ⇒ assert(problem == Problem("""Types are not comparable: "1" < 1:1:8 ..."""""))
    }
  }

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
      Equal(OrderReturnCode, NumericConstant(1)),
      Or(
        Equal(OrderReturnCode, NumericConstant(2)),
        Equal(OrderReturnCode, NumericConstant(3)))))

  testBooleanExpression("""returnCode >= 0 && returnCode <= 9 && $result == "OK"""",
    And(
      GreaterOrEqual(OrderReturnCode, NumericConstant(0)),
      And(
        LessOrEqual(OrderReturnCode, NumericConstant(9)),
        Equal(Variable(StringConstant("result")), StringConstant("OK")))))

  testBooleanExpression("""returnCode in (0, 3, 50)""",
    In(
      OrderReturnCode,
      ListExpression(List(NumericConstant(0), NumericConstant(3), NumericConstant(50)))))

  private def testBooleanExpression(exprString: String, expr: BooleanExpression)(implicit pos: source.Position) =
    registerTest(exprString) {
      import fastparse.all._
      val parser = booleanExpression ~ End
      assert(parser.checkedParse(exprString) == Valid(expr))
      assert(parser.checkedParse(expr.toString) == Valid(expr))
    }
}
