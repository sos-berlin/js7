package com.sos.jobscheduler.core.expression

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.core.expression.Evaluator._
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
import com.sos.jobscheduler.data.workflow.parser.ExpressionParser
import com.sos.jobscheduler.data.workflow.parser.Parsers.ops.RichParser
import fastparse.all._
import org.scalactic.source
import org.scalatest.FreeSpec
import org.scalatest.prop.PropertyChecks._

/**
  * @author Joacim Zschimmer
  */
final class EvaluatorTest extends FreeSpec
{
  private val eval = new Evaluator(new Scope(ReturnCode(1), Map("ASTRING" → "AA", "ANUMBER" → "7"))).eval _
  private val booleanError: BooleanExpression = LessThan(ToNumber(StringConstant("X")), NumericConstant(7))

  testEval("7",
    result = 7,
    Valid(NumericConstant(7)))

  testEval(Int.MinValue.toString,  // -2147483648
    result = Int.MinValue,
    Valid(NumericConstant(Int.MinValue)))

  testEval(""" "" """,
    result = "",
    Valid(StringConstant("")))

  testSyntaxError(""" "\\" """,
    """Double-quoted (") string is not properly terminated or contains a non-printable character or backslash (\):1:2 ..."\\\\\""""")

  testSyntaxError(""" "$var" """,
    """Variable interpolation via '$' in double-quoted is not implemented. Consider using single quotes ('):1:7 ...""""")

  testEval(""" "x" """,
    result = "x",
    Valid(StringConstant("x")))

  testEval(""" '' """,
    result = "",
    Valid(StringConstant("")))

  testEval(""" 'a\x' """,
    result = "a\\x",
    Valid(StringConstant("a\\x")))

  testEval("false",
    result = false,
    Valid(BooleanConstant(false)))

  testEval("true",
    result = true,
    Valid(BooleanConstant(true)))

  testEval("$ASTRING",
    result = "AA",
    Valid(Variable(StringConstant("ASTRING"))))

  testEval("$UNKNOWN",
    result = Invalid(Problem("No such variable: UNKNOWN")),
    Valid(Variable(StringConstant("UNKNOWN"))))

  testEval("""variable("ASTRING")""",
    result = "AA",
    Valid(Variable(StringConstant("ASTRING"))))

  testEval("""variable("UNKNOWN")""",
    result = Invalid(Problem("No such variable: UNKNOWN")),
    Valid(Variable(StringConstant("UNKNOWN"))))

  testEval("""variable("UNKNOWN", "DEFAULT")""",
    result = "DEFAULT",
    Valid(Variable(StringConstant("UNKNOWN"), Some(StringConstant("DEFAULT")))))

  testEval("""$ASTRING.toNumber""",
    result = Invalid(Problem("Not a valid number: AA")),
    Valid(ToNumber(Variable(StringConstant("ASTRING")))))

  testEval("""$ANUMBER.toNumber""",
    result = 7,
    Valid(ToNumber(Variable(StringConstant("ANUMBER")))))

  testEval(""""true".toBoolean""",
    result = true,
    Valid(ToBoolean(StringConstant("true"))))

  testEval(""""false".toBoolean""",
    result = false,
    Valid(ToBoolean(StringConstant("false"))))

  testEval("""returnCode""",
    result = 1,
    Valid(OrderReturnCode))

  testEval("""returnCode == 0""",
    result = false,
    Valid(Equal(OrderReturnCode, NumericConstant(0))))

  testEval("""returnCode >= 1""",
    result = true,
    Valid(GreaterOrEqual(OrderReturnCode, NumericConstant(1))))

  testEval("""returnCode <= 1""",
    result = true,
    Valid(LessOrEqual(OrderReturnCode, NumericConstant(1))))

  testEval("""returnCode > 1""",
    result = false,
    Valid(GreaterThan(OrderReturnCode, NumericConstant(1))))

  testEval("""returnCode < 1""", false,
    Valid(LessThan(OrderReturnCode, NumericConstant(1))))

  testEval(""" "" matches "" """,
    result = true,
    Valid(Matches(StringConstant(""), StringConstant(""))))

  testEval(""" "" matches "A.+" """,
    result = false,
    Valid(Matches(StringConstant(""), StringConstant("A.+"))))

  testEval(""" "A" matches "A.+" """,
    result = false,
    Valid(Matches(StringConstant("A"), StringConstant("A.+"))))

  testEval(""" "-A-" matches "A.+" """,
    result = false,
    Valid(Matches(StringConstant("-A-"), StringConstant("A.+"))))

  testEval(""" "A--" matches "A.+" """,
    result = true,
    Valid(Matches(StringConstant("A--"), StringConstant("A.+"))))

  testEval(""" "A-" matches "A.+" """,
    result = true,
    Valid(Matches(StringConstant("A-"), StringConstant("A.+"))))

  testEval("!false",
    result = true,
    Valid(Not(BooleanConstant(false))))

  testEval("! true",
    result = false,
    Valid(Not(BooleanConstant(true))))

  testEval("!!true",
    result = true,
    Valid(Not(Not(BooleanConstant(true)))))

  testEval("returnCode >= 1 && !(returnCode <= 9) && returnCode != 1",
    result = false,
    Valid(
      And(
        And(
          GreaterOrEqual(OrderReturnCode, NumericConstant(1)),
          Not(LessOrEqual(OrderReturnCode, NumericConstant(9)))),
        NotEqual(OrderReturnCode, NumericConstant(1)))))

  testEval("returnCode in [1, 2, 3]",
    result = true,
    Valid(
      In(OrderReturnCode, ListExpression(List(NumericConstant(1), NumericConstant(2), NumericConstant(3))))))

  "Equal" in {
    forAll((a: Int, b: Int) ⇒ assert(
      eval(Equal(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a == b))))
    assert(eval(Equal(NumericConstant(1), StringConstant("1"))) == Valid(BooleanValue(false)))
  }

  "NotEqual" in {
    forAll((a: Int, b: Int) ⇒ assert(
      eval(NotEqual(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a != b))))
    assert(eval(NotEqual(NumericConstant(1), StringConstant("1"))) == Valid(BooleanValue(true)))
  }

  "LessOrEqual" in {
    forAll((a: Int, b: Int) ⇒ assert(
      eval(LessOrEqual(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a <= b))))
  }

  "GreaterOrEqual" in {
    forAll((a: Int, b: Int) ⇒ assert(
      eval(GreaterOrEqual(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a >= b))))
  }

  "LessThan" in {
    forAll((a: Int, b: Int) ⇒ assert(
      eval(LessThan(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a < b))))
  }

  "GreaterThan" in {
    forAll((a: Int, b: Int) ⇒ assert(
      eval(GreaterThan(NumericConstant(a), NumericConstant(b))) == Valid(BooleanValue(a > b))))
  }

  "In" in {
    forAll((a: Int, b: Int, c: Int, d: Int) ⇒ assert(
      eval(In(NumericConstant(a), ListExpression(NumericConstant(b) :: NumericConstant(c) :: NumericConstant(d) :: Nil)))
        == Valid(BooleanValue(Set(b, c, d)(a)))))
  }

  "Not" in {
    forAll((bool: Boolean) ⇒ assert(
      eval(Not(BooleanConstant(bool))) == Valid(BooleanValue(!bool))))
  }

  "And" in {
    forAll((a: Boolean, b: Boolean) ⇒ assert(
      eval(And(BooleanConstant(a), BooleanConstant(b))) == Valid(BooleanValue(a && b))))
  }

  "And is lazy" in {
    assert(eval(And(BooleanConstant(true), booleanError)) == Invalid(Problem("Not a valid number: X")))
    assert(eval(And(BooleanConstant(false), booleanError)) == Valid(BooleanValue(false)))
  }

  "Or" in {
    forAll((a: Boolean, b: Boolean) ⇒ assert(
      eval(Or(BooleanConstant(a), BooleanConstant(b))) == Valid(BooleanValue(a || b))))
  }

  "Or is lazy" in {
    assert(eval(Or(BooleanConstant(true), booleanError)) == Valid(BooleanValue(true)))
    assert(eval(Or(BooleanConstant(false), booleanError)) == Invalid(Problem("Not a valid number: X")))
  }

  "And and LessThan" in {
    assert(eval(And(LessThan(NumericConstant(1), NumericConstant(2)), LessThan(NumericConstant(1), ToNumber(StringConstant("7"))))) ==
      Valid(BooleanValue(true)))
  }

  private val completeExpression = ExpressionParser.expression ~ End

  private def testSyntaxError(exprString: String, problem: String)(implicit pos: source.Position): Unit =
    registerTest(s"$exprString - should fail") {
      assert(completeExpression.checkedParse(exprString.trim) == Invalid(Problem(problem)))
    }

  private def testEval(exprString: String, result: Boolean, expression: Checked[Expression])(implicit pos: source.Position): Unit =
    testEval(exprString, Valid(BooleanValue(result)), expression)

  private def testEval(exprString: String, result: Int, expression: Checked[Expression])(implicit pos: source.Position): Unit =
    testEval(exprString, Valid(NumericValue(result)), expression)

  private def testEval(exprString: String, result: String, expression: Checked[Expression])(implicit pos: source.Position): Unit =
    testEval(exprString, Valid(StringValue(result)), expression)

  private def testEval(exprString: String, result: Checked[Value], expression: Checked[Expression])(implicit pos: source.Position): Unit =
    registerTest(exprString) {
      assert(completeExpression.checkedParse(exprString.trim) == expression)
      for (e ← expression) {
        assert(completeExpression.checkedParse(e.toString) == expression, " *** toString ***")
        assert(eval(e) == result)
      }
    }
}
