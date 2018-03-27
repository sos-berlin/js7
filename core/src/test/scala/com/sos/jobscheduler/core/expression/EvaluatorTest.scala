package com.sos.jobscheduler.core.expression

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.expression.Evaluator._
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
import org.scalatest.FreeSpec
import org.scalatest.prop.PropertyChecks._

/**
  * @author Joacim Zschimmer
  */
final class EvaluatorTest extends FreeSpec
{
  private val eval = new Evaluator(new Scope(ReturnCode(1), Map("A" → "AA", "B" → "BB", "N" → "7"))).eval _
  private val booleanError: BooleanExpression = LessThan(ToNumber(StringConstant("X")), NumericConstant(7))

  "NumericConstant" in {
    forAll((i: Int) ⇒ assert(
      eval(NumericConstant(i)) == Valid(NumericValue(i))))
    assert(eval(NumericConstant(7.1)) == Valid(NumericValue(7.1)))
  }

  "StringConstant" in {
    assert(eval(StringConstant("X")) == Valid(StringValue("X")))
  }

  "BooleanConstant" in {
    forAll((bool: Boolean) ⇒ assert(
      eval(BooleanConstant(bool)) == Valid(BooleanValue(bool))))
  }

  "Variable" in {
    assert(eval(Variable(StringConstant("A"))) == Valid(StringValue("AA")))
    assert(eval(Variable(StringConstant("X"))) == Invalid(Problem("No such variable: X")))
  }

  "returnCode" in {
    assert(eval(OrderReturnCode) == Valid(NumericValue(1)))
  }

  "ToNumber" in {
    assert(eval(ToNumber(StringConstant("7.1"))) == Valid(NumericValue(BigDecimal("7.1"))))
    assert(eval(ToNumber(StringConstant("X"))) == Invalid(Problem("Not a valid number: X")))
    assert(eval(ToNumber(NumericConstant(BigDecimal("7.1")))) == Valid(NumericValue(BigDecimal("7.1"))))
    assert(eval(ToNumber(BooleanConstant(false))) == Valid(NumericValue(0)))
    assert(eval(ToNumber(BooleanConstant(true))) == Valid(NumericValue(1)))
  }

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
}
