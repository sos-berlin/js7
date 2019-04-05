package com.sos.jobscheduler.data.expression

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.data.expression.Expression.{BooleanExpression, StringExpression}

/**
  * @author Joacim Zschimmer
  */
trait Scope
{
  val symbolToValue: String => Checked[Evaluator.Value]

  val findValue: ValueSearch => Checked[Option[Evaluator.Value]]

  final def evalBoolean(booleanExpression: BooleanExpression): Checked[Boolean] =
    new Evaluator(this).evalBoolean(booleanExpression) map (_.booleanValue)

  final def evalString(stringExpression: StringExpression): Checked[String] =
    new Evaluator(this).evalString(stringExpression) map (_.string)
}

object Scope
{
  object Constant extends Scope {
    val symbolToValue = _ => Invalid(ConstantExpressionRequiredProblem)
    val findValue = _ => Invalid(ConstantExpressionRequiredProblem)
  }

  case object ConstantExpressionRequiredProblem extends Problem.ArgumentlessCoded
}
