package com.sos.jobscheduler.data.expression

import com.sos.jobscheduler.base.problem.{Checked, Problem}

/**
  * @author Joacim Zschimmer
  */
trait Scope
{
  val symbolToValue: String => Checked[Evaluator.Value]

  val findValue: ValueSearch => Checked[Option[Evaluator.Value]]

  final def evalBoolean(expression: Expression): Checked[Boolean] =
    new Evaluator(this).evalBoolean(expression) map (_.booleanValue)

  final def evalString(expression: Expression): Checked[String] =
    new Evaluator(this).evalString(expression) map (_.string)
}

object Scope
{
  object Constant extends Scope {
    val symbolToValue = _ => Left(ConstantExpressionRequiredProblem)
    val findValue = _ => Left(ConstantExpressionRequiredProblem)
  }

  case object ConstantExpressionRequiredProblem extends Problem.ArgumentlessCoded
}
