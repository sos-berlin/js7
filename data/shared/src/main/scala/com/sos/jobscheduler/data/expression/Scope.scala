package com.sos.jobscheduler.data.expression

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.problem.{Checked, Problem}

/**
  * @author Joacim Zschimmer
  */
trait Scope
{
  val symbolToValue: String => Checked[Evaluator.Value]

  val variableNameToString: String => Checked[Option[String]]
}

object Scope
{
  object Constant extends Scope {
    val symbolToValue = _ => Invalid(ConstantExpressionRequiredProblem)
    val variableNameToString = _ => Invalid(ConstantExpressionRequiredProblem)
  }

  case object ConstantExpressionRequiredProblem extends Problem.ArgumentlessCoded
}
