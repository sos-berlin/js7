package js7.data.value.expression

import js7.base.problem.{Checked, Problem}
import js7.data.value.Value

/**
  * @author Joacim Zschimmer
  */
trait Scope
{
  val symbolToValue: String => Checked[Value]

  val findValue: ValueSearch => Checked[Option[Value]]

  final def evalBoolean(expression: Expression): Checked[Boolean] =
    new Evaluator(this).evalBoolean(expression).map(_.booleanValue)

  final def evalString(expression: Expression): Checked[String] =
    new Evaluator(this).evalString(expression).map(_.string)
}

object Scope
{
  val empty: Scope = Empty

  private object Empty extends Scope {
    val symbolToValue = _ => Left(ConstantExpressionRequiredProblem)
    val findValue = _ => Left(ConstantExpressionRequiredProblem)
  }

  case object ConstantExpressionRequiredProblem extends Problem.ArgumentlessCoded
}
