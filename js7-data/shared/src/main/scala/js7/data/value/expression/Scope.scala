package js7.data.value.expression

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.checkedCast
import js7.data.value.expression.Expression.FunctionCall
import js7.data.value.{NumberValue, Value}

/**
  * @author Joacim Zschimmer
  */
trait Scope
{
  protected final lazy val evaluator = Evaluator(this)

  val symbolToValue: String => Checked[Value]

  val findValue: ValueSearch => Checked[Option[Value]]

  final def evalBoolean(expression: Expression): Checked[Boolean] =
    evaluator.evalBoolean(expression).map(_.booleanValue)

  final def evalString(expression: Expression): Checked[String] =
    evaluator.evalString(expression).map(_.string)

  def evalFunctionCall(functionCall: FunctionCall): Checked[Value] =
    Left(Problem(s"Unknown function: ${functionCall.name}"))

  def variable(name: String): Checked[Option[Value]] =
    findValue(ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name)))

  def evalToBigDecimal(expression: String): Checked[BigDecimal] =
    eval(expression)
      .flatMap(checkedCast[NumberValue])
      .map(_.number)

  def eval(expression: String): Checked[Value] =
    ExpressionParser.parse(expression)
      .flatMap(evaluator.eval)
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
