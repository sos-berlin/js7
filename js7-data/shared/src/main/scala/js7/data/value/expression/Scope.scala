package js7.data.value.expression

import cats.kernel.Monoid
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.checkedCast
import js7.data.value.expression.Expression.FunctionCall
import js7.data.value.expression.scopes.DoubleScope
import js7.data.value.{NumberValue, Value}

/**
  * @author Joacim Zschimmer
  */
trait Scope
{
  final lazy val evaluator = Evaluator(this)

  def symbolToValue(symbol: String): Option[Checked[Value]] =
    None

  val findValue: ValueSearch => Option[Value]

  final def evalBoolean(expression: Expression): Checked[Boolean] =
    evaluator.evalBoolean(expression).map(_.booleanValue)

  final def evalString(expression: Expression): Checked[String] =
    evaluator.evalString(expression).map(_.string)

  def evalFunctionCall(functionCall: FunctionCall): Option[Checked[Value]] =
    None

  def namedValue(name: String): Option[Value] =
    findValue(ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name)))

  def parseAndEvalToBigDecimal(expression: String): Checked[BigDecimal] =
    parseAndEval(expression)
      .flatMap(checkedCast[NumberValue])
      .map(_.number)

  def parseAndEval(expression: String): Checked[Value] =
    ExpressionParser.parse(expression)
      .flatMap(evaluator.eval)
}

object Scope
{
  val empty: Scope = Empty

  private object Empty extends Scope {
    val findValue = _ => None
  }

  implicit object monoid extends Monoid[Scope] {
    def empty: Scope =
      Empty

    def combine(a: Scope, b: Scope): Scope =
      new DoubleScope(a, b)
  }
}
