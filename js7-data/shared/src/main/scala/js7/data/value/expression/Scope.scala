package js7.data.value.expression

import cats.kernel.Monoid
import cats.syntax.semigroup._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.Lazy
import js7.base.utils.ScalaUtils.checkedCast
import js7.data.value.expression.Expression.{FunctionCall, JobResourceSetting}
import js7.data.value.expression.scopes.{DoubleScope, EnvScope, NowScope}
import js7.data.value.{NumberValue, Value}
import scala.collection.MapView

/**
  * @author Joacim Zschimmer
  */
trait Scope
{
  final lazy val evaluator = Evaluator(this)

  def symbolToValue(symbol: String): Option[Checked[Value]] =
    None

  def findValue(valueSearch: ValueSearch): Checked[Option[Value]] =
    Checked(None)

  final def evalBoolean(expression: Expression): Checked[Boolean] =
    evaluator.evalBoolean(expression).map(_.booleanValue)

  final def evalString(expression: Expression): Checked[String] =
    evaluator.evalString(expression).map(_.string)

  def evalFunctionCall(functionCall: FunctionCall): Option[Checked[Value]] =
    None

  def evalJobResourceSetting(setting: JobResourceSetting): Option[Checked[Value]] =
    None

  def namedValue(name: String): Checked[Option[Value]] =
    findValue(ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name)))

  def parseAndEvalToBigDecimal(expression: String): Checked[BigDecimal] =
    parseAndEval(expression)
      .flatMap(checkedCast[NumberValue])
      .map(_.number)

  def parseAndEval(expression: String): Checked[Value] =
    ExpressionParser.parse(expression)
      .flatMap(evaluator.eval)

  def evalNameToExpression(nameToExpression: Map[String, Expression]): Checked[Map[String, Value]] =
    nameToExpression
      .toVector
      .traverse { case (k, v) => evaluator.eval(v).map(k -> _) }
      .map(_.toMap)

  def evalLazilyNameToExpression(nameToExpression: Map[String, Expression])
  : MapView[String, Checked[Value]] =
    nameToExpression.view
      .mapValues(expr => Lazy(evaluator.eval(expr)))
      .toMap
      .view
      .mapValues(_.apply())
}

object Scope
{
  implicit object monoid extends Monoid[Scope] {
    def empty: Scope =
      Empty

    def combine(a: Scope, b: Scope): Scope =
      new DoubleScope(a, b)
  }

  val empty: Scope = Empty

  def standard(): Scope =
    NowScope() |+| EnvScope

  private object Empty extends Scope
}
