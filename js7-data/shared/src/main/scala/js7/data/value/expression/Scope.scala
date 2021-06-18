package js7.data.value.expression

import cats.Monoid
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.Lazy
import js7.data.value.Value
import js7.data.value.expression.Expression.{FunctionCall, JobResourceVariable}
import js7.data.value.expression.scopes.CombinedScope
import scala.collection.MapView

/** Provides data for `Expression` evaluation.
  * A `Scope` can provide specialized access or can be a CombinedScope,
  * which combines multiple scopes to a stack.
  * The methods get a `fullScope`
  * which can be a different `Scope` or a greater CombinedScope
  * (while `this` is the own specicialized `Scope`). */
trait Scope
{
  def symbolToValue(symbol: String)(implicit fullScope: Scope): Option[Checked[Value]] =
    None

  def findValue(valueSearch: ValueSearch)(implicit fullScope: Scope): Checked[Option[Value]] =
    Checked(None)

  def evalFunctionCall(functionCall: FunctionCall)(implicit fullScope: Scope): Option[Checked[Value]] =
    None

  def evalJobResourceVariable(v: JobResourceVariable)(implicit fullScope: Scope): Option[Checked[Value]] =
    None

  final def namedValue(name: String): Checked[Option[Value]] =
    findValue(ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name)))(this)

  def parseAndEval(expression: String): Checked[Value] =
    ExpressionParser.parse(expression)
      .flatMap(_.eval(this))

  def evalLazilyExpressionMap(nameToExpr: Map[String, Expression]): MapView[String, Checked[Value]] =
    nameToExpr.view
      .mapValues(expr => Lazy(expr.eval(this)))
      .toMap
      .view
      .mapValues(_.apply())

  def evalExpressionMap(nameToExpr: Map[String, Expression]): Checked[Map[String, Value]] =
    nameToExpr.toVector
      .traverse { case (k, v) => v.eval(this).map(k -> _) }
      .map(_.toMap)
}

object Scope extends Monoid[Scope]
{
  implicit val monoid: Monoid[Scope] = this

  val empty: Scope = Empty

  def combine(a: Scope, b: Scope) =
    (a, b) match {
      case (a, Empty) => a
      case (Empty, b) => b
      case _ => new CombinedScope(a, b)
    }

  private object Empty extends Scope {
    override def toString = "EmptyScope"
  }
}
