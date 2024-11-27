package js7.data.value.expression

import cats.Monoid
import js7.base.problem.Checked
import js7.base.problem.Checked.{CheckedOption, RichCheckedIterable}
import js7.base.utils.Lazy
import js7.data.Problems.RecursiveEvaluationProblem
import js7.data.value.Value
import js7.data.value.expression.Expression.{FunctionCall, JobResourceVariable}
import js7.data.value.expression.ExpressionParser.parseExpression
import js7.data.value.expression.Scope.evalLazilyExpressions
import js7.data.value.expression.scopes.CombinedScope
import scala.annotation.unused
import scala.collection.MapView

/** Provides data for `Expression` evaluation.
  * A `Scope` can provide specialized access or can be a CombinedScope,
  * which combines multiple scopes to a stack.
  * The methods get a `fullScope`
  * which can be a different `Scope` or a greater CombinedScope
  * (while `this` is the own specicialized `Scope`). */
trait Scope:

  def symbolToValue: PartialFunction[String, Checked[Value]] =
    Map.empty

  def nameToCheckedValue: PartialFunction[String, Checked[Value]] =
    Map.empty

  def findValue(valueSearch: ValueSearch): Option[Checked[Value]] =
    valueSearch match
      case ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name)) =>
        nameToCheckedValue.lift(name)
      case _ =>
        None

  final def symbol(name: String): Option[Checked[Value]] =
    evalFunctionCall(FunctionCall(name, arguments = None))(using Scope.empty)

  def evalFunctionCall(functionCall: FunctionCall)(implicit @unused fullScope: Scope)
  : Option[Checked[Value]] =
    None

  def evalJobResourceVariable(v: JobResourceVariable)(implicit @unused fullScope: Scope)
  : Option[Checked[Value]] =
    None

  final def namedValue(name: String): Option[Checked[Value]] =
    findValue:
      ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name))

  def parseAndEval(expression: String): Checked[Value] =
    parseExpression(expression)
      .flatMap(_.eval(this))

  final def evalExpressionMap(nameToExpr: Map[String, Expression]): Checked[Map[String, Value]] =
    evalLazilyExpressions(nameToExpr.view)(this)
      .toVector
      .map: (k, checked) =>
        checked.map(k -> _)
      .combineProblems
      .map(_.toMap)


object Scope extends Monoid[Scope]:
  given Monoid[Scope] = this

  val empty: Scope = EmptyScope

  private object EmptyScope extends Scope:
    override def toString = "EmptyScope"

  def combine(a: Scope, b: Scope): Scope =
    (a, b) match
      case (a, EmptyScope) => a
      case (EmptyScope, b) => b
      case _ => CombinedScope(a, b)

  /** Optimized for empty nameToExpr. */
  def evalExpressionMap(nameToExpr: Map[String, Expression], scope: => Scope)
  : Checked[Map[String, Value]] =
    if nameToExpr.isEmpty then
      Right(Map.empty)
    else
      scope.evalExpressionMap(nameToExpr)

  /** Evaluates expressions lazily and memoizes the results.
    * Recursive evaluation is detected and returned as `Left`.
    * `scope` may recursively contain the resulting `MapView`.
    */
  def evalLazilyExpressions(nameToExpr: MapView[String, Expression])(scope: => Scope)
  : MapView[String, Checked[Value]] =
    lazy val myScope = scope
    nameToExpr
      .map: (name, expr) =>
        name -> Lazy(expr.eval(myScope))
      .toMap // memoize Lazy
      .view
      // Evaluate lazily (Lazy evaluates only once)
      .mapValues(_
        .recursionCheckedValue
        .toChecked(RecursiveEvaluationProblem)
        .flatten)
