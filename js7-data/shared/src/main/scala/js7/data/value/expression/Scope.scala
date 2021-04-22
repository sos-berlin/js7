package js7.data.value.expression

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.{NumberValue, StringValue, Value}

/**
  * @author Joacim Zschimmer
  */
trait Scope
{
  final lazy val evaluator = Evaluator(this)

  def symbolToValue(symbol: String): Checked[Value] =
    Left(Problem("Unknown symbol: " + symbol))

  val findValue: ValueSearch => Option[Value]

  final def evalBoolean(expression: Expression): Checked[Boolean] =
    evaluator.evalBoolean(expression).map(_.booleanValue)

  final def evalString(expression: Expression): Checked[String] =
    evaluator.evalString(expression).map(_.string)

  def evalFunctionCall(functionCall: FunctionCall): Checked[Value] =
    Left(Problem(s"Unknown function: ${functionCall.name}"))

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

  object Env extends Scope {
    val findValue = _ => None

    override def evalFunctionCall(functionCall: Expression.FunctionCall): Checked[Value] =
      functionCall match {
        case FunctionCall("env", Seq(Argument(nameExpr, None | Some("name")))) =>
          for {
            name <- evaluator.eval(nameExpr).flatMap(_.toStringValueString)
            string <- sys.env.rightOr(name, UnknownKeyProblem("environment variable", name))
          } yield
            StringValue(string)

        case _ => super.evalFunctionCall(functionCall)
      }
  }

  case object ConstantExpressionRequiredProblem extends Problem.ArgumentlessCoded
}
