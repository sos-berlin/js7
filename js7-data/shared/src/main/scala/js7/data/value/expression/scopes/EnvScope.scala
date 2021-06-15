package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{StringValue, Value}

/** Accesses environment variables. */
object EnvScope extends Scope
{
  override def evalFunctionCall(functionCall: Expression.FunctionCall) =
    functionCall match {
      case FunctionCall("env", Seq(Argument(nameExpr, None | Some("name")))) =>
        Some(
          for {
            name <- evaluator.eval(nameExpr).flatMap(_.toStringValueString)
            string <- sys.env.rightOr(name, UnknownKeyProblem("environment variable", name))
          } yield
            StringValue(string))

      case FunctionCall("env", Seq(Argument(nameExpr, None | Some("name")), Argument(defaultExpr, None | Some("default")))) =>
        Some(
          for {
            name <- evaluator.eval(nameExpr).flatMap(_.toStringValueString)
            result <- sys.env.get(name).fold(evaluator.eval(defaultExpr))(v => Checked(StringValue(v): Value))
          } yield
            result)

      case _ => None
    }
}
