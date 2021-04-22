package js7.data.value.expression.scopes

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}

object EnvScope extends Scope
{
  val findValue = _ => None

  override def evalFunctionCall(functionCall: Expression.FunctionCall) =
    functionCall match {
      case FunctionCall("env", Seq(Argument(nameExpr, None | Some("name")))) =>
        Some(
          for {
            name <- evaluator.eval(nameExpr).flatMap(_.toStringValueString)
            string <- sys.env.rightOr(name, UnknownKeyProblem("environment variable", name))
          } yield
            StringValue(string))

      case _ => None
    }
}
