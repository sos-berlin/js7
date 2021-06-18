package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.InvalidFunctionParametersProblem
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{StringValue, Value}

/** Accesses environment variables. */
object EnvScope extends Scope
{
  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit scope: Scope) =
    functionCall match {
      case FunctionCall("env", arguments) =>
        Some(arguments match {
          case Seq(Argument(nameExpr, None | Some("name"))) =>
            for {
              name <- nameExpr.evalAsString
              string <- sys.env.get(name) !! UnknownKeyProblem("environment variable", name)
            } yield StringValue(string)

          case Seq(
          Argument(nameExpr, None | Some("name")),
          Argument(defaultExpr, None | Some("default"))) =>
            for {
              name <- nameExpr.evalAsString
              result <- sys.env.get(name)
                .fold(defaultExpr.eval)(v => Checked(StringValue(v): Value))
            } yield result

          case _ =>
            Left(InvalidFunctionParametersProblem(functionCall))
        })

      case _ =>
        super.evalFunctionCall(functionCall)
    }

  override def toString = "EnvScope"
}
