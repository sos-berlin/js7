package js7.data.value.expression.scopes

import js7.base.problem.Checked
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.InvalidFunctionArgumentsProblem
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{StringValue, Value}

/** Accesses environment variables. */
sealed class EnvScope(env: String => Option[String]) extends Scope
{
  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit scope: Scope) =
    functionCall match {
      case FunctionCall("env", arguments) =>
        Some(arguments match {
          case Seq(Argument(nameExpr, None | Some("name"))) =>
            for {
              name <- nameExpr.evalAsString
              string <- env(name) !! UnknownKeyProblem("environment variable", name)
            } yield StringValue(string)

          case Seq(
          Argument(nameExpr, None | Some("name")),
          Argument(defaultExpr, None | Some("default"))) =>
            for {
              name <- nameExpr.evalAsString
              result <- env(name).fold(defaultExpr.eval)(v => Checked(StringValue(v): Value))
            } yield result

          case _ =>
            Left(InvalidFunctionArgumentsProblem(functionCall))
        })

      case _ =>
        super.evalFunctionCall(functionCall)
    }

  override def toString = "EnvScope"
}

object EnvScope extends EnvScope(sys.env.lift)
