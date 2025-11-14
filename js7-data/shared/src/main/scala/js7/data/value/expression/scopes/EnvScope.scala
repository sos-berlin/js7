package js7.data.value.expression.scopes

import java.util.concurrent.ConcurrentHashMap
import js7.base.problem.Checked
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.InvalidFunctionArgumentsProblem
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.scopes.EnvScope.testEnv
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{StringValue, Value}
import org.jetbrains.annotations.TestOnly

/** Accesses environment variables. */
trait EnvScope extends Scope:

  // Overridable for testing purposes
  protected def getEnv(name: String): Option[String] =
    Option(testEnv.get(name)).orElse:
      sys.env.get(name)

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit scope: Scope)
  : Option[Checked[Value]] =
    functionCall match
      case FunctionCall("env", arguments) =>
        Some:
          arguments match
            case Some(Seq(Argument(nameExpr, None | Some("name")))) =>
              for
                name <- nameExpr.evalAsString
                string <- getEnv(name) !! UnknownKeyProblem("environment variable", name)
              yield
                StringValue(string)

            case Some(Seq(
            Argument(nameExpr, None | Some("name")),
            Argument(defaultExpr, None | Some("default")))) =>
              for
                name <- nameExpr.evalAsString
                result <- getEnv(name).fold(defaultExpr.eval)(v => Checked(StringValue(v)))
              yield
                result

            case _ => Left(InvalidFunctionArgumentsProblem(functionCall))

      case _ => super.evalFunctionCall(functionCall)

  override def toString = "EnvScope"


object EnvScope extends EnvScope:

  private val testEnv = new ConcurrentHashMap[String, String]

  @TestOnly
  private[js7] def putForTest(name: String, value: String): Unit =
    testEnv.put(name, value)
