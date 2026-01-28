package js7.data.value.expression.scopes

import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.InvalidFunctionArgumentsProblem
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.value.ValueType.UnknownNameInExpressionProblem
import js7.data.value.expression.Expression.{Argument, FunctionCall}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{ObjectValue, Value}
import scala.annotation.unused

/**
  * @param throwException Exception to throw when a JobResource is to be queried.
  */
final class JobResourceScope(
  pathToJobResource: PartialFunction[JobResourcePath, JobResource],
  useScope: Scope,
  throwException: => Option[Exception] = None)
extends Scope:

  override def evalJobResourceVariable(v: Expression.JobResourceVariable)
    (using @unused fullScope: Scope)
  : Option[Checked[Value]] =
    // fullScope is the complete scope, maybe containing order variables,
    // which should not be accessible for a JobResource, to avoid name clash and
    // unexpected depedency to the order.
    // Maybe prefer useScope (via constructor) over fullscope (via function call) in all cases ???
    evalJobResourceVariable2(v)(using useScope)  // escape implicit fullScope

  private def evalJobResourceVariable2(v: Expression.JobResourceVariable)(implicit s: Scope) =
    Some(jobResourceVariable(v.jobResourcePath, v.name))

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit s: Scope)
  : Option[Checked[Value]] =
    functionCall match
      case FunctionCall("jobResourceVariable", arguments) =>
        Some:
          arguments match
            case Some(Seq(
              Argument(jobResourcePathExpr, None),
              Argument(variableNameExpr, None))) =>
              evalFunctionCall2(jobResourcePathExpr, Some(variableNameExpr))

            case _ =>
              Left(InvalidFunctionArgumentsProblem(functionCall))

      case FunctionCall("jobResourceVariables", arguments) =>
        Some:
          arguments match
            case Some(Seq(Argument(jobResourcePathExpr, None))) =>
              evalFunctionCall2(jobResourcePathExpr, None)

            case _ =>
              Left(InvalidFunctionArgumentsProblem(functionCall))

      case _ =>
        super.evalFunctionCall(functionCall)

  private def evalFunctionCall2(
    jobResourcePathExpr: Expression,
    variableNameExpr: Option[Expression])
    (using Scope)
  : Checked[Value] =
    for
      jobResourcePathString <- jobResourcePathExpr.evalAsString
      jobResourcePath <- JobResourcePath.checked(jobResourcePathString)
      variableName <- variableNameExpr.traverse(_.evalAsString)
      value <- jobResourceVariable(jobResourcePath, variableName)
    yield value

  private def jobResourceVariable(jrPath: JobResourcePath, variableName: Option[String])
    (using Scope)
  : Checked[Value] =
    throwException.foreach(throw _)
    pathToJobResource
      .rightOr(jrPath, UnknownKeyProblem("JobResource", jrPath.string))
      .flatMap: jobResource =>
        variableName match
          case None =>
            evalExpressionMap(jobResource.variables).map(ObjectValue(_))

          case Some(variableName) =>
            jobResource.variables.get(variableName) match
              case None => Left(UnknownNameInExpressionProblem(s"$jrPath:$variableName"))
              case Some(expr) => expr.eval

  override def toString = "JobResourceScope"
