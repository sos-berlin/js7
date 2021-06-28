package js7.data.value.expression.scopes

import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.InvalidFunctionArgumentsProblem
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.value.expression.Expression.{Argument, FunctionCall, JobResourceVariable}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{MissingValue, ObjectValue, Value}

final class JobResourceScope(
  pathToJobResource: PartialFunction[JobResourcePath, JobResource],
  useScope: Scope)
extends Scope
{
  override def evalJobResourceVariable(v: Expression.JobResourceVariable)
    (implicit fullScope: Scope)
  = {
    // fullScope is the complete scope, maybe containing order variables,
    // which should not be accessible for a JobResource, to avoid name clash and
    // unexpected depedency to the order.
    // Maybe prefer useScope (via constructor) over fullscope (via function call) in all cases ???
    evalJobResourceVariable2(v)(useScope)  // escape implicit fullScope
  }

  private def evalJobResourceVariable2(v: Expression.JobResourceVariable)(implicit s: Scope) =
    v match {
      case JobResourceVariable(path: JobResourcePath, variableName) =>
        Some(jobResourceVariable(path, variableName))

      case _ =>
        super.evalJobResourceVariable(v)
    }

  override def evalFunctionCall(functionCall: Expression.FunctionCall)(implicit s: Scope) =
    functionCall match {
      case FunctionCall("jobResourceVariable", arguments) =>
        Some(arguments match {
          case Seq(
            Argument(jobResourcePathExpr, None),
            Argument(variableNameExpr, None)) =>
            evalFunctionCall2(jobResourcePathExpr, Some(variableNameExpr))

          case _ =>
            Left(InvalidFunctionArgumentsProblem(functionCall))
        })

      case FunctionCall("jobResourceVariables", arguments) =>
        Some(arguments match {
          case Seq(Argument(jobResourcePathExpr, None)) =>
            evalFunctionCall2(jobResourcePathExpr, None)

          case _ =>
            Left(InvalidFunctionArgumentsProblem(functionCall))
        })

      case _ => None
    }

  private def evalFunctionCall2(
    jobResourcePathExpr: Expression,
    variableNameExpr: Option[Expression])
    (implicit s: Scope)
  : Checked[Value] =
    for {
      jobResourcePathString <- jobResourcePathExpr.evalAsString
      jobResourcePath <- JobResourcePath.checked(jobResourcePathString)
      variableName <- variableNameExpr.traverse(_.evalAsString)
      value <- jobResourceVariable(jobResourcePath, variableName)
    } yield value

  private def jobResourceVariable(jrPath: JobResourcePath, variableName: Option[String])
    (implicit s: Scope)
  : Checked[Value] =
    pathToJobResource
      .rightOr(jrPath, UnknownKeyProblem("JobResource", jrPath.string))
      .flatMap(jobResource =>
        variableName match {
          case None =>
            evalExpressionMap(jobResource.variables).map(ObjectValue(_))

          case Some(variableName) =>
            jobResource.variables.get(variableName) match {
              case None =>
                Right(MissingValue(
                  UnknownKeyProblem("JobResource variable", s"$jrPath:$variableName")))
              case Some(expr) =>
                expr.eval
            }
        })

  override def toString = s"JobResourceScope"
}

object JobResourceScope
{
  def apply(pathToJobResource: PartialFunction[JobResourcePath, JobResource], useScope: Scope): Scope =
    new JobResourceScope(pathToJobResource, useScope)
}
