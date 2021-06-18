package js7.data.value.expression.scopes

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.value.MissingValue
import js7.data.value.expression.Expression.JobResourceVariable
import js7.data.value.expression.{Expression, Scope}
import scala.collection.MapView

final class JobResourceScope(
  pathToJobResource: MapView[JobResourcePath, JobResource],
  useScope: Scope)
extends Scope
{
  override def evalJobResourceVariable(v: Expression.JobResourceVariable)(implicit fullScope: Scope) = {
    // fullScope is the complete scope, maybe containing order variables,
    // which should not be accessible for a JobResource, to avoid name clash and
    // unexpected depedency to the order.
    // Maybe prefer useScope (via constructor) over fullscope (via function call) in all cases ???
    evalJobResourceVariable2(v)(useScope)  // escape implicit fullScope
  }

  private def evalJobResourceVariable2(v: Expression.JobResourceVariable)(implicit scope: Scope) =
    v match { case JobResourceVariable(path: JobResourcePath, name) =>
      Some(
        pathToJobResource
          .rightOr(path, UnknownKeyProblem("JobResource", path.string))
          .flatMap(_
            .variables.get(name) match {
              case None =>
                Right(MissingValue(UnknownKeyProblem("JobResource variable", s"$path:$name")))
              case Some(expr) =>
                expr.eval
            }))

      case _ =>
        super.evalJobResourceVariable(v)}

  override def toString = s"JobResourceScope(${pathToJobResource.keys.mkString(", ")})"
}

object JobResourceScope
{
  def apply(pathToJobResource: MapView[JobResourcePath, JobResource], useScope: Scope): Scope =
    new JobResourceScope(pathToJobResource, useScope)
}
