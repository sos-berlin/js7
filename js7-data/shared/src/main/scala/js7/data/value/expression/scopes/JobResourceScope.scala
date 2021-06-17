package js7.data.value.expression.scopes

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.value.MissingValue
import js7.data.value.expression.Expression.JobResourceVariable
import js7.data.value.expression.{Expression, Scope}
import scala.collection.MapView

final class JobResourceScope(pathToJobResource: MapView[JobResourcePath, JobResource])
extends Scope
{
  override def evalJobResourceVariable(jobResourceVariable: Expression.JobResourceVariable) =
    jobResourceVariable match {
      case JobResourceVariable(path: JobResourcePath, name) =>
        Some(
          pathToJobResource
            .rightOr(path, UnknownKeyProblem("JobResource", path.string))
            .flatMap(_
              .variables.get(name) match {
                case None =>
                  Right(MissingValue(UnknownKeyProblem("JobResource variable", s"$path:$name")))
                case Some(expr) =>
                  evaluator.eval(expr)
              }))

      case _ => None
    }
}

object JobResourceScope
{
  def apply(pathToJobResource: MapView[JobResourcePath, JobResource]): Scope =
    new JobResourceScope(pathToJobResource)
}
