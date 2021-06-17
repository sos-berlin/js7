package js7.data.value.expression.scopes

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.value.MissingValue
import js7.data.value.expression.Expression.JobResourceSetting
import js7.data.value.expression.{Expression, Scope}
import scala.collection.MapView

final class JobResourceScope(pathToJobResource: MapView[JobResourcePath, JobResource])
extends Scope
{
  override def evalJobResourceSetting(setting: Expression.JobResourceSetting) =
    setting match {
      case JobResourceSetting(path: JobResourcePath, settingName) =>
        Some(
          pathToJobResource
            .rightOr(path, UnknownKeyProblem("JobResource", path.string))
            .flatMap(_
              .settings.get(settingName) match {
                case None => Right(MissingValue(UnknownKeyProblem("setting", s"$path:$settingName")))
                case Some(expr) => evaluator.eval(expr)
              }))

      case _ => None
    }
}

object JobResourceScope
{
  def apply(pathToJobResource: MapView[JobResourcePath, JobResource]): Scope =
    new JobResourceScope(pathToJobResource)
}
