package js7.data.value.expression.scopes

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.value.MissingValue
import js7.data.value.expression.Expression.JobResourceSetting
import js7.data.value.expression.{Expression, Scope}

final class JobResourceScope(jobResources: Seq[JobResource])
extends Scope
{
  private val pathToJobResource = jobResources.toKeyedMap(_.path)

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
