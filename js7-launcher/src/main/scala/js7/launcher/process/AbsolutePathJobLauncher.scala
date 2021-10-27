package js7.launcher.process

import java.nio.file.Paths
import js7.base.problem.Checked
import js7.data.job.{AbsolutePathExecutable, JobConf, JobResource, JobResourcePath}
import js7.launcher.configuration.JobLauncherConf
import monix.eval.Task

final class AbsolutePathJobLauncher(
  protected val executable: AbsolutePathExecutable,
  protected val jobConf: JobConf,
  protected val jobLauncherConf: JobLauncherConf,
  protected val pathToJobResource: JobResourcePath => Checked[JobResource])
extends PathProcessJobLauncher
{
  protected val checkFile =
    Task {
      Checked.catchNonFatal(
        Paths.get(executable.path))
    }.memoize

  def stop = Task.unit
}
