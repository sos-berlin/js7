package js7.executor.process

import java.nio.file.Paths
import js7.base.problem.Checked
import js7.data.job.{AbsolutePathExecutable, JobConf, JobResource, JobResourcePath}
import js7.executor.configuration.JobExecutorConf
import monix.eval.Task

final class AbsolutePathJobExecutor(
  protected val executable: AbsolutePathExecutable,
  protected val jobConf: JobConf,
  protected val jobExecutorConf: JobExecutorConf,
  protected val pathToJobResource: JobResourcePath => Checked[JobResource])
extends PathProcessJobExecutor
{
  protected val checkFile =
    Checked.catchNonFatal(
      Paths.get(executable.path))

  warnAboutFile()

  def stop = Task.unit
}
