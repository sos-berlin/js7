package js7.executor.process

import java.nio.file.Paths
import js7.base.problem.Checked
import js7.data.job.{AbsolutePathExecutable, JobConf}
import js7.executor.configuration.JobExecutorConf
import monix.eval.Task

final class AbsolutePathJobExecutor(
  protected val executable: AbsolutePathExecutable,
  protected val jobConf: JobConf,
  protected val executorConf: JobExecutorConf)
extends PathProcessJobExecutor
{
  protected val checkFile =
    Checked.catchNonFatal(
      Paths.get(executable.path))

  warnAboutFile()

  def stop = Task.unit
}
