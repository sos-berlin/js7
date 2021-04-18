package js7.executor.process

import java.nio.file.LinkOption.NOFOLLOW_LINKS
import js7.base.problem.Checked
import js7.data.job.{JobConf, JobResource, JobResourceId, RelativePathExecutable}
import js7.executor.configuration.JobExecutorConf
import monix.eval.Task

final class RelativePathJobExecutor(
  protected val executable: RelativePathExecutable,
  protected val jobConf: JobConf,
  protected val jobExecutorConf: JobExecutorConf,
  protected val idToJobResource: JobResourceId => Checked[JobResource])
extends PathProcessJobExecutor
{
  warnAboutFile()

  def stop = Task.unit

  // Evaluate file path again for each order
  protected def checkFile =
    Checked.catchNonFatal(
      executable.toFile(
        jobExecutorConf.executablesDirectory.toRealPath(NOFOLLOW_LINKS)))
}
