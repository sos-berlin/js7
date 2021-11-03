package js7.launcher.process

import java.nio.file.LinkOption.NOFOLLOW_LINKS
import js7.base.problem.Checked
import js7.data.job.{JobConf, JobResource, JobResourcePath, RelativePathExecutable}
import js7.launcher.configuration.JobLauncherConf
import monix.eval.Task

final class RelativePathJobLauncher(
  protected val executable: RelativePathExecutable,
  protected val jobConf: JobConf,
  protected val jobLauncherConf: JobLauncherConf,
  protected val pathToJobResource: JobResourcePath => Checked[JobResource])
extends PathProcessJobLauncher
{
  def stop = Task.unit

  // Evaluate file path again for each order
  protected def checkFile =
    Task {
      Checked.catchNonFatal(
        executable.toFile(
          jobLauncherConf.executablesDirectory.toRealPath(NOFOLLOW_LINKS)))
    }
}