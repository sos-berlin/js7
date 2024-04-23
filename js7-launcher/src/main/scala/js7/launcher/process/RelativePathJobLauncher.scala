package js7.launcher.process

import cats.effect.IO
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import js7.base.problem.Checked.catchNonFatal
import js7.data.job.{JobConf, RelativePathExecutable}
import js7.launcher.configuration.JobLauncherConf

final class RelativePathJobLauncher(
  protected val executable: RelativePathExecutable,
  protected val jobConf: JobConf,
  protected val jobLauncherConf: JobLauncherConf)
extends PathProcessJobLauncher:

  def stop: IO[Unit] =
    IO.unit

  // Evaluate file path again for each order
  protected def checkFile =
    IO:
      catchNonFatal(
        executable.toFile(
          jobLauncherConf.executablesDirectory.toRealPath(NOFOLLOW_LINKS)))
