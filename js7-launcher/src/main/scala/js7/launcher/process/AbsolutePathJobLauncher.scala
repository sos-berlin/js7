package js7.launcher.process

import cats.effect.IO
import java.nio.file.{InvalidPathException, Paths}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.problem.Checked.*
import js7.data.job.{AbsolutePathExecutable, JobConf}
import js7.launcher.configuration.JobLauncherConf

final class AbsolutePathJobLauncher(
  protected val executable: AbsolutePathExecutable,
  protected val jobConf: JobConf,
  protected val jobLauncherConf: JobLauncherConf)
extends PathProcessJobLauncher:

  protected val checkFile =
    memoize:
      IO:
        catchExpected[InvalidPathException]:
          Paths.get(executable.path)

  def stop: IO[Unit] =
    IO.unit
