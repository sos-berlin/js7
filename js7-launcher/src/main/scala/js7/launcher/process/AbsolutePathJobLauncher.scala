package js7.launcher.process

import java.nio.file.{InvalidPathException, Paths}
import js7.base.problem.Checked._
import js7.data.job.{AbsolutePathExecutable, JobConf}
import js7.launcher.configuration.JobLauncherConf
import cats.effect.IO
import js7.base.catsutils.UnsafeMemoizable.memoize

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

  def stop = IO.unit
