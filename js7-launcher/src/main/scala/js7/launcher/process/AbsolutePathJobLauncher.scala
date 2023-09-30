package js7.launcher.process

import java.nio.file.{InvalidPathException, Paths}
import js7.base.problem.Checked._
import js7.data.job.{AbsolutePathExecutable, JobConf}
import js7.launcher.configuration.JobLauncherConf
import monix.eval.Task

final class AbsolutePathJobLauncher(
  protected val executable: AbsolutePathExecutable,
  protected val jobConf: JobConf,
  protected val jobLauncherConf: JobLauncherConf)
extends PathProcessJobLauncher:

  protected val checkFile =
    Task {
      catchExpected[InvalidPathException](
        Paths.get(executable.path))
    }.memoize

  def stop = Task.unit
