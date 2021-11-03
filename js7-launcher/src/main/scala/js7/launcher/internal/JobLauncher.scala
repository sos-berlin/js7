package js7.launcher.internal

import cats.syntax.semigroup._
import java.nio.file.Files.{exists, getPosixFilePermissions}
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.system.OperatingSystem.isUnix
import js7.base.thread.IOExecutor
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{AbsolutePathExecutable, CommandLineExecutable, InternalExecutable, JobConf, JobResource, JobResourcePath, RelativePathExecutable, ShellScriptExecutable}
import js7.data.value.expression.Scope.evalExpressionMap
import js7.data.value.expression.scopes.{EnvScope, NowScope}
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.configuration.Problems.SignedInjectionNotAllowed
import js7.launcher.process.{AbsolutePathJobLauncher, CommandLineJobLauncher, RelativePathJobLauncher, ShellScriptJobLauncher}
import js7.launcher.{OrderProcess, ProcessOrder}
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.Try

trait JobLauncher
{
  protected val jobConf: JobConf
  protected val pathToJobResource: JobResourcePath => Checked[JobResource]

  def precheckAndWarn = Task.unit

  protected def start: Task[Checked[Unit]]

  def stop: Task[Unit]

  def toOrderProcess(processOrder: ProcessOrder): Task[Checked[OrderProcess]]

  final lazy val startIfNeeded: Task[Checked[Unit]] =
    start.memoize

  override def toString = s"${getClass.simpleScalaName}(${jobConf.jobKey})"
}

object JobLauncher
{
  private val logger = Logger(getClass)

  def checked(
    jobConf: JobConf,
    launcherConf: JobLauncherConf,
    pathToJobResource: JobResourcePath => Checked[JobResource])
    (implicit scheduler: Scheduler, iox: IOExecutor)
  : Checked[JobLauncher] = {
    import jobConf.workflowJob
    workflowJob.executable match {
      case executable: AbsolutePathExecutable =>
        if (!launcherConf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else
          Right(new AbsolutePathJobLauncher(executable, jobConf, launcherConf, pathToJobResource))

      case executable: RelativePathExecutable =>
        Right(new RelativePathJobLauncher(executable, jobConf, launcherConf, pathToJobResource))

      case executable: ShellScriptExecutable =>
        ShellScriptJobLauncher.checked(executable, jobConf, launcherConf, pathToJobResource)

      case executable: CommandLineExecutable =>
        if (!launcherConf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else
          Right(new CommandLineJobLauncher(executable, jobConf, launcherConf, pathToJobResource))

      case executable: InternalExecutable =>
        if (!launcherConf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else {
          lazy val scope = NowScope() |+| EnvScope
          for (jobArguments <- evalExpressionMap(executable.jobArguments, scope))
            yield new InternalJobLauncher(executable, jobConf, pathToJobResource, jobArguments,
              launcherConf.blockingJobScheduler, launcherConf.clock)
        }
    }
  }

  private[launcher] def warnIfNotExecutable(file: Path): Unit =
    if (!exists(file)) {
      logger.warn(s"Executable '$file' not found")
    } else if (isUnix && !Try(getPosixFilePermissions(file) contains OWNER_EXECUTE).getOrElse(true)) {
      logger.warn(s"Executable '$file' is not user executable")
    }
}