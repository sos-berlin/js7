package js7.executor.internal

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
import js7.executor.configuration.JobExecutorConf
import js7.executor.configuration.Problems.SignedInjectionNotAllowed
import js7.executor.process.{AbsolutePathJobExecutor, CommandLineJobExecutor, RelativePathJobExecutor, ScriptJobExecutor}
import js7.executor.{OrderProcess, ProcessOrder}
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.Try

trait JobExecutor
{
  protected val jobConf: JobConf
  protected val pathToJobResource: JobResourcePath => Checked[JobResource]

  def precheckAndWarn = Task.unit

  def start: Task[Checked[Unit]]

  def stop: Task[Unit]

  def prepareOrderProcess(processOrder: ProcessOrder): Task[Checked[OrderProcess]]

  override def toString = s"${getClass.simpleScalaName}(${jobConf.jobKey})"
}

object JobExecutor
{
  private val logger = Logger(getClass)

  def checked(
    jobConf: JobConf,
    executorConf: JobExecutorConf,
    pathToJobResource: JobResourcePath => Checked[JobResource])
    (implicit scheduler: Scheduler, iox: IOExecutor)
  : Checked[JobExecutor] = {
    import jobConf.workflowJob
    workflowJob.executable match {
      case executable: AbsolutePathExecutable =>
        if (!executorConf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else
          Right(new AbsolutePathJobExecutor(executable, jobConf, executorConf, pathToJobResource))

      case executable: RelativePathExecutable =>
        Right(new RelativePathJobExecutor(executable, jobConf, executorConf, pathToJobResource))

      case executable: ShellScriptExecutable =>
        ScriptJobExecutor.checked(executable, jobConf, executorConf, pathToJobResource)

      case executable: CommandLineExecutable =>
        if (!executorConf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else
          Right(new CommandLineJobExecutor(executable, jobConf, executorConf, pathToJobResource))

      case executable: InternalExecutable =>
        if (!executorConf.scriptInjectionAllowed)
          Left(SignedInjectionNotAllowed)
        else {
          lazy val scope = NowScope() |+| EnvScope
          for (jobArguments <- evalExpressionMap(executable.jobArguments, scope))
            yield new InternalJobExecutor(executable, jobConf, pathToJobResource, jobArguments,
              executorConf.blockingJobScheduler)
        }
    }
  }

  private[executor] def warnIfNotExecutable(file: Path): Unit =
    if (!exists(file)) {
      logger.warn(s"Executable '$file' not found")
    } else if (isUnix && !Try(getPosixFilePermissions(file) contains OWNER_EXECUTE).getOrElse(true)) {
      logger.warn(s"Executable '$file' is not user executable")
    }
}
