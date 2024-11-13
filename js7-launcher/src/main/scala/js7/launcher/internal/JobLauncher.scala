package js7.launcher.internal

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.semigroup.*
import java.nio.file.Files.{exists, getPosixFilePermissions}
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isUnix
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.{AbsolutePathExecutable, CommandLineExecutable, InternalExecutable, JobConf, RelativePathExecutable, ShellScriptExecutable}
import js7.data.value.expression.Scope.evalExpressionMap
import js7.data.value.expression.scopes.{EnvScope, NowScope}
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.configuration.Problems.SignedInjectionNotAllowed
import js7.launcher.process.{AbsolutePathJobLauncher, CommandLineJobLauncher, RelativePathJobLauncher, ShellScriptJobLauncher}
import js7.launcher.{OrderProcess, ProcessOrder}
import scala.util.Try

trait JobLauncher:
  protected val jobConf: JobConf

  def precheck: IO[Checked[Unit]] = IO.pure(Checked.unit)

  protected def start: IO[Checked[Unit]]

  def stop: IO[Unit]

  def toOrderProcess(processOrder: ProcessOrder): IO[Checked[OrderProcess]]

  final lazy val startIfNeeded: IO[Checked[Unit]] =
    start.unsafeMemoize

  override def toString = s"${getClass.simpleScalaName}(${jobConf.jobKey})"


object JobLauncher:
  private val logger = Logger[this.type]

  def checked(
    jobConf: JobConf,
    launcherConf: JobLauncherConf)
    (using ioRuntime: IORuntime)
  : Checked[JobLauncher] =
    jobConf.workflowJob.executable match
      case executable: AbsolutePathExecutable =>
        if !launcherConf.scriptInjectionAllowed then
          Left(SignedInjectionNotAllowed)
        else
          Right(new AbsolutePathJobLauncher(executable, jobConf, launcherConf))

      case executable: RelativePathExecutable =>
        Right(new RelativePathJobLauncher(executable, jobConf, launcherConf))

      case executable: ShellScriptExecutable =>
        ShellScriptJobLauncher.checked(executable, jobConf, launcherConf)

      case executable: CommandLineExecutable =>
        if !launcherConf.scriptInjectionAllowed then
          Left(SignedInjectionNotAllowed)
        else
          Right(new CommandLineJobLauncher(executable, jobConf, launcherConf))

      case executable: InternalExecutable =>
        if !launcherConf.scriptInjectionAllowed then
          // If check is relaxed, consider checking permission for executable.script !!!
          Left(SignedInjectionNotAllowed)
        else
          import launcherConf.implicitIox
          lazy val scope = NowScope() |+| EnvScope
          for jobArguments <- evalExpressionMap(executable.jobArguments, scope)
            yield new InternalJobLauncher(executable, jobConf, jobArguments,
              launcherConf.blockingJobEC, launcherConf.clock)

  private[launcher] def warnIfNotExecutable(file: Path): Unit =
    if !exists(file) then
      logger.warn(s"Executable '$file' not found")
    else if isUnix && !Try(getPosixFilePermissions(file).contains(OWNER_EXECUTE)).getOrElse(true) then
      logger.warn(s"Executable '$file' is not user executable")

  private[launcher] def checkIsExecutable(file: Path): IO[Checked[Unit]] =
    IO.blocking:
      if !exists(file) then
        Left(Problem(s"Executable '$file' not found"))
      else if isUnix
        && !Try(getPosixFilePermissions(file).contains(OWNER_EXECUTE)).getOrElse(true)
      then
        Left(Problem(s"Executable '$file' is not user executable"))
      else
        Checked.unit
