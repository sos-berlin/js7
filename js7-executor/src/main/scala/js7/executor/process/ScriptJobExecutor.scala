package js7.executor.process

import java.nio.file.Files.createTempFile
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.ShellFileAttributes
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.job.{JobConf, JobResource, JobResourcePath, ScriptExecutable}
import js7.executor.configuration.JobExecutorConf
import js7.executor.configuration.Problems.SignedInjectionNotAllowed
import js7.executor.forwindows.{WindowsProcess, WindowsProcessCredentials}
import js7.executor.process.RichProcess.tryDeleteFile
import monix.eval.Task

final class ScriptJobExecutor(
  protected val executable: ScriptExecutable,
  protected val jobConf: JobConf,
  protected val jobExecutorConf: JobExecutorConf,
  protected val pathToJobResource: JobResourcePath => Checked[JobResource],
  temporaryFile: Path)
extends PathProcessJobExecutor
{
  protected def checkFile = Right(temporaryFile)

  def stop = Task(tryDeleteFile(temporaryFile))
}

object ScriptJobExecutor
{
  def checked(
    executable: ScriptExecutable,
    jobConf: JobConf,
    executorConf: JobExecutorConf,
    pathToJobResource: JobResourcePath => Checked[JobResource])
  : Checked[ScriptJobExecutor] =
    if (!executorConf.scriptInjectionAllowed)
      Left(SignedInjectionNotAllowed)
    else
      for (file <- writeScriptToFile(executable, executorConf.temporaryDirectory)) yield
        new ScriptJobExecutor(executable, jobConf, executorConf, pathToJobResource, file)

  private def writeScriptToFile(executable: ScriptExecutable, tmpDir: Path): Checked[Path] =
    Checked.catchNonFatal {
      createTempFile(tmpDir, "script-", isWindows ?? ".cmd", ShellFileAttributes: _*)
    }.flatMap { file =>
      Checked.catchNonFatal {
        file.write(executable.script, JobExecutorConf.FileEncoding)
      }
      .flatMap(_ => makeFileUserAccessible(executable, file))
      .left.map { problem =>
        tryDeleteFile(file)
        problem
      }
      .map(_ => file)
    }

  private def makeFileUserAccessible(executable: ScriptExecutable, file: Path): Either[Problem, Unit] = {
    // TODO Create a new file with an updated file permission
    //  when the credential key's user has been changed
    //  Now, the credential key's user must not change after the first job start.
    executable.login match {
      case Some(login) if isWindows =>
        for (user <- WindowsProcessCredentials.keyToUser(login.credentialKey)) yield {
          WindowsProcess.makeFileExecutableForUser(file, user)
          ()
        }
      case _ =>
        RightUnit
    }
  }
}
