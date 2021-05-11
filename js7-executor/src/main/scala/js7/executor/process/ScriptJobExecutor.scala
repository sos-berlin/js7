package js7.executor.process

import cats.syntax.traverse._
import java.nio.file.Files.createTempFile
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.ShellFileAttributes
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEitherF}
import js7.base.utils.TaskLock
import js7.data.job.{JobConf, JobResource, JobResourcePath, ScriptExecutable}
import js7.executor.configuration.JobExecutorConf
import js7.executor.configuration.Problems.SignedInjectionNotAllowed
import js7.executor.forwindows.{WindowsProcess, WindowsProcessCredentials, WindowsUserName}
import js7.executor.process.RichProcess.{tryDeleteFile, tryDeleteFiles}
import js7.executor.process.ScriptJobExecutor.writeScriptToFile
import monix.eval.Task
import scala.collection.mutable

final class ScriptJobExecutor(
  protected val executable: ScriptExecutable,
  protected val jobConf: JobConf,
  protected val jobExecutorConf: JobExecutorConf,
  protected val pathToJobResource: JobResourcePath => Checked[JobResource])
extends PathProcessJobExecutor
{
  private val userToFileLock = TaskLock("ScriptJobExecutor.userToFile")
  private val userToFile = mutable.Map.empty[Option[WindowsUserName], Path]

  protected def checkFile =
    Task(
      executable.login
        .traverse(login => WindowsProcessCredentials.keyToUser(login.credentialKey))
    ).flatMapT(maybeUserName =>
        userToFileLock.lock(Task {
          userToFile.get(maybeUserName) match {
            case Some(path) => Right(path)
            case None =>
              writeScriptToFile(executable.script, jobExecutorConf.temporaryDirectory, maybeUserName)
                .map { path =>
                  userToFile.put(maybeUserName, path)
                  path
                }
          }
        }))

  def stop = Task(
    userToFile.synchronized {
      tryDeleteFiles(userToFile.values)
    })
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
      Right(new ScriptJobExecutor(executable, jobConf, executorConf, pathToJobResource))

  private def writeScriptToFile(script: String, tmpDir: Path, userName: Option[WindowsUserName]): Checked[Path] =
    Checked.catchNonFatal {
      createTempFile(tmpDir, "script-", isWindows ?? ".cmd", ShellFileAttributes: _*)
    }.flatMap { file =>
      Checked.catchNonFatal {
        file.write(script, JobExecutorConf.FileEncoding)
      }
      .flatMap(_ => makeFileUserAccessible(userName, file))
      .left.map { problem =>
        tryDeleteFile(file)
        problem
      }
      .map(_ => file)
    }

  private def makeFileUserAccessible(userName: Option[WindowsUserName], file: Path): Either[Problem, Unit] =
    userName match {
      case Some(userName) if isWindows =>
        Checked.catchNonFatal {
          WindowsProcess.makeFileExecutableForUser(file, userName)
        }
      case _ =>
        RightUnit
    }
}
