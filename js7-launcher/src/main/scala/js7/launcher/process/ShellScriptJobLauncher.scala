package js7.launcher.process

import cats.syntax.traverse._
import java.nio.file.Files.createTempFile
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.ShellFileAttributes
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.AsyncLock
import js7.base.utils.ScalaUtils.RightUnit
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.job.{JobConf, ShellScriptExecutable}
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.configuration.Problems.SignedInjectionNotAllowed
import js7.launcher.forwindows.{WindowsProcess, WindowsProcessCredential, WindowsUserName}
import js7.launcher.process.RichProcess.{tryDeleteFile, tryDeleteFiles}
import js7.launcher.process.ShellScriptJobLauncher.writeScriptToFile
import monix.eval.Task
import scala.collection.mutable

final class ShellScriptJobLauncher(
  protected val executable: ShellScriptExecutable,
  protected val jobConf: JobConf,
  protected val jobLauncherConf: JobLauncherConf)
extends PathProcessJobLauncher
{
  private val userToFileLock = AsyncLock("ShellScriptJobLauncher.userToFile")
  private val userToFile = mutable.Map.empty[Option[WindowsUserName], Path]

  protected def checkFile =
    Task(
      executable.login
        .traverse(login => WindowsProcessCredential.keyToUser(login.credentialKey))
    ).flatMapT(maybeUserName =>
        userToFileLock.lock(Task {
          userToFile.get(maybeUserName) match {
            case Some(path) => Right(path)
            case None =>
              writeScriptToFile(executable.script, jobLauncherConf.shellScriptTmpDirectory, maybeUserName)
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

object ShellScriptJobLauncher
{
  def checked(
    executable: ShellScriptExecutable,
    jobConf: JobConf,
    launcherConf: JobLauncherConf)
  : Checked[ShellScriptJobLauncher] =
    if (!launcherConf.scriptInjectionAllowed)
      Left(SignedInjectionNotAllowed)
    else
      Right(new ShellScriptJobLauncher(executable, jobConf, launcherConf))

  private def writeScriptToFile(script: String, tmpDir: Path, userName: Option[WindowsUserName]): Checked[Path] =
    Checked.catchNonFatal {
      val ext = if (isWindows) ".cmd" else ".sh"
      createTempFile(tmpDir, "script-", ext, ShellFileAttributes: _*)
    }.flatMap { file =>
      Checked.catchNonFatal {
        file.write(script, JobLauncherConf.FileEncoding)
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
