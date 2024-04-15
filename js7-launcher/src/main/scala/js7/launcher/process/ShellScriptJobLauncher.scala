package js7.launcher.process

import cats.effect.IO
import cats.syntax.traverse.*
import java.nio.charset.Charset
import java.nio.file.Files.createTempFile
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.ShellFileAttributes
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.system.OperatingSystem
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
import scala.collection.mutable

final class ShellScriptJobLauncher(
  protected val executable: ShellScriptExecutable,
  protected val jobConf: JobConf,
  protected val jobLauncherConf: JobLauncherConf)
extends PathProcessJobLauncher:

  private val userToFileLock = AsyncLock("ShellScriptJobLauncher.userToFile")
  private val userToFile = mutable.Map.empty[Option[WindowsUserName], Path]

  protected def checkFile =
    IO(
      executable.login
        .traverse(login => WindowsProcessCredential.keyToUser(login.credentialKey))
    ).flatMapT(maybeUserName =>
        userToFileLock.lock(IO {
          userToFile.get(maybeUserName) match {
            case Some(path) => Right(path)
            case None =>
              writeScriptToFile(
                executable.script,
                jobLauncherConf.shellScriptTmpDirectory,
                jobLauncherConf.systemEncoding,
                maybeUserName
              ).map { path =>
                userToFile.update(maybeUserName, path)
                path
              }
          }
        }))

  def stop: IO[Unit] = IO(
    userToFile.synchronized {
      tryDeleteFiles(userToFile.values)
    })


object ShellScriptJobLauncher:
  def checked(
    executable: ShellScriptExecutable,
    jobConf: JobConf,
    launcherConf: JobLauncherConf)
  : Checked[ShellScriptJobLauncher] =
    if !launcherConf.scriptInjectionAllowed then
      Left(SignedInjectionNotAllowed)
    else
      Right(new ShellScriptJobLauncher(executable, jobConf, launcherConf))

  private val crRegex = "\r?\n".r

  private[process] def writeScriptToFile(script: String, tmpDir: Path, encoding: Charset,
    userName: Option[WindowsUserName], isWindows: Boolean = OperatingSystem.isWindows)
  : Checked[Path] =
    catchNonFatal {
      val ext = if isWindows then ".cmd" else ".sh"
      createTempFile(tmpDir, "script-", ext, ShellFileAttributes*)
    }.flatMap { file =>
      catchNonFatal:
        val scrpt = if isWindows then crRegex.replaceAllIn(script, "\r\n") else script
        file.write(scrpt, encoding)
      .flatMap(_ => makeFileUserAccessible(userName, file))
      .left.map { problem =>
        tryDeleteFile(file)
        problem
      }
      .map(_ => file)
    }

  private def makeFileUserAccessible(userName: Option[WindowsUserName], file: Path): Checked[Unit] =
    userName match
      case Some(userName) if isWindows =>
        catchNonFatal:
          WindowsProcess.makeFileExecutableForUser(file, userName)
      case _ =>
        RightUnit
