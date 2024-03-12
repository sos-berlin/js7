package js7.launcher.configuration

import com.typesafe.config.Config
import java.nio.charset.Charset
import java.nio.file.Path
import js7.base.configutils.Configs.*
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.http.configuration.{RecouplingStreamReaderConf, RecouplingStreamReaderConfs}
import scala.concurrent.ExecutionContext

final case class JobLauncherConf(
  executablesDirectory: Path,
  shellScriptTmpDirectory: Path,
  tmpDirectory: Path,
  /** Working directory of the process to be started. */
  workingDirectory: Path,
  systemEncoding: Charset,
  killWithSigterm: Seq[String],
  killWithSigkill: Seq[String],
  killForWindows: Seq[String],
  killScript: Option[ProcessKillScript],
  scriptInjectionAllowed: Boolean,
  errorLineLengthMax: Int,
  recouplingStreamReaderConf: RecouplingStreamReaderConf,
  iox: IOExecutor,
  blockingJobEC: ExecutionContext,
  clock: AlarmClock):

  implicit def implicitIox: IOExecutor = iox


object JobLauncherConf:

  def checked(
    executablesDirectory: Path,
    shellScriptTmpDirectory: Path,
    workTmpDirectory: Path,
    jobWorkingDirectory: Path,
    systemEncoding: Charset,
    killScript: Option[ProcessKillScript],
    scriptInjectionAllowed: Boolean = false,
    iox: IOExecutor, blockingJobEC: ExecutionContext, clock: AlarmClock, config: Config)
  : Checked[JobLauncherConf] =
    val sigtermName = "js7.job.execution.kill-with-sigterm-command"
    val sigkillName = "js7.job.execution.kill-with-sigkill-command"
    val sigkillWindowsName = "js7.job.execution.kill-command-for-windows"
    val killWithSigterm = config.seqAs[String](sigtermName)
    val killWithSigkill = config.seqAs[String](sigkillName)
    val killForWindows = config.seqAs[String](sigkillWindowsName)
    if killWithSigterm.nonEmpty && !killWithSigterm.contains("$pid") then
      Left(Problem(s"Setting $sigtermName must contain \"$$pid\""))
    else if killWithSigkill.nonEmpty && !killWithSigkill.contains("$pid") then
      Left(Problem(s"Setting $sigkillName must contain \"$$pid\""))
    else if killForWindows.nonEmpty && !killForWindows.contains("$pid") then
      Left(Problem(s"Setting $sigkillWindowsName must contain \"$$pid\""))
    else Right(
      JobLauncherConf(
        executablesDirectory = executablesDirectory,
        shellScriptTmpDirectory = shellScriptTmpDirectory,
        tmpDirectory = workTmpDirectory,
        workingDirectory = jobWorkingDirectory,
        systemEncoding = config.optionAs[String]("js7.job.execution.encoding")
          .map(Charset.forName /*throws*/)
          .getOrElse(systemEncoding),
        killWithSigterm = config.seqAs[String](sigtermName),
        killWithSigkill = config.seqAs[String](sigkillName),
        killForWindows = config.seqAs[String](sigkillWindowsName),
        killScript = killScript,
        scriptInjectionAllowed = scriptInjectionAllowed,
        errorLineLengthMax = config.getInt("js7.job.execution.used-error-line-length"),
        RecouplingStreamReaderConfs.fromConfig(config).orThrow,
        iox,
        blockingJobEC = blockingJobEC,
        clock))
