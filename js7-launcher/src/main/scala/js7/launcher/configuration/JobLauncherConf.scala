package js7.launcher.configuration

import com.typesafe.config.Config
import java.nio.charset.Charset
import java.nio.file.Path
import js7.base.configutils.Configs.*
import js7.base.problem.Checked
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
    Right:
      JobLauncherConf(
        executablesDirectory = executablesDirectory,
        shellScriptTmpDirectory = shellScriptTmpDirectory,
        tmpDirectory = workTmpDirectory,
        workingDirectory = jobWorkingDirectory,
        systemEncoding = config.optionAs[String]("js7.job.execution.encoding")
          .map(Charset.forName /*throws*/)
          .getOrElse(systemEncoding),
        killScript = killScript,
        scriptInjectionAllowed = scriptInjectionAllowed,
        errorLineLengthMax = config.getInt("js7.job.execution.used-error-line-length"),
        RecouplingStreamReaderConfs.fromConfig(config).orThrow,
        iox,
        blockingJobEC = blockingJobEC,
        clock)
