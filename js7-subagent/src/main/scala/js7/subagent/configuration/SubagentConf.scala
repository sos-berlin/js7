package js7.subagent.configuration

import com.typesafe.config.Config
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.Path
import js7.base.configutils.Configs
import js7.base.configutils.Configs.RichConfig
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.{EmptyPath, WorkingDirectory}
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.configuration.{CommonConfiguration, Js7Configuration}
import js7.common.http.configuration.RecouplingStreamReaderConfs
import js7.launcher.configuration.{JobLauncherConf, ProcessKillScript}
import js7.launcher.process.ProcessKillScriptProvider
import js7.subagent.client.SubagentDriver.StdouterrConf
import js7.subagent.configuration.SubagentConf._
import monix.execution.schedulers.SchedulerService
import scala.concurrent.duration.FiniteDuration

final case class SubagentConf(
  configDirectory: Path,
  dataDirectory: Path,
  logDirectory: Path,
  jobWorkingDirectory: Path = WorkingDirectory,
  webServerPorts: Seq[WebServerPort],
  defaultJobSigkillDelay: FiniteDuration,
  killScript: Option[ProcessKillScript],
  stdouterr: StdouterrConf,
  outerrCharBufferSize: Int,
  stdoutCommitDelay: FiniteDuration,
  name: String,
  config: Config)
extends CommonConfiguration
{
  require(jobWorkingDirectory.isAbsolute)

  lazy val scriptInjectionAllowed = config.getBoolean("js7.job.execution.signed-script-injection-allowed")

  def executablesDirectory: Path =
    configDirectory / "executables"

  lazy val stateDirectory: Path =
    dataDirectory / "state"

  lazy val workDirectory: Path =
    dataDirectory / "work"

  private lazy val shellScriptTmpDirectory: Path =
    workDirectory / "scripts"

  private lazy val workTmpDirectory: Path =
    workDirectory / "tmp"

  /** Directory for temporary value files. */
  lazy val valueDirectory: Path =
    workDirectory / "values"

  def finishAndProvideFiles: SubagentConf =
    provideDataSubdirectories()
      .provideKillScript()

  private def provideDataSubdirectories(): SubagentConf = {
    if (logDirectory == defaultLogDirectory(dataDirectory) && !exists(logDirectory)) {
      createDirectory(logDirectory)
    }
    autoCreateDirectory(stateDirectory)
    autoCreateDirectory(workDirectory)
    autoCreateDirectory(shellScriptTmpDirectory)
    autoCreateDirectory(workTmpDirectory)
    autoCreateDirectory(valueDirectory)
    this
  }

  // TODO Duplicate
  def toJobLauncherConf(iox: IOExecutor, blockingJobScheduler: SchedulerService, clock: AlarmClock)
  : Checked[JobLauncherConf] = {
    val sigtermName = "js7.job.execution.kill-with-sigterm-command"
    val sigkillName = "js7.job.execution.kill-with-sigkill-command"
    val sigkillWindowsName = "js7.job.execution.kill-command-for-windows"
    val killWithSigterm = config.seqAs[String](sigtermName)
    val killWithSigkill = config.seqAs[String](sigkillName)
    val killForWindows = config.seqAs[String](sigkillWindowsName)
    if (killWithSigterm.nonEmpty && !killWithSigterm.contains("$pid"))
      Left(Problem(s"Setting $sigtermName must contain \"$$pid\""))
    else if (killWithSigkill.nonEmpty && !killWithSigkill.contains("$pid"))
      Left(Problem(s"Setting $sigkillName must contain \"$$pid\""))
    else if (killForWindows.nonEmpty && !killForWindows.contains("$pid"))
      Left(Problem(s"Setting $sigkillWindowsName must contain \"$$pid\""))
    else Right(
      JobLauncherConf(
        executablesDirectory = executablesDirectory,
        shellScriptTmpDirectory = shellScriptTmpDirectory,
        tmpDirectory = workTmpDirectory,
        workingDirectory = jobWorkingDirectory,
        killWithSigterm = config.seqAs[String](sigtermName),
        killWithSigkill = config.seqAs[String](sigkillName),
        killForWindows = config.seqAs[String](sigkillWindowsName),
        killScript = killScript,
        scriptInjectionAllowed = scriptInjectionAllowed,
        RecouplingStreamReaderConfs.fromConfig(config).orThrow,
        iox,
        blockingJobScheduler = blockingJobScheduler,
        clock))
  }

  private def provideKillScript(): SubagentConf =
    killScript match {
      case Some(DelayUntilFinishKillScript) =>
        // After Subagent termination, leave behind the kill script,
        // in case of regular termination after error.
        val provider = new ProcessKillScriptProvider  //.closeWithCloser
        copy(killScript = Some(provider.provideTo(workDirectory)))
      case _ => this
    }
}

object SubagentConf
{
  private def defaultLogDirectory(data: Path) = data / "logs"
  private val DelayUntilFinishKillScript = ProcessKillScript(EmptyPath)  // Marker for finish

  val defaultConfig = Configs
    .loadResource(JavaResource("js7/subagent/configuration/subagent.conf"))
    .withFallback(Js7Configuration.defaultConfig)

  def of(
    configDirectory: Path,
    dataDirectory: Path,
    logDirectory: Path,
    jobWorkingDirectory: Path,
    webServerPorts: Seq[WebServerPort],
    killScript: Option[ProcessKillScript],
    config: Config,
    name: String = "JS7")
  : SubagentConf = {
    val myConfig = config.withFallback(SubagentConf.defaultConfig)
    val outErrConf = StdouterrConf.fromConfig(config)
    SubagentConf(
      configDirectory = configDirectory,
      dataDirectory = dataDirectory,
      logDirectory = logDirectory,
      jobWorkingDirectory = jobWorkingDirectory,
      webServerPorts,
      defaultJobSigkillDelay = myConfig.finiteDuration("js7.job.execution.sigkill-delay").orThrow,
      killScript,
      outErrConf,
      outerrCharBufferSize = config.memorySizeAsInt("js7.order.stdout-stderr.char-buffer-size").orThrow
        .min(outErrConf.chunkSize),
      stdoutCommitDelay = config.finiteDuration("js7.order.stdout-stderr.commit-delay").orThrow,
      name = name,
      myConfig.withFallback(SubagentConf.defaultConfig))
  }

  private def autoCreateDirectory(directory: Path): Path = {
    if (!exists(directory)) createDirectory(directory)
    directory
  }
}
