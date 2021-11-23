package js7.subagent.configuration

import akka.util.Timeout
import com.typesafe.config.Config
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.{Path, Paths}
import js7.base.configutils.Configs
import js7.base.configutils.Configs.RichConfig
import js7.base.convert.AsJava.asAbsolutePath
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.{EmptyPath, WorkingDirectory}
import js7.base.problem.{Checked, Problem}
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.{CommonConfiguration, Js7Configuration}
import js7.common.http.configuration.RecouplingStreamReaderConfs
import js7.launcher.configuration.{JobLauncherConf, ProcessKillScript}
import js7.launcher.process.ProcessKillScriptProvider
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
  akkaAskTimeout: Timeout,
  name: String,
  config: Config)
extends CommonConfiguration
{
  require(jobWorkingDirectory.isAbsolute)

  lazy val scriptInjectionAllowed = config.getBoolean("js7.job.execution.signed-script-injection-allowed")

  private def withCommandLineArguments(a: CommandLineArguments): SubagentConf = {
    val common = CommonConfiguration.Common.fromCommandLineArguments(a)
    copy(
      webServerPorts = common.webServerPorts,
      logDirectory = a.optionAs("--log-directory=")(asAbsolutePath) getOrElse logDirectory,
      jobWorkingDirectory = a.as("--job-working-directory=", jobWorkingDirectory)(asAbsolutePath))
    .withKillScript(a.optionAs[String]("--kill-script="))
  }

  private def withKillScript(killScriptPath: Option[String]) = killScriptPath match {
    case None => this  // --kill-script= not given: Agent uses the internally provided kill script
    case Some("") => copy(killScript = None)      // --kill-script= (empty argument) means: don't use any kill script
    case Some(o) => copy(killScript = Some(ProcessKillScript(Paths.get(o).toAbsolutePath)))
  }

  def executablesDirectory: Path =
    configDirectory / "executables"

  lazy val stateDirectory: Path =
    dataDirectory / "state"

  lazy val workDirectory: Path =
    dataDirectory / "work"

  def finishAndProvideFiles: SubagentConf =
    provideDataSubdirectories()
      .provideKillScript()

  private def provideDataSubdirectories(): SubagentConf = {
    if (logDirectory == defaultLogDirectory(dataDirectory) && !exists(logDirectory)) {
      createDirectory(logDirectory)
    }
    if (!exists(stateDirectory)) {
      createDirectory(stateDirectory)
    }
    if (!exists(workDirectory)) {
      assertThat(workDirectory == dataDirectory / "work")
      createDirectory(workDirectory)
    }
    this
  }

  // TODO Duplicate
  def toJobLauncherConf(iox: IOExecutor, blockingJobScheduler: SchedulerService, clock: AlarmClock)
  : Checked[JobLauncherConf] = {
    val sigtermName = "js7.job.execution.kill-with-sigterm-command"
    val sigkillName = "js7.job.execution.kill-with-sigkill-command"
    val killWithSigterm = config.seqAs[String](sigtermName)
    val killWithSigkill = config.seqAs[String](sigkillName)
    if (killWithSigterm.nonEmpty && !killWithSigterm.contains("$pid"))
      Left(Problem(s"Setting $sigtermName must contain \"$$pid\""))
    else if (killWithSigkill.nonEmpty && !killWithSigkill.contains("$pid"))
      Left(Problem(s"Setting $sigkillName must contain \"$$pid\""))
    else Right(
      JobLauncherConf(
        executablesDirectory = executablesDirectory,
        workDirectory = workDirectory,
        workingDirectory = jobWorkingDirectory,
        killWithSigterm = config.seqAs[String](sigtermName),
        killWithSigkill = config.seqAs[String](sigkillName),
        killScript = killScript,
        scriptInjectionAllowed = scriptInjectionAllowed,
        RecouplingStreamReaderConfs.fromConfig(config).orThrow,
        iox,
        blockingJobScheduler = blockingJobScheduler,
        clock))
  }

  private def provideKillScript(): SubagentConf = {
    killScript match {
      case Some(DelayUntilFinishKillScript) =>
        // After Subagent termination, leave behind the kill script,
        // in case of regular termination after error.
        val provider = new ProcessKillScriptProvider  //.closeWithCloser
        copy(killScript = Some(provider.provideTo(workDirectory)))
      case _ => this
    }
  }
}

object SubagentConf
{
  private def defaultLogDirectory(data: Path) = data / "logs"
  private val DelayUntilFinishKillScript = ProcessKillScript(EmptyPath)  // Marker for finish

  def defaultConfig = Configs
    .loadResource(JavaResource("js7/subagent/configuration/subagent.conf"))
    .withFallback(Js7Configuration.defaultConfig)
}
