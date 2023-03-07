package js7.agent.configuration

import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.{Path, Paths}
import js7.agent.configuration.AgentConfiguration.*
import js7.agent.data.AgentState
import js7.base.configutils.Configs
import js7.base.configutils.Configs.*
import js7.base.convert.AsJava.asAbsolutePath
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{EmptyPath, WorkingDirectory}
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.time.JavaTimeConverters.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.Tests.isTest
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.CommonConfiguration
import js7.common.http.configuration.RecouplingStreamReaderConfs
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.journal.configuration.JournalConf
import js7.journal.data.JournalMeta
import js7.launcher.configuration.{JobLauncherConf, ProcessKillScript}
import js7.subagent.configuration.{DirectorConf, SubagentConf}
import monix.execution.Scheduler
import scala.jdk.CollectionConverters.*

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  configDirectory: Path,
  dataDirectory: Path,
  webServerPorts: Seq[WebServerPort],
  logDirectory: Path,
  jobWorkingDirectory: Path = WorkingDirectory,
  killScript: Option[ProcessKillScript],  // TODO Duplicate with SubagentConf
  akkaAskTimeout: Timeout,
  journalConf: JournalConf,
  name: String,
  config: Config)  // Should not be the first argument to avoid the misleading call AgentConfiguration(config)
extends CommonConfiguration
{
  require(jobWorkingDirectory.isAbsolute)

  val recouplingStreamReaderConf = RecouplingStreamReaderConfs.fromConfig(config).orThrow

  implicit def implicitAkkaAskTimeout: Timeout = akkaAskTimeout

  private def withCommandLineArguments(a: CommandLineArguments): AgentConfiguration = {
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
    (configDirectory / "executables").toRealPath()

  def stateDirectory: Path =
    dataDirectory / "state"

  def createDirectories(): Unit = {
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
  }

  lazy val workDirectory: Path =
    dataDirectory  / "work"

  lazy val scriptInjectionAllowed =
    config.getBoolean("js7.job.execution.signed-script-injection-allowed")

  def toJobLauncherConf(iox: IOExecutor, blockingJobScheduler: Scheduler, clock: AlarmClock)
  : Checked[JobLauncherConf] =
    subagentConf.toJobLauncherConf(iox, blockingJobScheduler, clock)

  val journalMeta = JournalMeta(AgentState, stateDirectory / "agent" )

  lazy val subagentDirectorConf =
    DirectorConf(journalConf, httpsConfig, recouplingStreamReaderConf, subagentConf)

  lazy val subagentConf =
    SubagentConf.of(
      configDirectory = configDirectory,
      dataDirectory = dataDirectory,
      logDirectory = logDirectory,
      jobWorkingDirectory = jobWorkingDirectory,
      webServerPorts,
      killScript,
      config,
      name = name
    ).finishAndProvideFiles

  // Suppresses Config (which may contain secrets)
  override def toString = s"AgentConfiguration($configDirectory,$dataDirectory,$webServerPorts," +
    s"$logDirectory,$jobWorkingDirectory,$killScript,$akkaAskTimeout,$journalConf,$name,Config)"
}

object AgentConfiguration
{
  val DefaultName = if (isTest) "Agent" else "JS7"
  private val DelayUntilFinishKillScript = ProcessKillScript(EmptyPath)  // Marker for finish

  val DefaultConfig = Configs
    .loadResource(JavaResource("js7/agent/configuration/agent.conf"))
    .withFallback(SubagentConf.defaultConfig)

  def fromCommandLine(arguments: CommandLineArguments, extraDefaultConfig: Config = ConfigFactory.empty) = {
    val common = CommonConfiguration.Common.fromCommandLineArguments(arguments)
    val c = fromDirectories(
      configDirectory = common.configDirectory,
      dataDirectory = common.dataDirectory,
      extraDefaultConfig)
    c.copy(webServerPorts = common.webServerPorts ++ c.webServerPorts)
    .withCommandLineArguments(arguments)
  }

  private def fromDirectories(
    configDirectory: Path,
    dataDirectory: Path,
    extraDefaultConfig: Config,
    name: String = DefaultName)
  : AgentConfiguration = {
    val config = resolvedConfig(configDirectory, extraDefaultConfig)
    var v = new AgentConfiguration(
      configDirectory = configDirectory,
      dataDirectory = dataDirectory,
      webServerPorts = Nil,
      logDirectory = config.optionAs("js7.job.execution.log.directory")(asAbsolutePath) getOrElse defaultLogDirectory(dataDirectory),
      killScript = Some(DelayUntilFinishKillScript),  // Changed later
      akkaAskTimeout = config.getDuration("js7.akka.ask-timeout").toFiniteDuration,
      journalConf = JournalConf.fromConfig(config),
      name = name,
      config = config)
    v = v.withKillScript(config.optionAs[String]("js7.job.execution.kill.script"))
    //for (o <- config.optionAs("js7.web.server.https-port")(StringToServerInetSocketAddress)) {
    //  v = v addHttps o
    //}
    v
  }

  private def resolvedConfig(configDirectory: Path, extraDefaultConfig: Config): Config = {
    ConfigFactory.systemProperties
      .withFallback(configDirectoryConfig(configDirectory))
      .withFallback(extraDefaultConfig)
      .withFallback(DefaultConfig)
      .resolve
  }

  // Same code in TextAgentClient.configDirectoryConfig
  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory.parseMap(Map(
        "js7.config-directory" -> configDirectory.toString
      ).asJava)
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "agent.conf", secret = false))

  private def defaultLogDirectory(data: Path) = data / "logs"

  def forTest(
    configAndData: Path,
    name: String,
    config: Config = ConfigFactory.empty,
    httpPort: Option[Int] = Some(findFreeTcpPort()),
    httpsPort: Option[Int] = None)
  : AgentConfiguration =
    fromDirectories(
      configDirectory = configAndData / "config",
      dataDirectory = configAndData / "data",
      config,
      name = name)
    .copy(
      webServerPorts  =
        httpPort.map(port => WebServerPort.localhost(port)) ++:
        httpsPort.map(port => WebServerPort.Https(new InetSocketAddress("127.0.0.1", port))).toList)
}
