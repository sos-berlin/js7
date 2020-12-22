package js7.agent.configuration

import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.{Path, Paths}
import js7.agent.configuration.AgentConfiguration._
import js7.agent.data.{KillScriptConf, ProcessKillScript}
import js7.base.convert.AsJava.asAbsolutePath
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.Assertions.assertThat
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.commandline.CommandLineArguments
import js7.common.configuration.JobSchedulerConfiguration
import js7.common.configutils.Configs
import js7.common.configutils.Configs._
import js7.common.process.Processes.ShellFileExtension
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.{EmptyPath, WorkingDirectory}
import js7.common.time.JavaTimeConverters._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.common.utils.JavaResource
import js7.common.utils.Tests.isTest
import js7.core.configuration.CommonConfiguration
import js7.journal.configuration.JournalConf
import js7.taskserver.task.process.ProcessKillScriptProvider
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  configDirectory: Path,
  dataDirectory: Path,
  webServerPorts: Seq[WebServerPort],
  logDirectory: Path,
  jobWorkingDirectory: Path = WorkingDirectory,
  /** Unused. */jobJavaOptions: Seq[String],
  sigkillProcessesAfter: FiniteDuration,
  killScript: Option[ProcessKillScript],
  implicit val akkaAskTimeout: Timeout,
  journalConf: JournalConf,
  name: String,
  config: Config)  // Should not be the first argument to avoid the misleading call AgentConfiguration(config)
extends CommonConfiguration
{
  require(jobWorkingDirectory.isAbsolute)

  private def withCommandLineArguments(a: CommandLineArguments): AgentConfiguration = {
    val common = CommonConfiguration.Common.fromCommandLineArguments(a)
    copy(
      webServerPorts = common.webServerPorts,
      logDirectory = a.optionAs("--log-directory=")(asAbsolutePath) getOrElse logDirectory,
      jobJavaOptions = a.optionAs[String]("--job-java-options=").fold(jobJavaOptions)(_ :: Nil),
      jobWorkingDirectory = a.as("--job-working-directory=", jobWorkingDirectory)(asAbsolutePath))
    .withKillScript(a.optionAs[String]("--kill-script="))
  }

  private def withKillScript(killScriptPath: Option[String]) = killScriptPath match {
    case None => this  // --kill-script= not given: Agent uses the internally provided kill script
    case Some("") => copy(killScript = None)      // --kill-script= (empty argument) means: don't use any kill script
    case Some(o) => copy(killScript = Some(ProcessKillScript(Paths.get(o).toAbsolutePath)))
  }

  def executableDirectory: Path =
    (configDirectory / "executables").toRealPath()

  def stateDirectory: Path =
    dataDirectory / "state"

  def finishAndProvideFiles: AgentConfiguration =
    provideDataSubdirectories()
      .provideKillScript()

  private def provideDataSubdirectories(): AgentConfiguration = {
    if (logDirectory == defaultLogDirectory(dataDirectory) && !exists(logDirectory)) {
      createDirectory(logDirectory)
    }
    if (!exists(stateDirectory)) {
      createDirectory(stateDirectory)
    }
    if (!exists(temporaryDirectory)) {
      assertThat(temporaryDirectory == dataDirectory / "tmp")
      createDirectory(temporaryDirectory)
    }
    this
  }

  private def provideKillScript(): AgentConfiguration = {
    killScript match {
      case Some(DelayUntilFinishKillScript) =>
        val provider = new ProcessKillScriptProvider  //.closeWithCloser  After Agent Server termination, leave behind the kill script, in case of regular termination after error.
        copy(killScript = Some(provider.provideTo(temporaryDirectory)))
      case _ => this
    }
  }

  def killScriptConf: Option[KillScriptConf] =
    killScript.map(o => KillScriptConf(o, temporaryDirectory / s"kill_tasks_after_crash$ShellFileExtension"))

  lazy val temporaryDirectory: Path =
    dataDirectory  / "tmp"

  lazy val scriptInjectionAllowed = config.getBoolean("js7.job.execution.signed-script-injection-allowed")

  // Suppresses Config (which may contain secrets)
  override def toString = s"AgentConfiguration($configDirectory,$dataDirectory,$webServerPorts," +
    s"$logDirectory,$jobWorkingDirectory,$jobJavaOptions,$killScript,$akkaAskTimeout,$journalConf,$name,Config)"
}

object AgentConfiguration
{
  val FileEncoding = if (isWindows) ISO_8859_1 else UTF_8
  private[configuration] val DefaultName = if (isTest) "Agent" else "JS7"
  private val DelayUntilFinishKillScript = ProcessKillScript(EmptyPath)  // Marker for finish
  lazy val DefaultConfig = Configs.loadResource(
    JavaResource("js7/agent/configuration/agent.conf"),
    internal = true
  ).withFallback(JobSchedulerConfiguration.defaultConfig)

  def fromCommandLine(arguments: CommandLineArguments, extraDefaultConfig: Config = ConfigFactory.empty) = {
    val common = CommonConfiguration.Common.fromCommandLineArguments(arguments)
    val c = fromDirectories(
      configDirectory = common.configDirectory,
      dataDirectory = common.dataDirectory,
      extraDefaultConfig)
    c.copy(webServerPorts = common.webServerPorts ++ c.webServerPorts)
    .withCommandLineArguments(arguments)
  }

  private def fromDirectories(configDirectory: Path, dataDirectory: Path, extraDefaultConfig: Config): AgentConfiguration = {
    val config = resolvedConfig(configDirectory, extraDefaultConfig)
    var v = new AgentConfiguration(
      configDirectory = configDirectory,
      dataDirectory = dataDirectory,
      webServerPorts = Nil,
      logDirectory = config.optionAs("js7.job.execution.log.directory")(asAbsolutePath) getOrElse defaultLogDirectory(dataDirectory),
      jobJavaOptions = config.stringSeq("js7.job.execution.java.options"),
      sigkillProcessesAfter = config.getDuration("js7.job.execution.sigkill-after").toFiniteDuration,
      killScript = Some(DelayUntilFinishKillScript),  // Changed later
      akkaAskTimeout = config.getDuration("js7.akka.ask-timeout").toFiniteDuration,
      journalConf = JournalConf.fromConfig(config),
      name = DefaultName,
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

  object forTest {
    private val TaskServerLog4jResource = JavaResource("js7/taskserver/configuration/log4j2.xml")

    def apply(
      configAndData: Path,
      config: Config = ConfigFactory.empty,
      httpPort: Option[Int] = Some(findFreeTcpPort()),
      httpsPort: Option[Int] = None) =
      fromDirectories(
        configDirectory = configAndData / "config",
        dataDirectory = configAndData / "data",
        config)
      .copy(
        webServerPorts  =
          httpPort.map(port => WebServerPort.localhost(port)) ++:
          httpsPort.map(port => WebServerPort.Https(new InetSocketAddress("127.0.0.1", port))).toList,
        jobJavaOptions =
          s"-Dlog4j.configurationFile=${TaskServerLog4jResource.path}" ::
            sys.props.get("agent.job.javaOptions").toList)
  }
}
