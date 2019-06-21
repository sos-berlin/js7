package com.sos.jobscheduler.agent.configuration

import akka.util.Timeout
import com.sos.jobscheduler.agent.configuration.AgentConfiguration._
import com.sos.jobscheduler.agent.data.{KillScriptConf, ProcessKillScript}
import com.sos.jobscheduler.base.convert.AsJava.asAbsolutePath
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerPort
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.process.Processes.ShellFileExtension
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.{EmptyPath, WorkingDirectory}
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.common.utils.Tests.isTest
import com.sos.jobscheduler.core.configuration.CommonConfiguration
import com.sos.jobscheduler.core.event.journal.JournalConf
import com.sos.jobscheduler.taskserver.task.process.ProcessKillScriptProvider
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.{Path, Paths}
import org.scalactic.Requirements._
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

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
      logDirectory = a.optionAs("-log-directory=")(asAbsolutePath) getOrElse logDirectory,
      jobJavaOptions = a.optionAs[String]("-job-java-options=").fold(jobJavaOptions) { _ :: Nil },
      jobWorkingDirectory = a.as("-job-working-directory=", jobWorkingDirectory)(asAbsolutePath))
    .withKillScript(a.optionAs[String]("-kill-script="))
  }

  private def withKillScript(killScriptPath: Option[String]) = killScriptPath match {
    case None => this  // -kill-script= not given: Agent uses the internally provided kill script
    case Some("") => copy(killScript = None)      // -kill-script= (empty argument) means: don't use any kill script
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
      assert(temporaryDirectory == dataDirectory / "tmp")
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
    killScript map (o => KillScriptConf(o, temporaryDirectory / s"kill_tasks_after_crash$ShellFileExtension"))

  lazy val temporaryDirectory: Path =
    dataDirectory  / "tmp"

  lazy val scriptInjectionAllowed = config.getBoolean("jobscheduler.agent.task.signed-script-injection-allowed")
}

object AgentConfiguration {
  val FileEncoding = if (isWindows) ISO_8859_1 else UTF_8
  private[configuration] val DefaultName = if (isTest) "Agent" else "JobScheduler"
  private val DelayUntilFinishKillScript = ProcessKillScript(EmptyPath)  // Marker for finish
  lazy val DefaultConfig = Configs.loadResource(
    JavaResource("com/sos/jobscheduler/agent/configuration/agent.conf"),
    internal = true)

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
      logDirectory = config.optionAs("jobscheduler.agent.log.directory")(asAbsolutePath) getOrElse defaultLogDirectory(dataDirectory),
      jobJavaOptions = config.stringSeq("jobscheduler.agent.task.java.options"),
      killScript = Some(DelayUntilFinishKillScript),  // Changed later
      akkaAskTimeout = config.getDuration("jobscheduler.akka.ask-timeout").toFiniteDuration,
      journalConf = JournalConf.fromConfig(config),
      name = DefaultName,
      config = config)
    v = v.withKillScript(config.optionAs[String]("jobscheduler.agent.task.kill.script"))
    //for (o <- config.optionAs("jobscheduler.https.port")(StringToServerInetSocketAddress)) {
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
        "jobscheduler.config-directory" -> configDirectory.toString
      ).asJava)
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf", secret = true))
      .withFallback(parseConfigIfExists(configDirectory / "agent.conf", secret = false))

  private def defaultLogDirectory(data: Path) = data / "logs"

  object forTest {
    private val TaskServerLog4jResource = JavaResource("com/sos/jobscheduler/taskserver/configuration/log4j2.xml")

    def apply(
      configAndData: Path,
      config: Config = ConfigFactory.empty,
      httpPort: Option[Int] = Some(findFreeTcpPort()),
      httpsPort: Option[Int] = None,
      mutualHttps: Boolean = false
    ) =
      fromDirectories(
        configDirectory = configAndData / "config",
        dataDirectory = configAndData / "data",
        config)
      .copy(
        webServerPorts  =
          httpPort.map(port => WebServerPort.Http(new InetSocketAddress("127.0.0.1", port))) ++:
          httpsPort.map(port => WebServerPort.Https(new InetSocketAddress("127.0.0.1", port), mutual = mutualHttps)).toList,
        jobJavaOptions =
          s"-Dlog4j.configurationFile=${TaskServerLog4jResource.path}" ::
            sys.props.get("agent.job.javaOptions").toList)
  }
}
