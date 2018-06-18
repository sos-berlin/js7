package com.sos.jobscheduler.agent.configuration

import akka.util.Timeout
import com.sos.jobscheduler.agent.configuration.AgentConfiguration._
import com.sos.jobscheduler.agent.data.{KillScriptConf, ProcessKillScript}
import com.sos.jobscheduler.base.convert.AsJava.{StringAsPath, asAbsolutePath}
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerBinding
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.internet.IP._
import com.sos.jobscheduler.common.process.Processes.ShellFileExtension
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.{EmptyPath, WorkingDirectory}
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.core.configuration.CommonConfiguration
import com.sos.jobscheduler.taskserver.data.DotnetConfiguration
import com.sos.jobscheduler.taskserver.task.process.ProcessKillScriptProvider
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.{Path, Paths}
import java.time.Duration
import org.scalactic.Requirements._
import scala.collection.immutable.Seq

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  configDirectory: Path,
  dataDirectory: Path,
  webServerPorts: Seq[(WebServerBinding.Scheme, InetSocketAddress)],
  workingDirectory: Path = WorkingDirectory,
  logDirectory: Path,
  environment: Map[String, String],
  jobJavaOptions: Seq[String],
  dotnet: DotnetConfiguration,
  rpcKeepaliveDuration: Option[Duration],
  killScript: Option[ProcessKillScript],
  commandTimeout: Duration,
  implicit val akkaAskTimeout: Timeout,
  name: String,
  journalSyncOnCommit: Boolean,
  config: Config)  // Should not be the first argument to avoid the misleading call AgentConfiguration(config)
extends CommonConfiguration
{
  require(workingDirectory.isAbsolute)

  private def withCommandLineArguments(a: CommandLineArguments): AgentConfiguration = {
    var v = copy(
      webServerPorts = webServerPorts ++
        a.seqAs("-http-port=")(StringToServerInetSocketAddress).map(WebServerBinding.Http → _) ++
        a.seqAs("-https-port=")(StringToServerInetSocketAddress).map(WebServerBinding.Https → _),
      logDirectory = a.optionAs("-log-directory=")(asAbsolutePath) getOrElse logDirectory,
      journalSyncOnCommit = a.boolean("-sync-journal", journalSyncOnCommit),
      jobJavaOptions = a.optionAs[String]("-job-java-options=") map { o ⇒ List(o) } getOrElse jobJavaOptions,
      rpcKeepaliveDuration = a.optionAs[Duration]("-rpc-keepalive=", rpcKeepaliveDuration))
    v = v withKillScript a.optionAs[String]("-kill-script=")
    for (o ← a.optionAs("-dotnet-class-directory=")(asAbsolutePath)) {
      v = v.copy(dotnet = DotnetConfiguration(classDllDirectory = Some(o)))
    }
    v
  }

  private def withKillScript(killScriptPath: Option[String]) = killScriptPath match {
    case None ⇒ this  // -kill-script= not given: Agent uses the internally provided kill script
    case Some("") ⇒ copy(killScript = None)      // -kill-script= (empty argument) means: don't use any kill script
    case Some(o) ⇒ copy(killScript = Some(ProcessKillScript(Paths.get(o).toAbsolutePath)))
  }

  def withDotnetAdapterDirectory(directory: Option[Path]) = copy(dotnet = dotnet.copy(adapterDllDirectory = directory))

  def fileBasedDirectory: Path =
    configDirectory / "live"

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
      case Some(DelayUntilFinishKillScript) ⇒
        val provider = new ProcessKillScriptProvider  //.closeWithCloser  After Agent termination, leave behind the kill script, in case of regular termination after error.
        copy(killScript = Some(provider.provideTo(temporaryDirectory)))
      case _ ⇒ this
    }
  }

  def killScriptConf: Option[KillScriptConf] =
    killScript map (o ⇒ KillScriptConf(o, temporaryDirectory / s"kill_tasks_after_crash$ShellFileExtension"))

  lazy val temporaryDirectory: Path =
    dataDirectory  / "tmp"
}

object AgentConfiguration {
  val FileEncoding = if (isWindows) ISO_8859_1 else UTF_8
  private val DelayUntilFinishKillScript = ProcessKillScript(EmptyPath)  // Marker for finish
  lazy val DefaultsConfig = Configs.loadResource(
    JavaResource("com/sos/jobscheduler/agent/configuration/agent.conf"))

  def fromCommandLine(args: Seq[String]) = CommandLineArguments.parse(args) { a ⇒
    fromDirectories(
      configDirectory = a.as[Path]("-config-directory="),
      dataDirectory = a.as[Path]("-data-directory=")
    ) withCommandLineArguments a
  }

  private def fromDirectories(configDirectory: Path, dataDirectory: Path, extraDefaultConfig: Config = ConfigFactory.empty): AgentConfiguration = {
    val configDir = configDirectory.toAbsolutePath
    val dataDir = dataDirectory.toAbsolutePath
    val config = resolvedConfig(configDirectory, extraDefaultConfig)
    var v = new AgentConfiguration(
      configDirectory = configDir,
      dataDirectory = dataDir,
      webServerPorts = Nil,
      logDirectory = config.optionAs("jobscheduler.agent.log.directory")(asAbsolutePath) getOrElse defaultLogDirectory(dataDir),
      environment = Map(),
      dotnet = DotnetConfiguration(classDllDirectory = config.optionAs("jobscheduler.agent.task.dotnet.class-directory")(asAbsolutePath)),
      rpcKeepaliveDuration = config.durationOption("jobscheduler.agent.task.rpc.keepalive.duration"),
      jobJavaOptions = config.stringSeq("jobscheduler.agent.task.java.options"),
      killScript = Some(DelayUntilFinishKillScript),  // Changed later
      commandTimeout = config.getDuration("jobscheduler.agent.command-timeout"),
      akkaAskTimeout = config.getDuration("jobscheduler.agent.akka-ask-timeout").toFiniteDuration,
      name = "Agent",
      journalSyncOnCommit = config.getBoolean("jobscheduler.journal.sync"),
      config = config)
    v = v.withKillScript(config.optionAs[String]("jobscheduler.agent.task.kill.script"))
    //for (o ← config.optionAs("jobscheduler.webserver.https.port")(StringToServerInetSocketAddress)) {
    //  v = v addHttps o
    //}
    v
  }

  private def resolvedConfig(configDirectory: Path, extraDefaultConfig: Config): Config = {
    val config = configDirectoryConfig(configDirectory)
    (config withFallback extraDefaultConfig withFallback DefaultsConfig).resolve
  }

  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty  //.parseMap(Map("jobscheduler.agent.data.directory" → dataDirectory.toString))  // For substitution of ${jobscheduler.agent.data.directory}
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf"))
      .withFallback(parseConfigIfExists(configDirectory / "agent.conf"))

  private def defaultLogDirectory(data: Path) = data / "logs"

  object forTest {
    private val TaskServerLog4jResource = JavaResource("com/sos/jobscheduler/taskserver/configuration/log4j2.xml")

    def apply(
      configAndData: Path,
      httpPort: Option[Int] = Some(findRandomFreeTcpPort()),
      httpsPort: Option[Int] = None,
      config: Config = ConfigFactory.empty
    ) =
      fromDirectories(
        configDirectory = configAndData / "config",
        dataDirectory = configAndData / "data",
        config)
      .copy(
        webServerPorts  =
          httpPort.map(WebServerBinding.Http → new InetSocketAddress("127.0.0.1", _)) ++:
          httpsPort.map(WebServerBinding.Https → new InetSocketAddress("127.0.0.1", _)).toList,
        jobJavaOptions =
          s"-Dlog4j.configurationFile=${TaskServerLog4jResource.path}" ::
            sys.props.get("agent.job.javaOptions").toList)
  }
}
