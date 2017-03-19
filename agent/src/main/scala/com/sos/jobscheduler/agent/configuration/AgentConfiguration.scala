package com.sos.jobscheduler.agent.configuration

import akka.util.Timeout
import com.sos.jobscheduler.agent.configuration.AgentConfiguration._
import com.sos.jobscheduler.agent.data.ProcessKillScript
import com.sos.jobscheduler.agent.web.common.ExternalWebService
import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.convert.As.asAbsolutePath
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.common.commandline.CommandLineArguments
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.configutils.Configs._
import com.sos.jobscheduler.common.internet.IP._
import com.sos.jobscheduler.common.process.Processes.ShellFileExtension
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.{EmptyPath, WorkingDirectory}
import com.sos.jobscheduler.common.sprayutils.WebServerBinding
import com.sos.jobscheduler.common.sprayutils.https.KeystoreReference
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.taskserver.data.DotnetConfiguration
import com.sos.jobscheduler.taskserver.task.process.ProcessKillScriptProvider
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.{Path, Paths}
import java.time.Duration
import org.scalactic.Requirements._
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.reflect.ClassTag
import spray.http.Uri

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  dataDirectory: Option[Path],
  configDirectory: Option[Path],
  http: Option[WebServerBinding.Http],
  https: Option[WebServerBinding.Https],
  uriPathPrefix: String,
  externalWebServiceClasses: immutable.Seq[Class[_ <: ExternalWebService]],
  workingDirectory: Path = WorkingDirectory,
  logDirectory: Path,
  environment: Map[String, String],
  jobJavaOptions: immutable.Seq[String],
  dotnet: DotnetConfiguration,
  rpcKeepaliveDuration: Option[Duration],
  killScript: Option[ProcessKillScript],
  startupTimeout: Duration,
  commandTimeout: Duration,
  implicit val akkaAskTimeout: Timeout,
  name: String,
  journalSyncOnCommit: Boolean,
  config: Config)  // Should not be the first argument to avoid the misleading call AgentConfiguration(config)
{
  require(!(uriPathPrefix.startsWith("/") || uriPathPrefix.endsWith("/")))
  require(workingDirectory.isAbsolute)

  private def withCommandLineArguments(a: CommandLineArguments): AgentConfiguration = {
    var v = copy(
      http = a.optionAs("-http-port=", http)(As(o ⇒ WebServerBinding.Http(StringToServerInetSocketAddress(o)))),
      https = a.optionAs("-https-port=")(StringToServerInetSocketAddress) map { o ⇒ inetSocketAddressToHttps(o) } orElse https,
      uriPathPrefix = a.as[String]("-uri-prefix=", uriPathPrefix) stripPrefix "/" stripSuffix "/",
      logDirectory = a.optionAs("-log-directory=")(asAbsolutePath) getOrElse logDirectory,
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

  def withHttpsInetSocketAddress(addr: InetSocketAddress) = copy(https = Some(inetSocketAddressToHttps(addr)))

  private def inetSocketAddressToHttps(addr: InetSocketAddress) = WebServerBinding.Https(
    addr,
    https match {
      case Some(o) ⇒ o.keystoreReference
      case None ⇒ newKeyStoreReference()
    })

  private def newKeyStoreReference() =
    KeystoreReference.fromSubConfig(
      config.getConfig("jobscheduler.agent.webserver.https.keystore"),
      configDirectory = configDirectory getOrElse {
        throw new IllegalArgumentException("For HTTPS, dataDirectory is required")
      })

  def withWebService[A <: ExternalWebService : ClassTag] = withWebServices(List(implicitClass[A]))

  def withWebServices(classes: Iterable[Class[_ <: ExternalWebService]]) =
    copy(externalWebServiceClasses = externalWebServiceClasses ++ classes)

  def withDotnetAdapterDirectory(directory: Option[Path]) = copy(dotnet = dotnet.copy(adapterDllDirectory = directory))

  def liveDirectoryOption: Option[Path] = configDirectory map { _ / "live" }

  def stateDirectoryOption: Option[Path] = dataDirectory map { _ / "state" }

  private[configuration] def finishAndProvideFiles: AgentConfiguration =
    provideDataSubdirectories()
      .provideKillScript()

  private def provideDataSubdirectories(): AgentConfiguration = {
    for (data ← dataDirectory) {
      if (logDirectory == defaultLogDirectory(data) && !exists(logDirectory)) {
        createDirectory(logDirectory)
      }
      if (!exists(temporaryDirectory)) {
        assert(temporaryDirectory == data / "tmp")
        createDirectory(temporaryDirectory)
      }
    }
    this
  }

  private def provideKillScript(): AgentConfiguration = {
    killScript match {
      case Some(ProcessKillScript(DelayUntilFinishFile)) ⇒
        val provider = new ProcessKillScriptProvider  //.closeWithCloser  After Agent termination, leave behind the kill script, in case of regular termination after error.
        copy(killScript = Some(provider.provideTo(temporaryDirectory)))
      case _ ⇒ this
    }
  }

  def crashKillScriptEnabled = dataDirectory.isDefined   // Suppressed when using a standard temporary directory to allow concurrent runs (and tests)

  def crashKillScriptFile: Path = temporaryDirectory / s"kill_tasks_after_crash$ShellFileExtension"

  lazy val temporaryDirectory: Path =
    dataDirectory match {
      case Some(data) ⇒ data / "tmp"
      case None ⇒ logDirectory  // Usage of logDirectory is compatible with v1.10.4
  }

  def localUri: Uri =
    http map { o ⇒ Uri(s"http://${o.address.getAddress.getHostAddress}:${o.address.getPort}") } getOrElse {
      throw sys.error("No HTTP binding for localUri")
    }
}

object AgentConfiguration {
  val InvalidAuthenticationDelay = 1.s
  private val DelayUntilFinishFile = EmptyPath  // Marker for finish
  private[configuration] lazy val DefaultsConfig = Configs.loadResource(
    JavaResource("com/sos/jobscheduler/agent/configuration/agent.conf"))

  def apply(args: Seq[String]) = CommandLineArguments.parse(args) { a ⇒
    fromDataDirectory(
      dataDirectory = a.optionAs[Path]("-data-directory="),
      configDirectory = a.optionAs[Path]("-config-directory=")
    ) withCommandLineArguments a
  }

  def fromDataDirectory(dataDirectory: Option[Path], configDirectory: Option[Path], extraDefaultConfig: Config = ConfigFactory.empty): AgentConfiguration = {
    val dataDir = dataDirectory map { _.toAbsolutePath }
    val configDir = configDirectory orElse (dataDir map { _ / "config" })
    val config = resolvedConfig(configDir, extraDefaultConfig)
    val c = config.getConfig("jobscheduler.agent")
    var v = new AgentConfiguration(
      dataDirectory = dataDir,
      configDirectory = configDir,
      http = c.optionAs("webserver.http.port")(StringToServerInetSocketAddress) map WebServerBinding.Http,
      https = None,  // Changed below
      externalWebServiceClasses = Nil,
      uriPathPrefix = c.as[String]("webserver.uri-prefix") stripPrefix "/" stripSuffix "/",
      logDirectory = c.optionAs("log.directory")(asAbsolutePath) orElse (dataDir map defaultLogDirectory) getOrElse temporaryDirectory,
      environment = Map(),
      dotnet = DotnetConfiguration(classDllDirectory = c.optionAs("task.dotnet.class-directory")(asAbsolutePath)),
      rpcKeepaliveDuration = c.durationOption("task.rpc.keepalive.duration"),
      jobJavaOptions = c.stringSeq("task.java.options"),
      killScript = Some(ProcessKillScript(DelayUntilFinishFile)),  // Changed below
      startupTimeout = c.getDuration("startup-timeout"),
      commandTimeout = c.getDuration("command-timeout"),
      akkaAskTimeout = c.getDuration("akka-ask-timeout").toFiniteDuration,
      name = "Agent",
      journalSyncOnCommit = c.getBoolean("journal.sync"),
      config = config)
    v = v.withKillScript(c.optionAs[String]("task.kill.script"))
    for (o ← c.optionAs("webserver.https.port")(StringToServerInetSocketAddress)) {
      v = v withHttpsInetSocketAddress o
    }
    v
  }

  private def resolvedConfig(configDirectory: Option[Path], extraDefaultConfig: Config): Config = {
    val config = configDirectory map configDirectoryConfig getOrElse ConfigFactory.empty
    (config withFallback extraDefaultConfig withFallback DefaultsConfig).resolve
  }

  private def configDirectoryConfig(configDirectory: Path): Config =
    ConfigFactory
      .empty  //.parseMap(Map("jobscheduler.agent.data.directory" → dataDirectory.toString))  // For substitution of ${jobscheduler.agent.data.directory}
      .withFallback(parseConfigIfExists(configDirectory / "private/private.conf"))
      .withFallback(parseConfigIfExists(configDirectory / "agent.conf"))

  private def defaultLogDirectory(data: Path) = data / "logs"

  object forTest {
    private val TaskServerLogbackResource = JavaResource("com/sos/jobscheduler/taskserver/configuration/logback.xml")

    def apply(data: Option[Path] = None, httpPort: Int = findRandomFreeTcpPort(), config: Config = ConfigFactory.empty) =
      fromDataDirectory(data, None, config).copy(
        http = Some(WebServerBinding.Http(new InetSocketAddress("127.0.0.1", httpPort))),
        jobJavaOptions = List(s"-Dlogback.configurationFile=${TaskServerLogbackResource.path}") ++ sys.props.get("agent.job.javaOptions"))
  }
}
