package com.sos.scheduler.engine.agent.configuration

import com.sos.scheduler.engine.agent.configuration.AgentConfiguration._
import com.sos.scheduler.engine.agent.data.ProcessKillScript
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.commandline.CommandLineArguments
import com.sos.scheduler.engine.common.configutils.Configs
import com.sos.scheduler.engine.common.configutils.Configs._
import com.sos.scheduler.engine.common.convert.As.asAbsolutePath
import com.sos.scheduler.engine.common.internet.IP._
import com.sos.scheduler.engine.common.process.Processes.ShellFileExtension
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.FileUtils.{EmptyPath, WorkingDirectory}
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.implicitClass
import com.sos.scheduler.engine.common.sprayutils.https.KeystoreReference
import com.sos.scheduler.engine.common.system.FileUtils.temporaryDirectory
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.taskserver.data.DotnetConfiguration
import com.sos.scheduler.engine.taskserver.task.process.ProcessKillScriptProvider
import com.typesafe.config.{Config, ConfigFactory}
import java.net.InetSocketAddress
import java.nio.file.Files.{createDirectory, exists}
import java.nio.file.{Path, Paths}
import java.time.Duration
import org.scalactic.Requirements._
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.reflect.ClassTag

/**
 * @author Joacim Zschimmer
 */
final case class AgentConfiguration(
  dataDirectory: Option[Path],
  httpAddress: Option[InetSocketAddress],
  https: Option[Https],
  uriPathPrefix: String,
  externalWebServiceClasses: immutable.Seq[Class[_ <: ExternalWebService]],
  workingDirectory: Path = WorkingDirectory,
  logDirectory: Path,
  environment: Map[String, String],
  jobJavaOptions: immutable.Seq[String],
  dotnet: DotnetConfiguration,
  rpcKeepaliveDuration: Option[Duration],
  killScript: Option[ProcessKillScript],
  config: Config)  // Should not be the first argument to avoid the misleading call AgentConfiguration(config)
{
  require(!(uriPathPrefix.startsWith("/") || uriPathPrefix.endsWith("/")))
  require(workingDirectory.isAbsolute)

  private def withCommandLineArguments(a: CommandLineArguments): AgentConfiguration = {
    var v = copy(
      httpAddress = a.optionAs("-http-port=", httpAddress)(StringToServerInetSocketAddress),
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

  private def inetSocketAddressToHttps(addr: InetSocketAddress) = Https(
    addr,
    https map { _.keystoreReference } getOrElse {
      val sub = config.getConfig("jobscheduler.agent.webserver.https.keystore")
      KeystoreReference(
        url = sub.as[Path]("file", privateDirectory / "private-https.jks").toURI.toURL ,
        storePassword = sub.optionAs[SecretString]("password"),
        keyPassword = Some(sub.as[SecretString]("key-password")))
    })

  def withWebService[A <: ExternalWebService : ClassTag] = withWebServices(List(implicitClass[A]))

  def withWebServices(classes: Iterable[Class[_ <: ExternalWebService]]) =
    copy(externalWebServiceClasses = externalWebServiceClasses ++ classes)

  def withDotnetAdapterDirectory(directory: Option[Path]) = copy(dotnet = dotnet.copy(adapterDllDirectory = directory))

  private def privateDirectory = dataDirectory map { _ / "config/private" } getOrElse {
    throw new IllegalArgumentException("Missing dataDirectory")
  }

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

  def crashKillScriptFile: Path = temporaryDirectory / s"kill_tasks_after_crash$ShellFileExtension"

  lazy val temporaryDirectory: Path =
    dataDirectory match {
      case Some(data) ⇒ data / "tmp"
      case None ⇒ logDirectory  // Usage of logDirectory is compatible with v1.10.4
  }

  lazy val authUsersConfig: Config = config.getConfig("jobscheduler.agent.auth.users")
}

object AgentConfiguration {
  val InvalidAuthenticationDelay = 1.s
  private val DelayUntilFinishFile = EmptyPath  // Marker for finish
  private[configuration] lazy val DefaultsConfig = Configs.loadResource(
    JavaResource("com/sos/scheduler/engine/agent/configuration/agent.conf"))

  def apply(args: Seq[String]) = CommandLineArguments.parse(args) { a ⇒
    fromDataDirectory(a.optionAs[Path]("-data-directory=")) withCommandLineArguments a
  }

  def fromDataDirectory(dataDirectory: Option[Path], extraDefaultConfig: Config = ConfigFactory.empty): AgentConfiguration = {
    val data = dataDirectory map { _.toAbsolutePath }
    val config = resolvedConfig(data, extraDefaultConfig)
    val c = config.getConfig("jobscheduler.agent")
    var v = new AgentConfiguration(
      dataDirectory = data,
      httpAddress = c.optionAs("webserver.http.port")(StringToServerInetSocketAddress),
      https = None,  // Changed below
      externalWebServiceClasses = Nil,
      uriPathPrefix = c.as[String]("webserver.uri-prefix") stripPrefix "/" stripSuffix "/",
      logDirectory = c.optionAs("log.directory")(asAbsolutePath) orElse (data map defaultLogDirectory) getOrElse temporaryDirectory,
      environment = Map(),
      dotnet = DotnetConfiguration(classDllDirectory = c.optionAs("task.dotnet.class-directory")(asAbsolutePath)),
      rpcKeepaliveDuration = c.durationOption("task.rpc.keepalive.duration"),
      jobJavaOptions = c.stringSeq("task.java.options"),
      killScript = Some(ProcessKillScript(DelayUntilFinishFile)),  // Changed below
      config = config)
    v = v.withKillScript(c.optionAs[String]("task.kill.script"))
    for (o ← c.optionAs("webserver.https.port")(StringToServerInetSocketAddress)) {
      v = v withHttpsInetSocketAddress o
    }
    v
  }

  private def resolvedConfig(dataDirectory: Option[Path], extraDefaultConfig: Config): Config = {
    val dataConfig = dataDirectory map dataDirectoryConfig getOrElse ConfigFactory.empty
    (dataConfig withFallback extraDefaultConfig withFallback DefaultsConfig).resolve
  }

  private def dataDirectoryConfig(dataDirectory: Path): Config =
    ConfigFactory
      .parseMap(Map("jobscheduler.agent.data.directory" → dataDirectory.toString))  // For substitution of ${jobscheduler.agent.data.directory}
      .withFallback(parseConfigIfExists(dataDirectory / "config/private/private.conf"))
      .withFallback(parseConfigIfExists(dataDirectory / "config/agent.conf"))

  final case class Https(address: InetSocketAddress, keystoreReference: KeystoreReference)

  private def defaultLogDirectory(data: Path) = data / "logs"

  object forTest {
    private val TaskServerLogbackResource = JavaResource("com/sos/scheduler/engine/taskserver/configuration/logback.xml")

    def apply(data: Option[Path] = None, httpPort: Int = findRandomFreeTcpPort(), config: Config = ConfigFactory.empty) =
      fromDataDirectory(data, config).copy(
        httpAddress = Some(new InetSocketAddress("127.0.0.1", httpPort)),
        jobJavaOptions = List(s"-Dlogback.configurationFile=${TaskServerLogbackResource.path}") ++ sys.props.get("agent.job.javaOptions"))
  }
}
